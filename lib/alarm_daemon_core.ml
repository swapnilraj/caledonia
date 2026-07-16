module Fire_key = struct
  type t = Alarm_fire_id.t

  let compare = Alarm_fire_id.compare
end

module Fired_set = Set.Make (Fire_key)
module Pending_map = Map.Make (Fire_key)

type state = {
  watermark : Ptime.t;
  fired : Fired_set.t;
  pending : int Pending_map.t;
}

type watcher_event = Timer | Changed | Overflow | Watcher_error of string

type driver = {
  now : unit -> Ptime.t;
  wait : timeout:float -> watcher_event;
  sleep : float -> unit;
  load_state : unit -> state option;
  save_state : state -> (unit, [ `Msg of string ]) result;
  load_fires :
    from:Ptime.t ->
    to_:Ptime.t ->
    (Alarm_query.fire list, [ `Msg of string ]) result;
  notify : Alarm_query.fire -> (unit, [ `Msg of string ]) result;
  report : string -> unit;
}

let max_delivery_attempts = 3

let empty_state ~watermark =
  { watermark; fired = Fired_set.empty; pending = Pending_map.empty }

let watermark state = state.watermark
let fired_count state = Fired_set.cardinal state.fired
let pending_count state = Pending_map.cardinal state.pending
let stable_digest value = Digest.string value |> Digest.to_hex

let recurrence_identity subject =
  match Component_query.get_recurrence_id_property subject with
  | Some recurrence_id ->
      "rid:"
      ^ stable_digest
          (Calendar_codec.canonical_recurrence_id_line recurrence_id)
  | None -> "master"

let fire_key (alarm_fire : Alarm_query.fire) =
  let alarm_identity =
    Calendar_codec.canonical_alarm_block alarm_fire.alarm |> stable_digest
  in
  let target = Component_query.get_target alarm_fire.owner in
  Alarm_fire_id.create ~target
    ~recurrence_key:(recurrence_identity alarm_fire.owner)
    ~alarm_index:alarm_fire.alarm_index ~alarm_key:alarm_identity
    ~fire_time:alarm_fire.fire_time

let distinct_fire_identity left right = fire_key left <> fire_key right

let key_to_json (key : Alarm_fire_id.t) =
  [
    `String key.calendar_key;
    `String key.source_file;
    `String key.uid;
    `String key.recurrence_key;
    `Int key.alarm_index;
    `String key.alarm_key;
    `String (Date.rfc3339_utc key.fire_time);
  ]

let state_to_json state =
  let fired =
    Fired_set.elements state.fired
    |> List.map (fun key -> `List (key_to_json key))
  in
  let pending =
    Pending_map.bindings state.pending
    |> List.map (fun (key, attempts) ->
        `List (key_to_json key @ [ `Int attempts ]))
  in
  `Assoc
    [
      ("schema_version", `Int 2);
      ("watermark", `String (Date.rfc3339_utc state.watermark));
      ("fired", `List fired);
      ("pending", `List pending);
    ]

let ptime_of_json = function
  | `String value -> (
      match Ptime.of_rfc3339 value with
      | Ok (timestamp, _, _) -> Some timestamp
      | Error _ -> None)
  | _ -> None

let key_of_json = function
  | [
      `String calendar;
      `String file;
      `String id;
      `String recurrence;
      `Int alarm_index;
      `String alarm;
      fire_time;
    ] ->
      Option.map
        (fun timestamp ->
          Alarm_fire_id.
            {
              calendar_key = calendar;
              source_file = file;
              uid = id;
              recurrence_key = recurrence;
              alarm_index;
              alarm_key = alarm;
              fire_time = timestamp;
            })
        (ptime_of_json fire_time)
  | _ -> None

let rec parse_fired fired = function
  | [] -> Some fired
  | `List entry :: rest -> (
      match key_of_json entry with
      | Some key -> parse_fired (Fired_set.add key fired) rest
      | None -> None)
  | _ :: _ -> None

let rec parse_pending pending = function
  | [] -> Some pending
  | `List entry :: rest -> (
      match List.rev entry with
      | `Int attempts :: reversed_key
        when attempts > 0 && attempts < max_delivery_attempts -> (
          match key_of_json (List.rev reversed_key) with
          | Some key ->
              parse_pending (Pending_map.add key attempts pending) rest
          | None -> None)
      | _ -> None)
  | _ :: _ -> None

let state_of_json = function
  | `Assoc fields
    when List.mem
           (List.assoc_opt "schema_version" fields)
           [ Some (`Int 1); Some (`Int 2) ] -> (
      match Option.bind (List.assoc_opt "watermark" fields) ptime_of_json with
      | None -> None
      | Some watermark -> (
          match List.assoc_opt "fired" fields with
          | Some (`List entries) -> (
              match parse_fired Fired_set.empty entries with
              | None -> None
              | Some fired -> (
                  match List.assoc_opt "schema_version" fields with
                  | Some (`Int 1) ->
                      Some { watermark; fired; pending = Pending_map.empty }
                  | Some (`Int 2) -> (
                      match List.assoc_opt "pending" fields with
                      | Some (`List entries) ->
                          Option.map
                            (fun pending -> { watermark; fired; pending })
                            (parse_pending Pending_map.empty entries)
                      | Some _ | None -> None)
                  | _ -> None))
          | Some _ | None -> None))
  | _ -> None

let load_state_file path =
  try Yojson.Safe.from_file path |> state_of_json
  with Sys_error _ | Yojson.Json_error _ -> None

let save_state_file path state =
  let temporary = ref None in
  let cleanup () =
    match !temporary with
    | None -> ()
    | Some path -> ( try Sys.remove path with Sys_error _ -> ())
  in
  try
    let temporary_path, channel =
      Filename.open_temp_file ~mode:[ Open_binary ] ~perms:0o600
        ~temp_dir:(Filename.dirname path)
        ("." ^ Filename.basename path ^ ".tmp-")
        ""
    in
    temporary := Some temporary_path;
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () ->
        Yojson.Safe.to_channel channel (state_to_json state);
        output_char channel '\n';
        flush channel;
        Unix.fsync (Unix.descr_of_out_channel channel));
    Unix.rename temporary_path path;
    temporary := None;
    (try
       let directory =
         Unix.openfile (Filename.dirname path) [ Unix.O_RDONLY ] 0
       in
       Fun.protect
         ~finally:(fun () -> Unix.close directory)
         (fun () -> Unix.fsync directory)
     with Unix.Unix_error _ -> ());
    Ok ()
  with
  | Sys_error message ->
      cleanup ();
      Error (`Msg ("could not persist alarm state: " ^ message))
  | Unix.Unix_error (error, operation, argument) ->
      cleanup ();
      Error
        (`Msg
           (Printf.sprintf "could not persist alarm state: %s: %s (%s)"
              operation (Unix.error_message error) argument))

let subtract_seconds timestamp seconds =
  match Ptime.sub_span timestamp (Ptime.Span.of_int_s seconds) with
  | Some value -> value
  | None -> Ptime.epoch

let prune_fired ~replay_watermark fired =
  Fired_set.filter
    (fun (key : Alarm_fire_id.t) ->
      Ptime.compare key.fire_time replay_watermark >= 0)
    fired

let prune_pending ~replay_watermark pending =
  Pending_map.filter
    (fun (key : Alarm_fire_id.t) _ ->
      Ptime.compare key.fire_time replay_watermark >= 0)
    pending

let pending_watermark ~default pending =
  Pending_map.fold
    (fun (key : Alarm_fire_id.t) _ earliest ->
      match earliest with
      | None -> Some key.fire_time
      | Some current ->
          Some
            (if Ptime.compare key.fire_time current < 0 then key.fire_time
             else current))
    pending None
  |> Option.value ~default

let fire_summary (alarm_fire : Alarm_query.fire) =
  Option.value ~default:"(no summary)"
    (Component_query.get_summary alarm_fire.owner)

let scan ~driver ~now state =
  let upper =
    match Ptime.add_span now (Ptime.Span.of_int_s 1) with
    | Some value -> value
    | None -> now
  in
  match driver.load_fires ~from:state.watermark ~to_:upper with
  | Error (`Msg message) ->
      driver.report ("Alarm calculation failed: " ^ message);
      Error ()
  | Ok fires ->
      let fired = ref state.fired in
      (* Rebuild this from the authoritative scan. A failed fire that was
         removed from the calendar is no longer retained indefinitely. *)
      let pending = ref Pending_map.empty in
      List.iter
        (fun (alarm_fire : Alarm_query.fire) ->
          if Ptime.compare alarm_fire.fire_time now <= 0 then
            let key = fire_key alarm_fire in
            if not (Fired_set.mem key !fired) then
              match alarm_fire.alarm with
              | `None _ ->
                  (* ACTION:NONE is a real scheduled VALARM, but it requests no
                     delivery. Record the fire so replay remains deterministic
                     without invoking a desktop notifier. *)
                  fired := Fired_set.add key !fired
              | `Audio _ | `Display _ | `Email _ -> (
                  let delivery =
                    try driver.notify alarm_fire
                    with exn ->
                      Error
                        (`Msg
                           ("notifier raised an exception: "
                          ^ Printexc.to_string exn))
                  in
                  match delivery with
                  | Ok () -> fired := Fired_set.add key !fired
                  | Error (`Msg message) ->
                      let attempt =
                        Option.value ~default:0
                          (Pending_map.find_opt key state.pending)
                        + 1
                      in
                      if attempt >= max_delivery_attempts then (
                        fired := Fired_set.add key !fired;
                        driver.report
                          (Printf.sprintf
                             "Notification dropped for %s after %d failed \
                              attempts: %s"
                             (fire_summary alarm_fire) max_delivery_attempts
                             message))
                      else (
                        pending := Pending_map.add key attempt !pending;
                        driver.report
                          (Printf.sprintf
                             "Notification failed for %s (attempt %d/%d; will \
                              retry): %s"
                             (fire_summary alarm_fire) attempt
                             max_delivery_attempts message))))
        fires;
      (* Advancing to the oldest unresolved fire avoids replaying an ever
         growing successful prefix. With the attempt cap, this watermark must
         eventually advance even if the notifier is permanently broken. *)
      let watermark = pending_watermark ~default:now !pending in
      Ok
        {
          watermark;
          (* Keep the just-completed scan's keys for one persisted generation.
             The next scan prunes against its already-advanced watermark. *)
          fired = prune_fired ~replay_watermark:state.watermark !fired;
          pending = !pending;
        }

let run ?max_cycles ~grace ~rescan_interval driver =
  if grace < 0 then Error (`Msg "grace period must be non-negative")
  else if rescan_interval <= 0. then
    Error (`Msg "rescan interval must be greater than zero")
  else if Option.fold ~none:false ~some:(fun value -> value <= 0) max_cycles
  then Error (`Msg "max_cycles must be greater than zero")
  else
    let started_at = driver.now () in
    let grace_start = subtract_seconds started_at grace in
    let persisted =
      Option.value
        ~default:(empty_state ~watermark:grace_start)
        (driver.load_state ())
    in
    let initial =
      if
        Ptime.compare persisted.watermark grace_start < 0
        || Ptime.compare persisted.watermark started_at > 0
      then
        {
          watermark = grace_start;
          fired = prune_fired ~replay_watermark:grace_start persisted.fired;
          pending =
            prune_pending ~replay_watermark:grace_start persisted.pending;
        }
      else persisted
    in
    let rec loop cycle state =
      let now = driver.now () in
      let updated =
        match scan ~driver ~now state with
        | Error () -> state
        | Ok updated ->
            (match driver.save_state updated with
            | Ok () -> ()
            | Error (`Msg message) -> driver.report message);
            updated
      in
      if Option.fold ~none:false ~some:(fun limit -> cycle >= limit) max_cycles
      then Ok updated
      else
        let () =
          match driver.wait ~timeout:rescan_interval with
          | Timer -> ()
          | Changed -> driver.sleep 0.2
          | Overflow ->
              driver.report
                "watcher overflowed; watches were reconciled and a full alarm \
                 rescan is running"
          | Watcher_error message ->
              driver.report
                ("watcher failure; full rescan is running: " ^ message)
        in
        loop (cycle + 1) updated
    in
    loop 1 initial
