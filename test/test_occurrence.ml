open Caledonia_lib

module Event = struct
  include Event

  let create = create ~now:Ptime.epoch
  let edit_patch = edit_patch ~now:Ptime.epoch

  let authored_event event =
    let known = Calendar_codec.make_known (`Event event) |> Result.get_ok in
    let event =
      match Calendar_codec.component known with
      | `Event event -> event
      | _ -> assert false
    in
    let date_until =
      match (event.Icalendar.rrule, Calendar_codec.rrule_date_untils known) with
      | None, [] -> None
      | Some _, [ date_until ] -> date_until
      | _ -> failwith "invalid VEVENT RRULE metadata shape"
    in
    (event, date_until)

  let events_of_icalendar_result ?display_name ?source_fingerprint _calendar_key
      ~file calendar =
    let _ = (display_name, source_fingerprint, file) in
    List.filter_map
      (function `Event event -> Some event | _ -> None)
      (snd calendar)
    |> List.map authored_event |> of_authored_events_result

  let events_of_icalendar ?display_name ?source_fingerprint calendar_key ~file
      calendar =
    events_of_icalendar_result ?display_name ?source_fingerprint calendar_key
      ~file calendar
    |> Result.get_ok

  let to_ical_calendar series =
    let known =
      Calendar_document.known_entries_of_body (Component.event_body series)
      |> Result.get_ok
    in
    Calendar_codec.create_known
      ~properties:
        [
          `Prodid (Icalendar.Params.empty, "-//Caledonia occurrence test//EN");
          `Version (Icalendar.Params.empty, "2.0");
        ]
      known
    |> Calendar_codec.serialize ~cr:true
    |> Calendar_codec.Legacy.parse |> Result.get_ok
end

let fixed_date = Option.get @@ Ptime.of_date_time ((2025, 3, 27), ((0, 0, 0), 0))
let setup_fixed_date () = fixed_date
let calendar_dir_path = Filename.concat (Sys.getcwd ()) "calendar"
let ptime_of ymd hms = Option.get @@ Ptime.of_date_time (ymd, (hms, 0))

let get_events ~fs calendar_dir =
  Calendar_dir.get_components ~fs calendar_dir
  |> Result.map (List.filter_map Component.to_event)

let instant_of_date ~tz date =
  Result.get_ok (Date.ptime_of_ical_result ~floating_tz:tz (`Date date))

let event_from_ics ~fs ics =
  let _ = fs in
  let calendar = Result.get_ok (Icalendar.parse ics) in
  let events =
    List.filter_map
      (function `Event event -> Some event | _ -> None)
      (snd calendar)
  in
  Event.of_events_result events |> Result.get_ok |> List.hd

let first_stored_event ~fs calendar_dir =
  Calendar_dir.get_components ~fs calendar_dir
  |> Result.get_ok
  |> List.find_map (fun component ->
      Option.map
        (fun event -> (component, event))
        (Component.to_event component))
  |> Option.get

let occurrence_start occurrence =
  Event.Occurrence.get_start_result occurrence |> Result.get_ok

let occurrence_summary occurrence = Event.Occurrence.get_summary occurrence

let expand ?(floating_tz = Timedesc.Time_zone.utc) ?max_instances ~from ~to_
    series =
  Event.Recurrence.expand ?max_instances ~floating_tz ~from ~to_ series

let occurrence_reference ?(floating_tz = Timedesc.Time_zone.utc) series
    occurrence_start =
  Event.Recurrence.resolve_reference ~floating_tz series occurrence_start
  |> Result.get_ok

module Query_item = struct
  type t = Stored of Event.t | Occurrence of Event.Occurrence.t

  let summary = function
    | Stored event -> Event.get_summary event
    | Occurrence occurrence -> Event.Occurrence.get_summary occurrence

  let start = function
    | Stored event ->
        Event.get_start_result ~floating_tz:Timedesc.Time_zone.utc event
        |> Result.get_ok
    | Occurrence occurrence -> occurrence_start occurrence
end

let query_items events ~from ~to_ =
  List.concat_map
    (fun event ->
      if Event.has_recurrence_set event then
        expand ~from ~to_ event |> Result.get_ok
        |> List.map (fun occurrence -> Query_item.Occurrence occurrence)
      else
        let start =
          Event.get_start_result ~floating_tz:Timedesc.Time_zone.utc event
          |> Result.get_ok
        in
        let end_ =
          Event.get_end_result ~floating_tz:Timedesc.Time_zone.utc event
          |> Result.get_ok
          |> Option.value ~default:start
        in
        if
          Ptime.compare start to_ < 0
          &&
          match from with
          | None -> true
          | Some lower when Ptime.equal start end_ ->
              Ptime.compare start lower >= 0
          | Some lower -> Ptime.compare end_ lower > 0
        then [ Query_item.Stored event ]
        else [])
    events
  |> List.stable_sort (fun left right ->
      Ptime.compare (Query_item.start left) (Query_item.start right))

let rec remove_tree path =
  if Sys.is_directory path then (
    Sys.readdir path
    |> Array.iter (fun child -> remove_tree (Filename.concat path child));
    Sys.rmdir path)
  else Sys.remove path

(* --- Event wire output includes start_utc --- *)

let%expect_test "event wire output includes start_utc" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _ = setup_fixed_date () in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let events = Result.get_ok @@ get_events ~fs calendar_dir in
  let event =
    List.find (fun e -> Event.get_summary e = Some "Test Event") events
  in
  let sexp = Sexp.event_wire_sexp event in
  let sexp_str = Sexplib.Sexp.to_string_hum sexp in
  (* Check that start_utc is present *)
  Printf.printf "has start_utc: %b\n"
    (try
       ignore (Str.search_forward (Str.regexp "start_utc") sexp_str 0);
       true
     with Not_found -> false);
  (* Check it looks like an RFC 3339 timestamp *)
  Printf.printf "has rfc3339 format: %b\n"
    (try
       ignore
         (Str.search_forward
            (Str.regexp "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T")
            sexp_str 0);
       true
     with Not_found -> false);
  [%expect {|
    has start_utc: true
    has rfc3339 format: true |}]

(* --- Event wire output uses event timezone for start/end --- *)

let%expect_test "event wire output formats times in event timezone not local" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _ = setup_fixed_date () in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let events = Result.get_ok @@ get_events ~fs calendar_dir in
  let event =
    List.find (fun e -> Event.get_summary e = Some "Timezone Test Event") events
  in
  let sexp = Sexp.event_wire_sexp event in
  let sexp_str = Sexplib.Sexp.to_string_hum sexp in
  (* start should be 19:30 (Asia/Kolkata), not 14:00 (UTC) or 15:00 (BST) *)
  Printf.printf "has start 19:30: %b\n"
    (try
       ignore (Str.search_forward (Str.regexp "19:30:00") sexp_str 0);
       true
     with Not_found -> false);
  (* end should be 21:30 (Asia/Kolkata) *)
  Printf.printf "has end 21:30: %b\n"
    (try
       ignore (Str.search_forward (Str.regexp "21:30:00") sexp_str 0);
       true
     with Not_found -> false);
  (* start_tz should be Asia/Kolkata *)
  Printf.printf "has start_tz Kolkata: %b\n"
    (try
       ignore (Str.search_forward (Str.regexp "Asia/Kolkata") sexp_str 0);
       true
     with Not_found -> false);
  [%expect
    {|
    has start 19:30: true
    has end 21:30: true
    has start_tz Kolkata: true |}]

(* --- delete_occurrence adds EXDATE --- *)

let%expect_test "delete_occurrence adds EXDATE to recurring event" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _ = setup_fixed_date () in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let from = Some (ptime_of (2025, 3, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 5, 31) (23, 59, 59) in
  let events = Result.get_ok @@ get_events ~fs calendar_dir in
  (* Find the weekly recurring event *)
  let weekly =
    List.find
      (fun e -> Event.get_summary e = Some "Weekly Recurring Event")
      events
  in
  (* Expand before deletion *)
  let before = expand ~from ~to_ weekly |> Result.get_ok in
  let count_before = List.length before in
  Printf.printf "occurrences before: %d\n" count_before;
  (* Pick the second occurrence's start time to delete *)
  let second_occ = List.nth before 1 in
  let occ_start = occurrence_start second_occ in
  Printf.printf "deleting occurrence at: %s\n" (Ptime.to_rfc3339 occ_start);
  (* Delete that occurrence *)
  let reference = Event.Occurrence.reference second_occ in
  let modified =
    Event.Recurrence.delete_occurrence weekly reference |> Result.get_ok
  in
  let after = expand ~from ~to_ modified |> Result.get_ok in
  let count_after = List.length after in
  Printf.printf "occurrences after: %d\n" count_after;
  Printf.printf "one fewer: %b\n" (count_after = count_before - 1);
  (* Verify the deleted occurrence is gone *)
  let still_has =
    List.exists (fun e -> Ptime.equal (occurrence_start e) occ_start) after
  in
  Printf.printf "deleted occurrence still present: %b\n" still_has;
  [%expect
    {|
    occurrences before: 10
    deleting occurrence at: 2025-04-03T12:00:00-00:00
    occurrences after: 9
    one fewer: true
    deleted occurrence still present: false
    |}]

let%expect_test
    "RDATE-only recurrence participates in query alarms and mutation" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Caledonia RDATE-only test//EN";
        "BEGIN:VEVENT";
        "UID:rdate-only";
        "DTSTAMP:20260701T000000Z";
        "DTSTART:20260715T090000Z";
        "RDATE:20260717T090000Z,20260718T090000Z";
        "EXDATE:20260717T090000Z";
        "SUMMARY:RDATE only";
        "BEGIN:VALARM";
        "ACTION:DISPLAY";
        "TRIGGER:-PT1H";
        "DESCRIPTION:RDATE reminder";
        "END:VALARM";
        "END:VEVENT";
        "END:VCALENDAR";
        "";
      ]
  in
  let calendar = Result.get_ok (Calendar_codec.Legacy.parse source) in
  let file = Eio.Path.(fs / Filename.get_temp_dir_name () / "rdate-only.ics") in
  let event =
    Result.get_ok (Event.events_of_icalendar_result "work" ~file calendar)
    |> List.hd
  in
  let from = Some (ptime_of (2026, 7, 15) (0, 0, 0)) in
  let to_ = ptime_of (2026, 7, 19) (0, 0, 0) in
  let starts value =
    expand ~from ~to_ value |> Result.get_ok
    |> List.map (fun occurrence ->
        occurrence_start occurrence |> Ptime.to_date |> fun (y, m, d) ->
        Printf.sprintf "%04d-%02d-%02d" y m d)
  in
  Printf.printf "recurring=%b starts=%s\n"
    (Event.has_recurrence_set event)
    (String.concat "," (starts event));
  let fires =
    Event.compute_alarm_fires_result ~floating_tz:Timedesc.Time_zone.utc ~from
      ~to_ event
    |> Result.get_ok
    |> List.map (fun (fire : Event.alarm_owner Alarm.fire) ->
        Ptime.to_rfc3339 ~tz_offset_s:0 fire.fire_time)
  in
  Printf.printf "alarm-fires=%s\n" (String.concat "," fires);
  let deleted =
    occurrence_reference event (ptime_of (2026, 7, 18) (9, 0, 0))
    |> Event.Recurrence.delete_occurrence event
    |> Result.get_ok
  in
  Printf.printf "after-delete=%s\n" (String.concat "," (starts deleted));
  let cleared =
    Event.edit_patch ~recurrence:Patch.Clear event |> Result.get_ok
  in
  let wire = Sexp.event_wire_sexp event |> Sexplib.Sexp.to_string in
  let cleared_ics =
    Event.to_ical_calendar cleared |> Calendar_codec.Legacy.to_ics
  in
  let has_property name =
    String.split_on_char '\n' cleared_ics
    |> List.exists (fun line ->
        String.starts_with ~prefix:(name ^ ":") line
        || String.starts_with ~prefix:(name ^ ";") line)
  in
  let retained_as_stored =
    match query_items [ cleared ] ~from ~to_ with
    | [ Query_item.Stored _ ] -> true
    | [] | [ Query_item.Occurrence _ ] | _ :: _ :: _ -> false
  in
  Printf.printf
    "protocol-set=%b cleared=%b/%b/%b stored=%b recurrence-starts=%s\n"
    (try
       ignore
         (Str.search_forward (Str.regexp_string "recurrence_set_value") wire 0);
       ignore (Str.search_forward (Str.regexp_string "RDATE:") wire 0);
       true
     with Not_found -> false)
    (not (Event.has_recurrence_set cleared))
    (not (has_property "RDATE"))
    (not (has_property "EXDATE"))
    retained_as_stored
    (String.concat "," (starts cleared));
  [%expect
    {|
    recurring=true starts=2026-07-15,2026-07-18
    alarm-fires=2026-07-15T08:00:00Z,2026-07-18T08:00:00Z
    after-delete=2026-07-15
    protocol-set=true cleared=true/true/true stored=true recurrence-starts= |}]

let%expect_test
    "nominal series expansion preserves authored occurrence references" =
  Eio_main.run @@ fun env ->
  let _ = env in
  let source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Caledonia nominal occurrence test//EN";
        "BEGIN:VEVENT";
        "UID:nominal-series";
        "DTSTAMP:20260701T000000Z";
        "DTSTART:20260715T090000Z";
        "DURATION:PT1H";
        "RRULE:FREQ=DAILY;COUNT=3";
        "SUMMARY:Master";
        "END:VEVENT";
        "BEGIN:VEVENT";
        "UID:nominal-series";
        "DTSTAMP:20260701T000000Z";
        "RECURRENCE-ID:20260716T090000Z";
        "DTSTART:20260716T110000Z";
        "SUMMARY:Moved override";
        "END:VEVENT";
        "END:VCALENDAR";
        "";
      ]
  in
  let calendar = Result.get_ok (Calendar_codec.Legacy.parse source) in
  let raw_events =
    List.filter_map
      (function `Event event -> Some event | _ -> None)
      (snd calendar)
  in
  let series = Event.of_events_result raw_events |> Result.get_ok |> List.hd in
  Printf.printf "authored=%d overrides=%d recurring=%b valid=%b\n"
    (List.length (Event.authored_events series))
    (List.length (Event.overrides series))
    (Event.has_recurrence_set series)
    (Result.is_ok (Event.validate series));
  let from = Some (ptime_of (2026, 7, 15) (0, 0, 0)) in
  let to_ = ptime_of (2026, 7, 18) (0, 0, 0) in
  let occurrences =
    Event.Recurrence.expand ~floating_tz:Timedesc.Time_zone.utc ~from ~to_
      series
    |> Result.get_ok
  in
  List.iter
    (fun occurrence ->
      let reference = Event.Occurrence.reference occurrence in
      let origin =
        match Event.Occurrence.origin occurrence with
        | Event.Occurrence.Generated -> "generated"
        | Event.Occurrence.Persisted_override -> "override"
      in
      let recurrence_start =
        Event.Occurrence.Reference.occurrence_start reference
        |> Date.rfc3339_utc
      in
      let effective_start =
        Event.Occurrence.get_start_result occurrence
        |> Result.get_ok |> Date.rfc3339_utc
      in
      Printf.printf "%s rid=%s effective=%s summary=%s\n" origin
        recurrence_start effective_start
        (Event.Occurrence.get_summary occurrence
        |> Option.value ~default:"missing"))
    occurrences;
  Printf.printf "nominal=%d\n" (List.length occurrences);
  [%expect
    {|
    authored=2 overrides=1 recurring=true valid=true
    generated rid=2026-07-15T09:00:00Z effective=2026-07-15T09:00:00Z summary=Master
    override rid=2026-07-16T09:00:00Z effective=2026-07-16T11:00:00Z summary=Moved override
    generated rid=2026-07-17T09:00:00Z effective=2026-07-17T09:00:00Z summary=Master
    nominal=3 |}]

let%expect_test "DATE UNTIL includes its final date in a non-UTC query zone" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Caledonia DATE UNTIL membership//EN";
        "BEGIN:VEVENT";
        "UID:date-until-membership";
        "DTSTAMP:20260701T000000Z";
        "DTSTART;VALUE=DATE:20260715";
        "RRULE:FREQ=DAILY;UNTIL=20260716";
        "SUMMARY:DATE UNTIL";
        "END:VEVENT";
        "END:VCALENDAR";
        "";
      ]
  in
  let calendar = Result.get_ok (Calendar_codec.Legacy.parse source) in
  let file = Eio.Path.(fs / Filename.get_temp_dir_name () / "date-until.ics") in
  let event =
    Result.get_ok (Event.events_of_icalendar_result "work" ~file calendar)
    |> List.hd
  in
  let zone = Timedesc.Time_zone.make_exn "Asia/Tokyo" in
  let instant date =
    Date.ptime_of_ical_result ~floating_tz:zone (`Date date) |> Result.get_ok
  in
  let occurrences =
    expand ~floating_tz:zone
      ~from:(Some (instant (2026, 7, 15)))
      ~to_:(instant (2026, 7, 18))
      event
    |> Result.get_ok
  in
  let dates =
    List.map
      (fun occurrence ->
        match
          (Event.Occurrence.effective_ical_event occurrence).Icalendar.dtstart
          |> snd
        with
        | `Date (year, month, day) ->
            Printf.sprintf "%04d-%02d-%02d" year month day
        | `Datetime _ -> "wrong-kind")
      occurrences
  in
  let encoded = Calendar_codec.Legacy.to_ics (Event.to_ical_calendar event) in
  Printf.printf "dates=%s final-date-syntax=%b no-private-marker=%b\n"
    (String.concat "," dates)
    (String.split_on_char '\n' encoded
    |> List.exists (fun line ->
        String.trim line = "RRULE:FREQ=DAILY;UNTIL=20260716"))
    (not
       (String.split_on_char '\n' encoded
       |> List.exists (fun line ->
           String.starts_with ~prefix:"RRULE;X-CALEDONIA-DATE-UNTIL" line)));
  [%expect
    {| dates=2026-07-15,2026-07-16 final-date-syntax=true no-private-marker=true |}]

let%expect_test "recurrence preserves a mixed DTSTART and DTEND representation"
    =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Caledonia mixed recurrence end//EN";
        "BEGIN:VEVENT";
        "UID:mixed-end-kind";
        "DTSTAMP:20260701T000000Z";
        "DTSTART;TZID=Europe/London:20260715T090000";
        "DTEND:20260715T100000Z";
        "RRULE:FREQ=DAILY;COUNT=2";
        "SUMMARY:Mixed end kind";
        "END:VEVENT";
        "END:VCALENDAR";
        "";
      ]
  in
  let calendar = Result.get_ok (Calendar_codec.Legacy.parse source) in
  let file = Eio.Path.(fs / Filename.get_temp_dir_name () / "mixed-end.ics") in
  let event =
    Result.get_ok (Event.events_of_icalendar_result "work" ~file calendar)
    |> List.hd
  in
  let occurrences =
    expand
      ~floating_tz:(Timedesc.Time_zone.make_exn "Europe/London")
      ~from:(Some (ptime_of (2026, 7, 16) (0, 0, 0)))
      ~to_:(ptime_of (2026, 7, 17) (0, 0, 0))
      event
    |> Result.get_ok
  in
  let occurrence = List.hd occurrences in
  let raw_end =
    (Event.Occurrence.effective_ical_event occurrence)
      .Icalendar.dtend_or_duration
  in
  let kind, instant =
    match raw_end with
    | Some (`Dtend (_, `Datetime (`Utc instant))) ->
        ("utc", Ptime.to_rfc3339 ~tz_offset_s:0 instant)
    | _ -> ("wrong", "wrong")
  in
  let protocol_utc =
    match raw_end with
    | Some (`Dtend (_, `Datetime (`Utc _))) -> true
    | _ -> false
  in
  Printf.printf "end=%s/%s protocol-utc=%b\n" kind instant protocol_utc;
  [%expect {| end=utc/2026-07-16T10:00:00Z protocol-utc=true |}]

let%expect_test
    "persisted series and selected occurrence remain nominally distinct" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Caledonia exact source test//EN";
        "BEGIN:VTIMEZONE";
        "TZID:Europe/London";
        "BEGIN:STANDARD";
        "DTSTART:19700101T000000";
        "TZOFFSETFROM:+0000";
        "TZOFFSETTO:+0000";
        "END:STANDARD";
        "END:VTIMEZONE";
        "BEGIN:VEVENT";
        "UID:selected-source";
        "DTSTAMP:20260701T000000Z";
        "DTSTART;TZID=Europe/London:20260715T090000";
        "RRULE:FREQ=DAILY;COUNT=2";
        "SUMMARY:Selected";
        "END:VEVENT";
        "BEGIN:VEVENT";
        "UID:selected-source";
        "RECURRENCE-ID;TZID=Europe/London:20260716T090000";
        "DTSTAMP:20260701T000000Z";
        "DTSTART;TZID=Europe/London:20260716T100000";
        "SUMMARY:Selected override";
        "END:VEVENT";
        "BEGIN:VEVENT";
        "UID:sibling-secret";
        "DTSTAMP:20260701T000000Z";
        "DTSTART:20260715T100000Z";
        "SUMMARY:Sibling";
        "END:VEVENT";
        "END:VCALENDAR";
        "";
      ]
  in
  let file =
    Eio.Path.(fs / Filename.get_temp_dir_name () / "exact-source.ics")
  in
  let selected =
    Calendar_codec.Legacy.parse source
    |> Result.get_ok
    |> Event.events_of_icalendar_result "work" ~file
    |> Result.get_ok
    |> List.find (fun event -> Event.get_id event = "selected-source")
  in
  let occurrence =
    expand
      ~from:(Some (ptime_of (2026, 7, 16) (0, 0, 0)))
      ~to_:(ptime_of (2026, 7, 17) (0, 0, 0))
      selected
    |> Result.get_ok |> List.hd
  in
  let reference = Event.Occurrence.reference occurrence in
  let recurrence_start =
    Event.Occurrence.Reference.occurrence_start reference |> Date.rfc3339_utc
  in
  let effective_start = occurrence_start occurrence |> Date.rfc3339_utc in
  let origin =
    match Event.Occurrence.origin occurrence with
    | Event.Occurrence.Generated -> "generated"
    | Event.Occurrence.Persisted_override -> "override"
  in
  Printf.printf "authored=%d origin=%s target=%s effective=%s summary=%s\n"
    (List.length (Event.authored_events selected))
    origin recurrence_start effective_start
    (occurrence_summary occurrence |> Option.value ~default:"");
  [%expect
    {|
    authored=2 origin=override target=2026-07-16T08:00:00Z effective=2026-07-16T09:00:00Z summary=Selected override |}]

let%expect_test "protocol source_ics reports conflicting timezone definitions" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Caledonia conflicting source test//EN";
        "BEGIN:VTIMEZONE";
        "TZID:Europe/London";
        "BEGIN:STANDARD";
        "DTSTART:19700101T000000";
        "TZOFFSETFROM:+0000";
        "TZOFFSETTO:+0000";
        "END:STANDARD";
        "END:VTIMEZONE";
        "BEGIN:VTIMEZONE";
        "TZID:Europe/London";
        "BEGIN:STANDARD";
        "DTSTART:19700101T000000";
        "TZOFFSETFROM:+0100";
        "TZOFFSETTO:+0100";
        "END:STANDARD";
        "END:VTIMEZONE";
        "BEGIN:VEVENT";
        "UID:timezone-conflict";
        "DTSTAMP:20260701T000000Z";
        "DTSTART;TZID=Europe/London:20260715T090000";
        "SUMMARY:Conflict";
        "END:VEVENT";
        "END:VCALENDAR";
        "";
      ]
  in
  let file =
    Eio.Path.(fs / Filename.get_temp_dir_name () / "timezone-conflict.ics")
  in
  let component_source =
    Component_source.of_decoded_document ~calendar_key:"work" ~file
      ~fingerprint:(Digest.string source |> Digest.to_hex)
      ()
  in
  let document =
    Calendar_codec.parse_document source
    |> Result.get_ok
    |> Calendar_document.decode ~source:component_source
    |> Result.get_ok
  in
  let component = Calendar_document.components document |> List.hd in
  let wire = Sexp.stored_event_wire_sexp ~documents:[ document ] component in
  let source_ics, source_ics_error =
    match wire with
    | Sexplib.Sexp.List fields ->
        List.fold_left
          (fun (source, error) -> function
            | Sexplib.Sexp.List
                [ Sexplib.Sexp.Atom "source_ics"; Sexplib.Sexp.Atom value ] ->
                (Some value, error)
            | Sexplib.Sexp.List
                [
                  Sexplib.Sexp.Atom "source_ics_error"; Sexplib.Sexp.Atom value;
                ] ->
                (source, Some value)
            | _ -> (source, error))
          (None, None) fields
    | Sexplib.Sexp.Atom _ -> assert false
  in
  Printf.printf "source-empty=%b conflict-reported=%b\n" (source_ics = Some "")
    (source_ics_error
   = Some "Conflicting VTIMEZONE definitions for TZID Europe/London");
  [%expect {| source-empty=true conflict-reported=true |}]

(* --- delete_occurrence on existing exdate event --- *)

let%expect_test "delete_occurrence with specific known timestamp" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _ = setup_fixed_date () in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let from = Some (ptime_of (2025, 3, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 5, 31) (23, 59, 59) in
  let events = Result.get_ok @@ get_events ~fs calendar_dir in
  let weekly =
    List.find
      (fun e -> Event.get_summary e = Some "Weekly Recurring Event")
      events
  in
  let before = expand ~from ~to_ weekly |> Result.get_ok in
  Printf.printf "occurrences before: %d\n" (List.length before);
  (* Delete using a manually constructed timestamp for Apr 17 12:00 UTC *)
  let occ_to_delete = ptime_of (2025, 4, 17) (12, 0, 0) in
  (* Verify this occurrence exists *)
  let exists_before =
    List.exists (fun e -> Ptime.equal (occurrence_start e) occ_to_delete) before
  in
  Printf.printf "occurrence exists before: %b\n" exists_before;
  let reference = occurrence_reference weekly occ_to_delete in
  let modified =
    Event.Recurrence.delete_occurrence weekly reference |> Result.get_ok
  in
  let after = expand ~from ~to_ modified |> Result.get_ok in
  Printf.printf "occurrences after: %d\n" (List.length after);
  let exists_after =
    List.exists (fun e -> Ptime.equal (occurrence_start e) occ_to_delete) after
  in
  Printf.printf "occurrence exists after: %b\n" exists_after;
  [%expect
    {|
    occurrences before: 10
    occurrence exists before: true
    occurrences after: 9
    occurrence exists after: false
    |}]

let%expect_test "all-day deletion emits VALUE=DATE and reparses" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let london = Timedesc.Time_zone.make_exn "Europe/London" in
  let event =
    event_from_ics ~fs
      (String.concat "\r\n"
         [
           "BEGIN:VCALENDAR";
           "VERSION:2.0";
           "PRODID:-//Test//EN";
           "BEGIN:VEVENT";
           "UID:all-day-delete@example.test";
           "DTSTAMP:20250301T000000Z";
           "DTSTART;VALUE=DATE:20250329";
           "DTEND;VALUE=DATE:20250330";
           "RRULE:FREQ=DAILY;COUNT=4";
           "SUMMARY:All day";
           "END:VEVENT";
           "END:VCALENDAR";
           "";
         ])
  in
  let occurrence = instant_of_date ~tz:london (2025, 3, 30) in
  let modified =
    let reference = occurrence_reference ~floating_tz:london event occurrence in
    Result.get_ok (Event.Recurrence.delete_occurrence event reference)
  in
  let serialized =
    Icalendar.to_ics ~cr:true (Event.to_ical_calendar modified)
  in
  Printf.printf "typed exdate: %b\n"
    (String.split_on_char '\n' serialized
    |> List.exists (fun line -> String.trim line = "EXDATE;VALUE=DATE:20250330")
    );
  let reparsed = Result.get_ok (Icalendar.parse serialized) in
  let reloaded =
    List.hd
      (Event.events_of_icalendar "test"
         ~file:Eio.Path.(fs / "roundtrip.ics")
         reparsed)
  in
  let from = Some (instant_of_date ~tz:london (2025, 3, 29)) in
  let to_ = instant_of_date ~tz:london (2025, 4, 3) in
  let dates =
    expand ~floating_tz:london ~from ~to_ reloaded
    |> Result.get_ok
    |> List.map (fun occurrence ->
        let instant = occurrence_start occurrence in
        let local = Date.ptime_to_timedesc ~tz:london instant in
        Printf.sprintf "%04d-%02d-%02d" (Timedesc.year local)
          (Timedesc.month local) (Timedesc.day local))
  in
  Printf.printf "remaining: %s\n" (String.concat "," dates);
  [%expect
    {|
    typed exdate: true
    remaining: 2025-03-29,2025-03-31,2025-04-01 |}]

let%expect_test "occurrence deletion validates membership" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let event =
    event_from_ics ~fs
      "BEGIN:VCALENDAR\r\n\
       VERSION:2.0\r\n\
       PRODID:-//Test//EN\r\n\
       BEGIN:VEVENT\r\n\
       UID:membership@example.test\r\n\
       DTSTAMP:20250301T000000Z\r\n\
       DTSTART:20250327T120000Z\r\n\
       RRULE:FREQ=WEEKLY;COUNT=2\r\n\
       SUMMARY:Membership\r\n\
       END:VEVENT\r\n\
       END:VCALENDAR\r\n"
  in
  let invalid = ptime_of (2025, 3, 28) (12, 0, 0) in
  (match
     Event.Recurrence.resolve_reference ~floating_tz:Timedesc.Time_zone.utc
       event invalid
   with
  | Ok _ -> print_endline "unexpected success"
  | Error (`Msg message) -> Printf.printf "rejected: %s\n" message);
  [%expect
    {|
    rejected: requested recurrence is not an active member of this series |}]

let%expect_test "typed occurrence references cannot mutate another series" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let series uid =
    event_from_ics ~fs
      (String.concat "\r\n"
         [
           "BEGIN:VCALENDAR";
           "VERSION:2.0";
           "PRODID:-//Typed reference test//EN";
           "BEGIN:VEVENT";
           "UID:" ^ uid;
           "DTSTAMP:20250301T000000Z";
           "DTSTART:20250327T120000Z";
           "RRULE:FREQ=WEEKLY;COUNT=2";
           "END:VEVENT";
           "END:VCALENDAR";
           "";
         ])
  in
  let first = series "first-reference-series" in
  let second = series "second-reference-series" in
  let occurrence = ptime_of (2025, 4, 3) (12, 0, 0) in
  let reference = occurrence_reference first occurrence in
  let delete_rejected =
    Event.Recurrence.delete_occurrence second reference |> Result.is_error
  in
  let override_rejected =
    Event.Recurrence.create_override ~now:occurrence second reference ()
    |> Result.is_error
  in
  Printf.printf "delete=%b override=%b\n" delete_rejected override_rejected;
  [%expect {| delete=true override=true |}]

let%expect_test "unbounded old recurrence respects work limit" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let event =
    event_from_ics ~fs
      "BEGIN:VCALENDAR\r\n\
       VERSION:2.0\r\n\
       PRODID:-//Test//EN\r\n\
       BEGIN:VEVENT\r\n\
       UID:bounded@example.test\r\n\
       DTSTAMP:20000101T000000Z\r\n\
       DTSTART:20000101T000000Z\r\n\
       RRULE:FREQ=DAILY\r\n\
       SUMMARY:Bounded\r\n\
       END:VEVENT\r\n\
       END:VCALENDAR\r\n"
  in
  let from = Some (ptime_of (2025, 1, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 1, 1) (0, 1, 0) in
  (match expand ~max_instances:50 ~from ~to_ event with
  | Ok _ -> print_endline "unexpected success"
  | Error (`Msg message) -> Printf.printf "bounded: %s\n" message);
  [%expect
    {|
    bounded: recurrence expansion exceeded the 50-instance safety limit |}]

let%expect_test "recurrence reports unknown TZID as a typed error" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let event =
    event_from_ics ~fs
      "BEGIN:VCALENDAR\r\n\
       VERSION:2.0\r\n\
       PRODID:-//Test//EN\r\n\
       BEGIN:VEVENT\r\n\
       UID:unknown-tz@example.test\r\n\
       DTSTAMP:20250301T000000Z\r\n\
       DTSTART;TZID=Mars/Olympus_Mons:20250327T120000\r\n\
       RRULE:FREQ=DAILY;COUNT=2\r\n\
       SUMMARY:Unknown zone\r\n\
       END:VEVENT\r\n\
       END:VCALENDAR\r\n"
  in
  let from = Some (ptime_of (2025, 3, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 4, 1) (0, 0, 0) in
  (match expand ~from ~to_ event with
  | Ok _ -> print_endline "unexpected success"
  | Error (`Msg message) -> Printf.printf "error: %s\n" message);
  [%expect {| error: unknown timezone Mars/Olympus_Mons |}]

let%expect_test "duplicate recurrence overrides are rejected" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar =
    Result.get_ok
      (Icalendar.parse
         (String.concat "\r\n"
            [
              "BEGIN:VCALENDAR";
              "VERSION:2.0";
              "PRODID:-//Test//EN";
              "BEGIN:VEVENT";
              "UID:duplicate@example.test";
              "DTSTAMP:20250301T000000Z";
              "DTSTART:20250327T120000Z";
              "RRULE:FREQ=WEEKLY;COUNT=3";
              "SUMMARY:Master";
              "END:VEVENT";
              "BEGIN:VEVENT";
              "UID:duplicate@example.test";
              "DTSTAMP:20250301T000000Z";
              "RECURRENCE-ID:20250403T120000Z";
              "DTSTART:20250403T130000Z";
              "SUMMARY:First override";
              "END:VEVENT";
              "BEGIN:VEVENT";
              "UID:duplicate@example.test";
              "DTSTAMP:20250301T000000Z";
              "RECURRENCE-ID:20250403T120000Z";
              "DTSTART:20250403T140000Z";
              "SUMMARY:Second override";
              "END:VEVENT";
              "END:VCALENDAR";
              "";
            ]))
  in
  (match
     Event.events_of_icalendar_result "test"
       ~file:Eio.Path.(fs / "duplicates.ics")
       calendar
   with
  | Ok _ -> print_endline "unexpected success"
  | Error (`Msg message) -> Printf.printf "error: %s\n" message);
  [%expect
    {|
    error: VEVENT UID duplicate@example.test has duplicate RECURRENCE-ID overrides |}]

let%expect_test "recurrence override temporal kind must match its master" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar =
    Result.get_ok
      (Icalendar.parse
         "BEGIN:VCALENDAR\r\n\
          VERSION:2.0\r\n\
          PRODID:-//Test//EN\r\n\
          BEGIN:VEVENT\r\n\
          UID:mismatched-override@example.test\r\n\
          DTSTAMP:20250301T000000Z\r\n\
          DTSTART;VALUE=DATE:20250327\r\n\
          RRULE:FREQ=DAILY;COUNT=2\r\n\
          END:VEVENT\r\n\
          BEGIN:VEVENT\r\n\
          UID:mismatched-override@example.test\r\n\
          DTSTAMP:20250301T000000Z\r\n\
          RECURRENCE-ID:20250328T000000Z\r\n\
          DTSTART;VALUE=DATE:20250328\r\n\
          END:VEVENT\r\n\
          END:VCALENDAR\r\n")
  in
  (match
     Event.events_of_icalendar_result "test"
       ~file:Eio.Path.(fs / "mismatched.ics")
       calendar
   with
  | Ok _ -> print_endline "unexpected success"
  | Error (`Msg message) -> Printf.printf "error: %s\n" message);
  [%expect
    {|
    error: VEVENT UID mismatched-override@example.test has a RECURRENCE-ID whose value kind does not match DTSTART |}]

let%expect_test "THISANDFUTURE overrides are rejected instead of misexpanded" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar =
    Calendar_codec.Legacy.parse
      "BEGIN:VCALENDAR\r\n\
       VERSION:2.0\r\n\
       PRODID:-//Range test//EN\r\n\
       BEGIN:VEVENT\r\n\
       UID:range@example.test\r\n\
       DTSTAMP:20260701T000000Z\r\n\
       DTSTART:20260701T090000Z\r\n\
       RRULE:FREQ=DAILY;COUNT=3\r\n\
       SUMMARY:Master\r\n\
       END:VEVENT\r\n\
       BEGIN:VEVENT\r\n\
       UID:range@example.test\r\n\
       DTSTAMP:20260701T000000Z\r\n\
       RECURRENCE-ID;RANGE=THISANDFUTURE:20260702T090000Z\r\n\
       DTSTART:20260702T100000Z\r\n\
       SUMMARY:Shift onward\r\n\
       END:VEVENT\r\n\
       END:VCALENDAR\r\n"
    |> Result.get_ok
  in
  (match
     Event.events_of_icalendar_result "test"
       ~file:Eio.Path.(fs / "range.ics")
       calendar
   with
  | Ok _ -> print_endline "unexpected success"
  | Error (`Msg message) -> print_endline message);
  [%expect
    {| VEVENT UID range@example.test uses unsupported RECURRENCE-ID RANGE=THISANDFUTURE |}]

let%expect_test
    "direct and loaded overrides reject unsupported recurrence semantics" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let master =
    event_from_ics ~fs
      "BEGIN:VCALENDAR\r\n\
       VERSION:2.0\r\n\
       PRODID:-//Override domain//EN\r\n\
       BEGIN:VEVENT\r\n\
       UID:override-domain\r\n\
       DTSTAMP:20260701T000000Z\r\n\
       DTSTART:20260701T090000Z\r\n\
       RRULE:FREQ=DAILY;COUNT=3\r\n\
       SUMMARY:Master\r\n\
       END:VEVENT\r\n\
       END:VCALENDAR\r\n"
  in
  let candidate extra =
    let calendar =
      Calendar_codec.Legacy.parse
        (String.concat "\r\n"
           ([
              "BEGIN:VCALENDAR";
              "VERSION:2.0";
              "PRODID:-//Override candidate//EN";
              "BEGIN:VEVENT";
              "UID:override-domain";
              "DTSTAMP:20260701T000000Z";
              "DTSTART:20260702T100000Z";
              "RECURRENCE-ID:20260702T090000Z";
            ]
           @ extra
           @ [ "END:VEVENT"; "END:VCALENDAR"; "" ]))
      |> Result.get_ok
    in
    match snd calendar with [ `Event event ] -> event | _ -> assert false
  in
  let range_candidate = candidate [] in
  let range_params =
    Icalendar.Params.empty
    |> Icalendar.Params.add Icalendar.Range `Thisandfuture
  in
  let range_candidate =
    {
      range_candidate with
      props =
        List.map
          (function
            | `Recur_id (_, value) -> `Recur_id (range_params, value)
            | property -> property)
          range_candidate.props;
    }
  in
  let direct_candidates =
    [
      range_candidate;
      candidate [ "RRULE:FREQ=DAILY;COUNT=2" ];
      candidate [ "RDATE:20260703T090000Z" ];
      candidate [ "EXDATE:20260703T090000Z" ];
    ]
  in
  let loaded_reject property =
    let source =
      String.concat "\r\n"
        [
          "BEGIN:VCALENDAR";
          "VERSION:2.0";
          "PRODID:-//Override load domain//EN";
          "BEGIN:VEVENT";
          "UID:override-load";
          "DTSTAMP:20260701T000000Z";
          "DTSTART:20260701T090000Z";
          "RRULE:FREQ=DAILY;COUNT=3";
          "END:VEVENT";
          "BEGIN:VEVENT";
          "UID:override-load";
          "DTSTAMP:20260701T000000Z";
          "DTSTART:20260702T100000Z";
          "RECURRENCE-ID:20260702T090000Z";
          property;
          "END:VEVENT";
          "END:VCALENDAR";
          "";
        ]
    in
    match Calendar_codec.Legacy.parse source with
    | Error _ -> true
    | Ok calendar ->
        Event.events_of_icalendar_result "test"
          ~file:Eio.Path.(fs / "override-load.ics")
          calendar
        |> Result.is_error
  in
  let reference =
    occurrence_reference master (ptime_of (2026, 7, 2) (9, 0, 0))
  in
  Printf.printf "direct RANGE/RRULE/RDATE/EXDATE rejected=%b\n"
    (List.for_all
       (fun candidate ->
         Result.is_error
           (Event.Recurrence.validate_override master reference candidate))
       direct_candidates);
  Printf.printf "loaded RRULE/RDATE/EXDATE rejected=%b\n"
    (List.for_all loaded_reject
       [
         "RRULE:FREQ=DAILY;COUNT=2";
         "RDATE:20260703T090000Z";
         "EXDATE:20260703T090000Z";
       ]);
  [%expect
    {|
    direct RANGE/RRULE/RDATE/EXDATE rejected=true
    loaded RRULE/RDATE/EXDATE rejected=true |}]

let%expect_test "empty alarm Set explicitly clears inherited occurrence alarms"
    =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let master =
    event_from_ics ~fs
      "BEGIN:VCALENDAR\r\n\
       VERSION:2.0\r\n\
       PRODID:-//Alarm clear//EN\r\n\
       BEGIN:VEVENT\r\n\
       UID:alarm-clear\r\n\
       DTSTAMP:20260701T000000Z\r\n\
       DTSTART:20260701T090000Z\r\n\
       RRULE:FREQ=DAILY;COUNT=2\r\n\
       BEGIN:VALARM\r\n\
       ACTION:DISPLAY\r\n\
       TRIGGER:-PT5M\r\n\
       DESCRIPTION:Reminder\r\n\
       END:VALARM\r\n\
       END:VEVENT\r\n\
       END:VCALENDAR\r\n"
  in
  let occurrence = ptime_of (2026, 7, 2) (9, 0, 0) in
  let reference = occurrence_reference master occurrence in
  let override =
    Event.Recurrence.create_override
      ~now:(ptime_of (2026, 7, 1) (0, 0, 0))
      master reference ~alarms:(Patch.Set []) ()
    |> Result.get_ok
  in
  let properties, components = Event.to_ical_calendar master in
  let loaded =
    Event.events_of_icalendar_result "test"
      ~file:Eio.Path.(fs / "alarm-clear.ics")
      (properties, components @ [ `Event override ])
    |> Result.get_ok |> List.hd
  in
  let expanded =
    expand ~from:(Some occurrence) ~to_:(ptime_of (2026, 7, 3) (0, 0, 0)) loaded
    |> Result.get_ok
  in
  let cleared =
    List.find
      (fun event -> Ptime.equal (occurrence_start event) occurrence)
      expanded
  in
  Printf.printf "marker=%b effective-empty=%b\n"
    ( Calendar_codec.Legacy.to_ics ~cr:true
        ( [
            `Prodid (Icalendar.Params.empty, "-//Caledonia alarm clear test//EN");
            `Version (Icalendar.Params.empty, "2.0");
          ],
          [ `Event override ] )
    |> fun source ->
      try
        ignore
          (Str.search_forward
             (Str.regexp_string "X-CALEDONIA-CLEARED:VALARM")
             source 0);
        true
      with Not_found -> false )
    (Event.Occurrence.get_alarms cleared = []);
  [%expect {| marker=true effective-empty=true |}]

let%expect_test "multiple recurrence masters are rejected at load" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar =
    Result.get_ok
      (Icalendar.parse
         "BEGIN:VCALENDAR\r\n\
          VERSION:2.0\r\n\
          PRODID:-//Test//EN\r\n\
          BEGIN:VEVENT\r\n\
          UID:multi-master@example.test\r\n\
          DTSTAMP:20250301T000000Z\r\n\
          DTSTART:20250327T120000Z\r\n\
          RRULE:FREQ=DAILY\r\n\
          END:VEVENT\r\n\
          BEGIN:VEVENT\r\n\
          UID:multi-master@example.test\r\n\
          DTSTAMP:20250301T000000Z\r\n\
          DTSTART:20250328T120000Z\r\n\
          RRULE:FREQ=DAILY\r\n\
          END:VEVENT\r\n\
          END:VCALENDAR\r\n")
  in
  (match
     Event.events_of_icalendar_result "test"
       ~file:Eio.Path.(fs / "multiple.ics")
       calendar
   with
  | Ok _ -> print_endline "unexpected success"
  | Error (`Msg message) -> Printf.printf "error: %s\n" message);
  [%expect
    {|
    error: VEVENT UID multi-master@example.test has multiple recurrence masters |}]

(* --- create_occurrence_override --- *)

let%expect_test "create_occurrence_override has correct structure" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _ = setup_fixed_date () in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let events = Result.get_ok @@ get_events ~fs calendar_dir in
  let weekly =
    List.find
      (fun e -> Event.get_summary e = Some "Weekly Recurring Event")
      events
  in
  let occ_start = ptime_of (2025, 4, 3) (12, 0, 0) in
  let reference = occurrence_reference weekly occ_start in
  let override =
    Event.Recurrence.create_override ~now:(setup_fixed_date ()) weekly reference
      ~summary:(Patch.Set "Modified Weekly") ~location:(Patch.Set "New Room") ()
    |> Result.get_ok
  in
  (* Check UID matches parent *)
  Printf.printf "uid matches: %b\n"
    (snd override.Icalendar.uid = Event.get_id weekly);
  (* Check no RRULE *)
  Printf.printf "no rrule: %b\n" (override.Icalendar.rrule = None);
  (* Check has RECURRENCE-ID *)
  let has_recur_id =
    List.exists
      (function `Recur_id _ -> true | _ -> false)
      override.Icalendar.props
  in
  Printf.printf "has recurrence-id: %b\n" has_recur_id;
  (* Check summary was overridden *)
  let has_summary =
    List.exists
      (function `Summary (_, s) -> s = "Modified Weekly" | _ -> false)
      override.Icalendar.props
  in
  Printf.printf "has overridden summary: %b\n" has_summary;
  (* Check location was overridden *)
  let has_location =
    List.exists
      (function `Location (_, l) -> l = "New Room" | _ -> false)
      override.Icalendar.props
  in
  Printf.printf "has overridden location: %b\n" has_location;
  [%expect
    {|
    uid matches: true
    no rrule: true
    has recurrence-id: true
    has overridden summary: true
    has overridden location: true |}]

let%expect_test "create_occurrence_override inherits unmodified props" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _ = setup_fixed_date () in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let events = Result.get_ok @@ get_events ~fs calendar_dir in
  let weekly =
    List.find
      (fun e -> Event.get_summary e = Some "Weekly Recurring Event")
      events
  in
  let occ_start = ptime_of (2025, 4, 3) (12, 0, 0) in
  let reference = occurrence_reference weekly occ_start in
  (* Only change location, leave summary untouched *)
  let override =
    Event.Recurrence.create_override ~now:(setup_fixed_date ()) weekly reference
      ~location:(Patch.Set "New Room") ()
    |> Result.get_ok
  in
  (* Summary should be inherited from parent *)
  let has_parent_summary =
    List.exists
      (function `Summary (_, s) -> s = "Weekly Recurring Event" | _ -> false)
      override.Icalendar.props
  in
  Printf.printf "inherited parent summary: %b\n" has_parent_summary;
  Printf.printf "uses occurrence start: %b\n"
    (Result.get_ok
       (Date.ptime_of_ical_result ~floating_tz:Timedesc.Time_zone.utc
          (snd override.Icalendar.dtstart))
    = occ_start);
  [%expect
    {|
    inherited parent summary: true
    uses occurrence start: true |}]

let%expect_test "all-day override preserves exclusive DTEND across DST" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let london = Timedesc.Time_zone.make_exn "Europe/London" in
  let event =
    event_from_ics ~fs
      "BEGIN:VCALENDAR\r\n\
       VERSION:2.0\r\n\
       PRODID:-//Test//EN\r\n\
       BEGIN:VEVENT\r\n\
       UID:all-day-override@example.test\r\n\
       DTSTAMP:20251001T000000Z\r\n\
       DTSTART;VALUE=DATE:20251025\r\n\
       DTEND;VALUE=DATE:20251026\r\n\
       RRULE:FREQ=DAILY;COUNT=3\r\n\
       SUMMARY:DST all day\r\n\
       END:VEVENT\r\n\
       END:VCALENDAR\r\n"
  in
  let occurrence = instant_of_date ~tz:london (2025, 10, 26) in
  let reference = occurrence_reference ~floating_tz:london event occurrence in
  let override =
    Result.get_ok
      (Event.Recurrence.create_override ~now:occurrence event reference ())
  in
  (match
     (snd override.Icalendar.dtstart, override.Icalendar.dtend_or_duration)
   with
  | `Date (sy, sm, sd), Some (`Dtend (_, `Date (ey, em, ed))) ->
      Printf.printf "start=%04d-%02d-%02d end=%04d-%02d-%02d\n" sy sm sd ey em
        ed
  | _ -> print_endline "wrong temporal kind");
  [%expect {| start=2025-10-26 end=2025-10-27 |}]

let%expect_test
    "floating occurrence override and delete preserve local wall time across \
     DST" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let london = Timedesc.Time_zone.make_exn "Europe/London" in
  let event =
    event_from_ics ~fs
      "BEGIN:VCALENDAR\r\n\
       VERSION:2.0\r\n\
       PRODID:-//Test//EN\r\n\
       BEGIN:VEVENT\r\n\
       UID:floating-dst@example.test\r\n\
       DTSTAMP:20260301T000000Z\r\n\
       DTSTART:20260328T090000\r\n\
       DTEND:20260328T100000\r\n\
       RRULE:FREQ=DAILY;COUNT=3\r\n\
       SUMMARY:Floating DST\r\n\
       END:VEVENT\r\n\
       END:VCALENDAR\r\n"
  in
  let occurrence = ptime_of (2026, 3, 29) (8, 0, 0) in
  let reference = occurrence_reference ~floating_tz:london event occurrence in
  let override =
    Result.get_ok
      (Event.Recurrence.create_override ~now:occurrence event reference ())
  in
  let override_source =
    Icalendar.to_ics ~cr:true
      ([ `Version (Icalendar.Params.empty, "2.0") ], [ `Event override ])
  in
  let deleted =
    Result.get_ok (Event.Recurrence.delete_occurrence event reference)
  in
  let deleted_source =
    Icalendar.to_ics ~cr:true (Event.to_ical_calendar deleted)
  in
  let has_line source line =
    source |> String.split_on_char '\n'
    |> List.exists (fun value -> String.trim value = line)
  in
  Printf.printf
    "override-rid-local=%b override-start-local=%b exdate-local=%b\n"
    (has_line override_source "RECURRENCE-ID:20260329T090000")
    (has_line override_source "DTSTART:20260329T090000")
    (has_line deleted_source "EXDATE:20260329T090000");
  [%expect
    {|
    override-rid-local=true override-start-local=true exdate-local=true
    |}]

let%expect_test "persisted non-member override cannot authorize mutation" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar =
    Result.get_ok
      (Calendar_codec.Legacy.parse
         "BEGIN:VCALENDAR\r\n\
          VERSION:2.0\r\n\
          PRODID:-//Membership test//EN\r\n\
          BEGIN:VEVENT\r\n\
          UID:invalid-member@example.test\r\n\
          DTSTAMP:20260701T000000Z\r\n\
          DTSTART:20260715T090000Z\r\n\
          RRULE:FREQ=DAILY;COUNT=2\r\n\
          SUMMARY:Master\r\n\
          END:VEVENT\r\n\
          BEGIN:VEVENT\r\n\
          UID:invalid-member@example.test\r\n\
          DTSTAMP:20260701T000000Z\r\n\
          RECURRENCE-ID:20260725T090000Z\r\n\
          DTSTART:20260725T100000Z\r\n\
          SUMMARY:Invalid override\r\n\
          END:VEVENT\r\n\
          END:VCALENDAR\r\n")
  in
  let loaded =
    Event.events_of_icalendar_result "test"
      ~file:Eio.Path.(fs / "invalid-member.ics")
      calendar
  in
  Printf.printf "load-rejected=%b\n" (Result.is_error loaded);
  [%expect {| load-rejected=true |}]

let%expect_test "sparse persisted overrides inherit and remain patchable" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let event =
    event_from_ics ~fs
      "BEGIN:VCALENDAR\r\n\
       VERSION:2.0\r\n\
       PRODID:-//Sparse override test//EN\r\n\
       BEGIN:VEVENT\r\n\
       UID:sparse-override@example.test\r\n\
       DTSTAMP:20260701T000000Z\r\n\
       DTSTART:20260715T090000Z\r\n\
       DTEND:20260715T100000Z\r\n\
       RRULE:FREQ=DAILY;COUNT=2\r\n\
       SUMMARY:Master summary\r\n\
       LOCATION:Room A\r\n\
       DESCRIPTION:Master description\r\n\
       CATEGORIES:One,Two\r\n\
       STATUS:CONFIRMED\r\n\
       PRIORITY:5\r\n\
       BEGIN:VALARM\r\n\
       ACTION:DISPLAY\r\n\
       TRIGGER:-PT5M\r\n\
       DESCRIPTION:Reminder\r\n\
       END:VALARM\r\n\
       END:VEVENT\r\n\
       BEGIN:VEVENT\r\n\
       UID:sparse-override@example.test\r\n\
       DTSTAMP:20260701T000000Z\r\n\
       RECURRENCE-ID:20260716T090000Z\r\n\
       DTSTART:20260716T093000Z\r\n\
       SUMMARY:Sparse summary\r\n\
       STATUS:TENTATIVE\r\n\
       PRIORITY:7\r\n\
       END:VEVENT\r\n\
       END:VCALENDAR\r\n"
  in
  let occurrence = ptime_of (2026, 7, 16) (9, 0, 0) in
  let effective =
    Result.get_ok
      (expand
         ~from:(Some (ptime_of (2026, 7, 16) (0, 0, 0)))
         ~to_:(ptime_of (2026, 7, 17) (0, 0, 0))
         event)
    |> List.hd
  in
  Printf.printf
    "effective start=%s end=%s location=%s description=%s categories=%s \
     alarms=%d\n"
    (Ptime.to_rfc3339 (occurrence_start effective))
    (Event.Occurrence.get_end_result effective
    |> Result.get_ok |> Option.get |> Ptime.to_rfc3339)
    (Event.Occurrence.get_location effective |> Option.value ~default:"missing")
    (Event.Occurrence.get_description effective
    |> Option.value ~default:"missing")
    (String.concat "," (Event.Occurrence.get_categories effective))
    (List.length (Event.Occurrence.get_alarms effective));
  let first =
    Result.get_ok
      (Event.Recurrence.create_override
         ~now:(ptime_of (2026, 7, 1) (0, 0, 0))
         event
         (Event.Occurrence.reference effective)
         ~description:(Patch.Set "Changed description") ())
  in
  let property predicate properties =
    List.find_map predicate properties |> Option.value ~default:"missing"
  in
  let summary candidate =
    property
      (function `Summary (_, value) -> Some value | _ -> None)
      candidate.Icalendar.props
  in
  let location candidate =
    property
      (function `Location (_, value) -> Some value | _ -> None)
      candidate.Icalendar.props
  in
  let description candidate =
    property
      (function `Description (_, value) -> Some value | _ -> None)
      candidate.Icalendar.props
  in
  let categories candidate =
    List.find_map
      (function `Categories (_, value) -> Some value | _ -> None)
      candidate.Icalendar.props
    |> Option.value ~default:[]
  in
  let statuses candidate =
    List.filter_map
      (function `Status (_, value) -> Some value | _ -> None)
      candidate.Icalendar.props
  in
  let priorities candidate =
    List.filter_map
      (function `Priority (_, value) -> Some value | _ -> None)
      candidate.Icalendar.props
  in
  let status_name = function
    | `Tentative -> "TENTATIVE"
    | `Confirmed -> "CONFIRMED"
    | `Cancelled -> "CANCELLED"
    | _ -> "INVALID"
  in
  let end_time candidate =
    match candidate.Icalendar.dtend_or_duration with
    | Some (`Dtend (_, value)) ->
        Date.ptime_of_ical_result ~floating_tz:Timedesc.Time_zone.utc value
        |> Result.get_ok |> Ptime.to_rfc3339
    | Some (`Duration _) -> "duration"
    | None -> "none"
  in
  Printf.printf
    "first summary=%s location=%s description=%s categories=%s end=%s \
     alarms=%d status=%s/%d priority=%d/%d\n"
    (summary first) (location first) (description first)
    (String.concat "," (categories first))
    (end_time first)
    (List.length first.Icalendar.alarms)
    (status_name (List.hd (statuses first)))
    (List.length (statuses first))
    (List.hd (priorities first))
    (List.length (priorities first));
  let properties, components = Event.to_ical_calendar event in
  let components =
    List.map
      (function
        | `Event candidate
          when List.exists
                 (function `Recur_id _ -> true | _ -> false)
                 candidate.Icalendar.props ->
            `Event first
        | component -> component)
      components
  in
  let serialized =
    Calendar_codec.Legacy.to_ics ~cr:true (properties, components)
  in
  let reparsed = Result.get_ok (Calendar_codec.Legacy.parse serialized) in
  let reloaded =
    List.hd
      (Result.get_ok
         (Event.events_of_icalendar_result "test"
            ~file:Eio.Path.(fs / "sparse.ics")
            reparsed))
  in
  let second =
    let reference = occurrence_reference reloaded occurrence in
    Result.get_ok
      (Event.Recurrence.create_override
         ~now:(ptime_of (2026, 7, 1) (0, 1, 0))
         reloaded reference ~summary:Patch.Clear ~location:Patch.Clear
         ~description:Patch.Clear ~categories:Patch.Clear ~end_:Patch.Clear
         ~alarms:Patch.Clear ())
  in
  Printf.printf
    "second summary=%s location=%s description=%s categories=%d end=%s \
     alarms=%d status=%s/%d priority=%d/%d\n"
    (summary second) (location second) (description second)
    (List.length (categories second))
    (end_time second)
    (List.length second.Icalendar.alarms)
    (status_name (List.hd (statuses second)))
    (List.length (statuses second))
    (List.hd (priorities second))
    (List.length (priorities second));
  let properties, components = Event.to_ical_calendar reloaded in
  let components =
    List.map
      (function
        | `Event candidate
          when List.exists
                 (function `Recur_id _ -> true | _ -> false)
                 candidate.Icalendar.props ->
            `Event second
        | component -> component)
      components
  in
  let final_calendar =
    Result.get_ok
      (Calendar_codec.Legacy.parse
         (Calendar_codec.Legacy.to_ics ~cr:true (properties, components)))
  in
  let final_master =
    List.hd
      (Result.get_ok
         (Event.events_of_icalendar_result "test"
            ~file:Eio.Path.(fs / "sparse-final.ics")
            final_calendar))
  in
  let effective_cleared =
    Result.get_ok
      (expand
         ~from:(Some (ptime_of (2026, 7, 16) (0, 0, 0)))
         ~to_:(ptime_of (2026, 7, 17) (0, 0, 0))
         final_master)
    |> List.hd
  in
  Printf.printf
    "effective cleared summary=%b location=%b description=%b categories=%d \
     end=%b alarms=%d\n"
    (Option.is_none (Event.Occurrence.get_summary effective_cleared))
    (Option.is_none (Event.Occurrence.get_location effective_cleared))
    (Option.is_none (Event.Occurrence.get_description effective_cleared))
    (List.length (Event.Occurrence.get_categories effective_cleared))
    (Event.Occurrence.get_end_result effective_cleared
    |> Result.get_ok |> Option.is_none)
    (List.length (Event.Occurrence.get_alarms effective_cleared));
  let final_override =
    List.find_map
      (function
        | `Event candidate
          when List.exists
                 (function `Recur_id _ -> true | _ -> false)
                 candidate.Icalendar.props ->
            Some candidate
        | _ -> None)
      (snd (Event.to_ical_calendar final_master))
    |> Option.get
  in
  Printf.printf "reloaded status=%s/%d priority=%d/%d\n"
    (status_name (List.hd (statuses final_override)))
    (List.length (statuses final_override))
    (List.hd (priorities final_override))
    (List.length (priorities final_override));
  let third =
    let reference = occurrence_reference final_master occurrence in
    Result.get_ok
      (Event.Recurrence.create_override
         ~now:(ptime_of (2026, 7, 1) (0, 2, 0))
         final_master reference ())
  in
  let has_clear_marker candidate =
    List.exists
      (function
        | `Xprop ((vendor, name), _, _) ->
            (vendor = "CALEDONIA" && name = "CLEARED")
            || (vendor = "" && name = "CALEDONIA-CLEARED")
        | _ -> false)
      candidate.Icalendar.props
  in
  Printf.printf
    "third summary=%s location=%s description=%s categories=%d end=%s \
     alarms=%d marker=%b status=%s/%d priority=%d/%d\n"
    (summary third) (location third) (description third)
    (List.length (categories third))
    (end_time third)
    (List.length third.Icalendar.alarms)
    (has_clear_marker third)
    (status_name (List.hd (statuses third)))
    (List.length (statuses third))
    (List.hd (priorities third))
    (List.length (priorities third));
  let properties, components = Event.to_ical_calendar final_master in
  let components =
    List.map
      (function
        | `Event candidate
          when List.exists
                 (function `Recur_id _ -> true | _ -> false)
                 candidate.Icalendar.props ->
            `Event third
        | component -> component)
      components
  in
  let third_reloads =
    match
      Calendar_codec.Legacy.to_ics ~cr:true (properties, components)
      |> Calendar_codec.Legacy.parse
    with
    | Error _ -> false
    | Ok calendar ->
        Event.events_of_icalendar_result "test"
          ~file:Eio.Path.(fs / "sparse-third.ics")
          calendar
        |> Result.is_ok
  in
  Printf.printf "third reloads: %b\n" third_reloads;
  [%expect
    {|
    effective start=2026-07-16T09:30:00-00:00 end=2026-07-16T10:30:00-00:00 location=Room A description=Master description categories=One,Two alarms=1
    first summary=Sparse summary location=Room A description=Changed description categories=One,Two end=2026-07-16T10:30:00-00:00 alarms=1 status=TENTATIVE/1 priority=7/1
    second summary=missing location=missing description=missing categories=0 end=none alarms=0 status=TENTATIVE/1 priority=7/1
    effective cleared summary=true location=true description=true categories=0 end=true alarms=0
    reloaded status=TENTATIVE/1 priority=7/1
    third summary=missing location=missing description=missing categories=0 end=none alarms=0 marker=true status=TENTATIVE/1 priority=7/1
    third reloads: true |}]

let%expect_test "moved overrides do not corrupt recurrence query bounds" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let moved_out =
    event_from_ics ~fs
      "BEGIN:VCALENDAR\r\n\
       VERSION:2.0\r\n\
       PRODID:-//Moved bounds test//EN\r\n\
       BEGIN:VEVENT\r\n\
       UID:moved-out@example.test\r\n\
       DTSTAMP:20260701T000000Z\r\n\
       DTSTART:20260701T090000Z\r\n\
       DURATION:PT1H\r\n\
       RRULE:FREQ=DAILY;COUNT=10\r\n\
       SUMMARY:Master\r\n\
       END:VEVENT\r\n\
       BEGIN:VEVENT\r\n\
       UID:moved-out@example.test\r\n\
       DTSTAMP:20260701T000000Z\r\n\
       RECURRENCE-ID:20260701T090000Z\r\n\
       DTSTART:20261201T090000Z\r\n\
       SUMMARY:Moved out\r\n\
       END:VEVENT\r\n\
       END:VCALENDAR\r\n"
  in
  let moved_in =
    event_from_ics ~fs
      "BEGIN:VCALENDAR\r\n\
       VERSION:2.0\r\n\
       PRODID:-//Moved bounds test//EN\r\n\
       BEGIN:VEVENT\r\n\
       UID:moved-in@example.test\r\n\
       DTSTAMP:20260701T000000Z\r\n\
       DTSTART:20260701T090000Z\r\n\
       DURATION:PT1H\r\n\
       RRULE:FREQ=DAILY;COUNT=40\r\n\
       SUMMARY:Master\r\n\
       END:VEVENT\r\n\
       BEGIN:VEVENT\r\n\
       UID:moved-in@example.test\r\n\
       DTSTAMP:20260701T000000Z\r\n\
       RECURRENCE-ID:20260720T090000Z\r\n\
       DTSTART:20260705T120000Z\r\n\
       SUMMARY:Moved in\r\n\
       END:VEVENT\r\n\
       END:VCALENDAR\r\n"
  in
  let far_future =
    event_from_ics ~fs
      "BEGIN:VCALENDAR\r\n\
       VERSION:2.0\r\n\
       PRODID:-//Moved bounds test//EN\r\n\
       BEGIN:VEVENT\r\n\
       UID:far-future@example.test\r\n\
       DTSTAMP:20260701T000000Z\r\n\
       DTSTART:20260701T090000Z\r\n\
       RRULE:FREQ=DAILY\r\n\
       SUMMARY:Master\r\n\
       END:VEVENT\r\n\
       BEGIN:VEVENT\r\n\
       UID:far-future@example.test\r\n\
       DTSTAMP:20260701T000000Z\r\n\
       RECURRENCE-ID:20300701T090000Z\r\n\
       DTSTART:20301201T090000Z\r\n\
       SUMMARY:Far future\r\n\
       END:VEVENT\r\n\
       END:VCALENDAR\r\n"
  in
  let expand_window event =
    expand
      ~from:(Some (ptime_of (2026, 7, 1) (0, 0, 0)))
      ~to_:(ptime_of (2026, 7, 10) (0, 0, 0))
      event
    |> Result.get_ok
  in
  let outside = expand_window moved_out in
  let inside = expand_window moved_in in
  let bounded_future =
    expand ~max_instances:20
      ~from:(Some (ptime_of (2026, 7, 1) (0, 0, 0)))
      ~to_:(ptime_of (2026, 7, 10) (0, 0, 0))
      far_future
    |> Result.get_ok
  in
  Printf.printf "moved-out remaining=%d first=%s last=%s\n"
    (List.length outside)
    (List.hd outside |> occurrence_start |> Ptime.to_rfc3339)
    (List.rev outside |> List.hd |> occurrence_start |> Ptime.to_rfc3339);
  let moved =
    List.find (fun event -> occurrence_summary event = Some "Moved in") inside
  in
  Printf.printf "moved-in count=%d found=%s end=%s\n" (List.length inside)
    (occurrence_start moved |> Ptime.to_rfc3339)
    (Event.Occurrence.get_end_result moved
    |> Result.get_ok |> Option.get |> Ptime.to_rfc3339);
  Printf.printf "far-future bounded count=%d\n" (List.length bounded_future);
  [%expect
    {|
    moved-out remaining=8 first=2026-07-02T09:00:00-00:00 last=2026-07-09T09:00:00-00:00
    moved-in count=10 found=2026-07-05T12:00:00-00:00 end=2026-07-05T13:00:00-00:00
    far-future bounded count=9 |}]

(* --- add_occurrence_override preserves parent series --- *)

let%expect_test "add_occurrence_override preserves recurring series" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _ = setup_fixed_date () in
  (* Set up a temp calendar dir with a recurring event *)
  let tmp_dir = Filename.temp_dir "caledonia_test" "" in
  let cal_name = "test_cal" in
  let cal_path = Filename.concat tmp_dir cal_name in
  Sys.mkdir cal_path 0o755;
  let ics_content =
    "BEGIN:VCALENDAR\r\n\
     VERSION:2.0\r\n\
     PRODID:-//Test//EN\r\n\
     BEGIN:VEVENT\r\n\
     UID:override-test@caledonia.test\r\n\
     DTSTAMP:20250327T000000Z\r\n\
     DTSTART:20250327T120000Z\r\n\
     DTEND:20250327T130000Z\r\n\
     SUMMARY:Weekly Meeting\r\n\
     LOCATION:Room A\r\n\
     RRULE:FREQ=WEEKLY\r\n\
     END:VEVENT\r\n\
     END:VCALENDAR\r\n"
  in
  let ics_path = Filename.concat cal_path "override-test.ics" in
  let oc = open_out ics_path in
  output_string oc ics_content;
  close_out oc;
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs tmp_dir in
  let component, event = first_stored_event ~fs calendar_dir in
  let events = [ event ] in
  let from = Some (ptime_of (2025, 3, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 5, 31) (23, 59, 59) in
  let before = query_items events ~from ~to_ in
  Printf.printf "occurrences before override: %d\n" (List.length before);
  (* Create an override for the second occurrence (Apr 3) *)
  let occ_start = ptime_of (2025, 4, 3) (12, 0, 0) in
  let reference = occurrence_reference event occ_start in
  let override =
    Event.Recurrence.create_override ~now:(setup_fixed_date ()) event reference
      ~summary:(Patch.Set "Modified Meeting") ~location:(Patch.Set "Room B") ()
    |> Result.get_ok
  in
  let stored_after =
    Result.get_ok
    @@ Calendar_dir.add_occurrence_override ~fs calendar_dir component reference
         override
  in
  let events_after = [ Component.to_event stored_after |> Option.get ] in
  let after = query_items events_after ~from ~to_ in
  Printf.printf "occurrences after override: %d\n" (List.length after);
  (* The overridden occurrence should have the new summary *)
  let modified_occ =
    List.find_opt
      (fun e -> Query_item.summary e = Some "Modified Meeting")
      after
  in
  Printf.printf "override occurrence found: %b\n" (modified_occ <> None);
  (* Other occurrences should still have the original summary *)
  let original_occs =
    List.filter (fun e -> Query_item.summary e = Some "Weekly Meeting") after
  in
  Printf.printf "original occurrences remaining: %d\n"
    (List.length original_occs);
  Printf.printf "series preserved: %b\n" (List.length after = List.length before);
  (* Clean up *)
  remove_tree tmp_dir;
  [%expect
    {|
    occurrences before override: 10
    occurrences after override: 10
    override occurrence found: true
    original occurrences remaining: 9
    series preserved: true
    |}]

(* --- delete_occurrence with timezone-aware DTSTART --- *)

let%expect_test "two consecutive delete_occurrences with TZID" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _ = setup_fixed_date () in
  let tmp_dir = Filename.temp_dir "caledonia_test" "" in
  let cal_name = "test_cal" in
  let cal_path = Filename.concat tmp_dir cal_name in
  Sys.mkdir cal_path 0o755;
  let ics_content =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Test//EN";
        "BEGIN:VEVENT";
        "UID:two-deletes@caledonia.test";
        "DTSTAMP:20260312T000000Z";
        "DTSTART;TZID=Europe/London:20260312T060000";
        "DTEND;TZID=Europe/London:20260312T070000";
        "RRULE:FREQ=DAILY;COUNT=7";
        "SUMMARY:test";
        "END:VEVENT";
        "END:VCALENDAR";
        "";
      ]
  in
  let ics_path = Filename.concat cal_path "two-deletes.ics" in
  let oc = open_out ics_path in
  output_string oc ics_content;
  close_out oc;
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs tmp_dir in
  let component, event = first_stored_event ~fs calendar_dir in
  let events = [ event ] in
  let london = Timedesc.Time_zone.make_exn "Europe/London" in
  let from = Some (ptime_of (2026, 3, 1) (0, 0, 0)) in
  let to_ = ptime_of (2026, 3, 31) (23, 59, 59) in
  let before = query_items events ~from ~to_ in
  Printf.printf "before: %d\n" (List.length before);
  (* Delete the 15th — get_start returns UTC, London is GMT in March *)
  let occ_15 = Query_item.start (List.nth before 3) in
  let reference = occurrence_reference ~floating_tz:london event occ_15 in
  let _events =
    Result.get_ok
    @@ Calendar_dir.delete_occurrence ~fs calendar_dir component reference
  in
  (* Simulate server Refresh: re-read from disk *)
  let component, event = first_stored_event ~fs calendar_dir in
  let events = [ event ] in
  let mid = query_items events ~from ~to_ in
  Printf.printf "after first delete: %d\n" (List.length mid);
  (* Delete the 14th *)
  let occ_14 = Query_item.start (List.nth mid 2) in
  let reference = occurrence_reference ~floating_tz:london event occ_14 in
  let _events =
    Result.get_ok
    @@ Calendar_dir.delete_occurrence ~fs calendar_dir component reference
  in
  (* Re-read from disk again *)
  let events_final = Result.get_ok @@ get_events ~fs calendar_dir in
  let after = query_items events_final ~from ~to_ in
  Printf.printf "after second delete: %d\n" (List.length after);
  List.iter
    (fun e ->
      let (y, m, d), ((hh, mm, _ss), _) =
        Ptime.to_date_time (Query_item.start e)
      in
      Printf.printf "  %04d-%02d-%02d %02d:%02d\n" y m d hh mm)
    after;
  (* Clean up *)
  remove_tree tmp_dir;
  [%expect
    {|
    before: 7
    after first delete: 6
    after second delete: 5
      2026-03-12 06:00
      2026-03-13 06:00
      2026-03-16 06:00
      2026-03-17 06:00
      2026-03-18 06:00
    |}]

let%expect_test "delete_occurrence works with TZID dtstart" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _ = setup_fixed_date () in
  let tmp_dir = Filename.temp_dir "caledonia_test" "" in
  let cal_name = "test_cal" in
  let cal_path = Filename.concat tmp_dir cal_name in
  Sys.mkdir cal_path 0o755;
  let ics_content =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Test//EN";
        "BEGIN:VEVENT";
        "UID:tz-delete@caledonia.test";
        "DTSTAMP:20250327T000000Z";
        "DTSTART;TZID=Europe/London:20250327T120000";
        "DTEND;TZID=Europe/London:20250327T130000";
        "SUMMARY:TZ Weekly";
        "RRULE:FREQ=WEEKLY;COUNT=5";
        "END:VEVENT";
        "END:VCALENDAR";
        "";
      ]
  in
  let ics_path = Filename.concat cal_path "tz-delete.ics" in
  let oc = open_out ics_path in
  output_string oc ics_content;
  close_out oc;
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs tmp_dir in
  let component, event = first_stored_event ~fs calendar_dir in
  let events = [ event ] in
  let london = Timedesc.Time_zone.make_exn "Europe/London" in
  let from = Some (ptime_of (2025, 3, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 5, 31) (23, 59, 59) in
  let before = query_items events ~from ~to_ in
  Printf.printf "occurrences before: %d\n" (List.length before);
  (* Delete the second occurrence *)
  let second_occ = List.nth before 1 in
  let occ_start = Query_item.start second_occ in
  Printf.printf "deleting: %s\n" (Ptime.to_rfc3339 occ_start);
  let reference = occurrence_reference ~floating_tz:london event occ_start in
  let _events_after =
    Result.get_ok
    @@ Calendar_dir.delete_occurrence ~fs calendar_dir component reference
  in
  (* Re-read from disk to verify EXDATE is honored *)
  let events_disk = Result.get_ok @@ get_events ~fs calendar_dir in
  let after = query_items events_disk ~from ~to_ in
  Printf.printf "occurrences after (from disk): %d\n" (List.length after);
  Printf.printf "one fewer: %b\n" (List.length after = List.length before - 1);
  (* Clean up *)
  remove_tree tmp_dir;
  [%expect
    {|
    occurrences before: 5
    deleting: 2025-04-03T11:00:00-00:00
    occurrences after (from disk): 4
    one fewer: true
    |}]

(* --- delete_occurrence preserves existing overrides --- *)

let%expect_test "delete_occurrence preserves existing RECURRENCE-ID overrides" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _ = setup_fixed_date () in
  (* Set up a temp calendar dir with a recurring event + override *)
  let tmp_dir = Filename.temp_dir "caledonia_test" "" in
  let cal_name = "test_cal" in
  let cal_path = Filename.concat tmp_dir cal_name in
  Sys.mkdir cal_path 0o755;
  let ics_content =
    "BEGIN:VCALENDAR\r\n\
     VERSION:2.0\r\n\
     PRODID:-//Test//EN\r\n\
     BEGIN:VEVENT\r\n\
     UID:preserve-override@caledonia.test\r\n\
     DTSTAMP:20250327T000000Z\r\n\
     DTSTART:20250327T120000Z\r\n\
     DTEND:20250327T130000Z\r\n\
     SUMMARY:Weekly Meeting\r\n\
     RRULE:FREQ=WEEKLY\r\n\
     END:VEVENT\r\n\
     BEGIN:VEVENT\r\n\
     UID:preserve-override@caledonia.test\r\n\
     DTSTAMP:20250327T000000Z\r\n\
     DTSTART:20250403T140000Z\r\n\
     DTEND:20250403T150000Z\r\n\
     SUMMARY:Modified Meeting\r\n\
     RECURRENCE-ID:20250403T120000Z\r\n\
     END:VEVENT\r\n\
     END:VCALENDAR\r\n"
  in
  let ics_path = Filename.concat cal_path "preserve-override.ics" in
  let oc = open_out ics_path in
  output_string oc ics_content;
  close_out oc;
  let calendar_dir = Result.get_ok @@ Calendar_dir.create ~fs tmp_dir in
  let component, event = first_stored_event ~fs calendar_dir in
  let events = [ event ] in
  let from = Some (ptime_of (2025, 3, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 5, 31) (23, 59, 59) in
  let before = query_items events ~from ~to_ in
  Printf.printf "occurrences before: %d\n" (List.length before);
  let has_override_before =
    List.exists (fun e -> Query_item.summary e = Some "Modified Meeting") before
  in
  Printf.printf "has override before: %b\n" has_override_before;
  (* Delete a different occurrence (Apr 10) — should NOT remove the override *)
  let occ_to_delete = ptime_of (2025, 4, 10) (12, 0, 0) in
  let reference = occurrence_reference event occ_to_delete in
  let _events_after =
    Result.get_ok
    @@ Calendar_dir.delete_occurrence ~fs calendar_dir component reference
  in
  (* Re-read from disk to verify the file preserved the override *)
  let events_from_disk = Result.get_ok @@ get_events ~fs calendar_dir in
  let after = query_items events_from_disk ~from ~to_ in
  Printf.printf "occurrences after: %d\n" (List.length after);
  let has_override_after =
    List.exists (fun e -> Query_item.summary e = Some "Modified Meeting") after
  in
  Printf.printf "has override after: %b\n" has_override_after;
  (* Clean up *)
  remove_tree tmp_dir;
  [%expect
    {|
    occurrences before: 10
    has override before: true
    occurrences after: 9
    has override after: true
    |}]

(* --- Alarm wire-output round-trip --- *)

let%expect_test "alarm wire output uses short parseable format" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _ = setup_fixed_date () in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let components =
    Result.get_ok
    @@ Calendar_dir.get_calendar_components ~fs calendar_dir "alarm"
  in
  let events = List.filter_map Component.to_event components in
  let event =
    List.find (fun e -> Event.get_id e = "alarm-event@caledonia.test") events
  in
  let sexp = Sexp.event_wire_sexp event in
  (* Extract the alarms value from the sexp *)
  let alarm_str =
    match sexp with
    | Sexplib.Sexp.List fields -> (
        match
          List.find_opt
            (function
              | Sexplib.Sexp.List (Sexplib.Sexp.Atom "alarms" :: _) -> true
              | _ -> false)
            fields
        with
        | Some (Sexplib.Sexp.List [ _; Sexplib.Sexp.Atom s ]) -> s
        | _ -> "")
    | _ -> ""
  in
  Printf.printf "alarm wire value: %s\n" alarm_str;
  (* Source VALARM order is preserved; values use the compact format. *)
  Printf.printf "uses short format: %b\n" (not (String.contains alarm_str ' '));
  [%expect {|
    alarm wire value: 15m,1h
    uses short format: true
    |}]

(* --- Sexp protocol parsing --- *)

let%expect_test "delete_event_request parses with occurrence_start" =
  let sexp =
    Sexplib.Sexp.of_string
      {|((id "abc-123")(calendar_key "test")(file "abc.ics")(occurrence_start "2025-04-03T12:00:00Z"))|}
  in
  let req = Sexp.delete_event_request_of_sexp sexp in
  Printf.printf "id: %s\n" req.id;
  Printf.printf "occurrence_start: %s\n"
    (match req.occurrence_start with Some s -> s | None -> "none");
  [%expect {|
    id: abc-123
    occurrence_start: 2025-04-03T12:00:00Z |}]

let%expect_test "delete_event_request parses without occurrence_start" =
  let sexp =
    Sexplib.Sexp.of_string
      {|((id "abc-123")(calendar_key "test")(file "abc.ics"))|}
  in
  let req = Sexp.delete_event_request_of_sexp sexp in
  Printf.printf "id: %s\n" req.id;
  Printf.printf "occurrence_start: %s\n"
    (match req.occurrence_start with Some s -> s | None -> "none");
  [%expect {|
    id: abc-123
    occurrence_start: none |}]

let%expect_test "edit_event_request parses with occurrence_start" =
  let sexp =
    Sexplib.Sexp.of_string
      {|((id "abc-123")(calendar_key "test")(file "abc.ics")(summary (Set "New Title"))(occurrence_start "2025-04-03T12:00:00Z"))|}
  in
  let req = Sexp.edit_event_request_of_sexp sexp in
  Printf.printf "id: %s\n" req.id;
  Printf.printf "summary: %s\n"
    (match req.summary with
    | Sexp.Set s -> s
    | Sexp.Keep -> "keep"
    | Sexp.Clear -> "clear");
  Printf.printf "occurrence_start: %s\n"
    (match req.occurrence_start with Some s -> s | None -> "none");
  [%expect
    {|
    id: abc-123
    summary: New Title
    occurrence_start: 2025-04-03T12:00:00Z |}]

let%expect_test "DeleteEvent request parses with new record format" =
  let sexp =
    Sexplib.Sexp.of_string
      {|(DeleteEvent ((id "abc-123")(calendar_key "test")(file "abc.ics")))|}
  in
  let req = Sexp.request_of_sexp sexp in
  (match req with
  | Sexp.DeleteEvent r ->
      Printf.printf "id: %s\n" r.id;
      Printf.printf "occurrence_start: %s\n"
        (match r.occurrence_start with Some s -> s | None -> "none")
  | _ -> Printf.printf "wrong variant\n");
  [%expect {|
    id: abc-123
    occurrence_start: none |}]

let%expect_test "DeleteEvent request parses with occurrence_start" =
  let sexp =
    Sexplib.Sexp.of_string
      {|(DeleteEvent ((id "abc-123")(calendar_key "test")(file "abc.ics")(occurrence_start "2025-04-03T12:00:00Z")))|}
  in
  let req = Sexp.request_of_sexp sexp in
  (match req with
  | Sexp.DeleteEvent r ->
      Printf.printf "id: %s\n" r.id;
      Printf.printf "occurrence_start: %s\n"
        (match r.occurrence_start with Some s -> s | None -> "none")
  | _ -> Printf.printf "wrong variant\n");
  [%expect {|
    id: abc-123
    occurrence_start: 2025-04-03T12:00:00Z |}]

let%expect_test "protocol envelope rejects empty request ids" =
  let request =
    Sexplib.Sexp.of_string
      {|(Request ((version 1) (request_id "") (request Handshake)))|}
  in
  (match Sexp.parse_wire_request request with
  | Ok _ -> print_endline "accepted"
  | Error (request_id, error) ->
      Printf.printf "%s: %s: %s\n" request_id error.code error.message);
  [%expect
    {|
    unknown: invalid_request: request_id must be a non-empty string
    |}]

let%expect_test "protocol recurrence belongs to the selected VEVENT" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Protocol recurrence test//EN";
        "BEGIN:VTIMEZONE";
        "TZID:Example/Fixed";
        "BEGIN:STANDARD";
        "DTSTART:19700101T000000";
        "TZOFFSETFROM:+0000";
        "TZOFFSETTO:+0000";
        "RRULE:FREQ=YEARLY;BYMONTH=1";
        "END:STANDARD";
        "END:VTIMEZONE";
        "BEGIN:VEVENT";
        "UID:sibling";
        "DTSTAMP:20260101T000000Z";
        "DTSTART:20260701T100000Z";
        "RRULE:FREQ=DAILY;COUNT=2";
        "SUMMARY:Sibling";
        "END:VEVENT";
        "BEGIN:VEVENT";
        "UID:selected";
        "DTSTAMP:20260101T000000Z";
        "DTSTART:20260715T100000Z";
        "RRULE:FREQ=MONTHLY;BYMONTHDAY=15;COUNT=3";
        "SUMMARY:Selected";
        "END:VEVENT";
        "END:VCALENDAR";
        "";
      ]
  in
  let calendar = Result.get_ok (Icalendar.parse source) in
  let file = Eio.Path.(fs / "multi-component.ics") in
  let component_source =
    Component_source.of_decoded_document ~calendar_key:"test" ~file
      ~fingerprint:(Digest.string source |> Digest.to_hex)
      ()
  in
  let selected =
    Component.stored_views_of_decoded_components ~source:component_source
      (snd calendar)
    |> Result.get_ok
    |> List.find (fun component -> Component.get_id component = "selected")
  in
  let document =
    Calendar_codec.parse_document source
    |> Result.get_ok
    |> Calendar_document.decode ~source:component_source
    |> Result.get_ok
  in
  let response =
    Sexp.sexp_of_response_payload
      (Sexp.Events
         {
           events = [ Component_query.Stored selected ];
           occurrence_timezone = None;
           documents = [ document ];
         })
  in
  let recurrence =
    match response with
    | Sexplib.Sexp.List
        [
          Sexplib.Sexp.Atom "Events";
          Sexplib.Sexp.List [ Sexplib.Sexp.List fields ];
        ] ->
        List.find_map
          (function
            | Sexplib.Sexp.List
                [
                  Sexplib.Sexp.Atom "recurrence_value";
                  Sexplib.Sexp.List
                    [ Sexplib.Sexp.List [ Sexplib.Sexp.Atom "rrule"; value ] ];
                ] ->
                Some value
            | _ -> None)
          fields
    | _ -> None
  in
  (match recurrence with
  | Some (Sexplib.Sexp.Atom value) ->
      Printf.printf "selected=%b sibling=%b timezone=%b\n"
        (String.starts_with ~prefix:"FREQ=MONTHLY" value)
        (String.starts_with ~prefix:"FREQ=DAILY" value)
        (String.starts_with ~prefix:"FREQ=YEARLY" value)
  | _ -> print_endline "missing");
  let occurrence =
    Component.to_event selected
    |> Option.get
    |> Event.Recurrence.expand ~floating_tz:Timedesc.Time_zone.utc
         ~from:(Some (ptime_of (2026, 7, 15) (0, 0, 0)))
         ~to_:(ptime_of (2026, 7, 16) (0, 0, 0))
    |> Result.get_ok |> List.hd
  in
  let occurrence_response =
    Sexp.sexp_of_response_payload
      (Sexp.Events
         {
           events =
             [
               Component_query.Occurrence
                 { stored_series = selected; occurrence };
             ];
           occurrence_timezone = Some "UTC";
           documents = [ document ];
         })
  in
  let series_has_target =
    let nonempty name fields =
      List.exists
        (function
          | Sexplib.Sexp.List
              [ Sexplib.Sexp.Atom field_name; Sexplib.Sexp.Atom value ] ->
              String.equal field_name name && value <> ""
          | _ -> false)
        fields
    in
    match occurrence_response with
    | Sexplib.Sexp.List
        [
          Sexplib.Sexp.Atom "Events";
          Sexplib.Sexp.List [ Sexplib.Sexp.List occurrence_fields ];
        ] ->
        List.exists
          (function
            | Sexplib.Sexp.List
                [ Sexplib.Sexp.Atom "series_master"; Sexplib.Sexp.List fields ]
              ->
                nonempty "calendar_key" fields
                && nonempty "file" fields
                && nonempty "source_fingerprint" fields
            | _ -> false)
          occurrence_fields
    | _ -> false
  in
  Printf.printf "series-master-target=%b\n" series_has_target;
  [%expect
    {|
    selected=true sibling=false timezone=false
    series-master-target=true |}]
