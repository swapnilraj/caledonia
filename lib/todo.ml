open Icalendar

type t = { props : todo_prop list; alarms : alarm list }

let get_id t =
  List.find_map (function `Uid (_, id) -> Some id | _ -> None) t.props
  |> Option.value ~default:""

let ( let* ) = Result.bind

let valid_status = function
  | `Needs_action | `Completed | `In_process | `Cancelled -> true
  | `Draft | `Final | `Tentative | `Confirmed -> false

let validate_status = function
  | Some status when not (valid_status status) ->
      Error
        (`Msg
           "VTODO status must be NEEDS-ACTION, IN-PROCESS, COMPLETED, or \
            CANCELLED")
  | _ -> Ok ()

let validate_priority = function
  | Some priority when priority < 0 || priority > 9 ->
      Error (`Msg "Priority must be between 0 and 9")
  | _ -> Ok ()

let validate_percent = function
  | Some percent when percent < 0 || percent > 100 ->
      Error (`Msg "Percent must be between 0 and 100")
  | _ -> Ok ()

let date_kind = function `Date _ -> `Date | `Datetime _ -> `Datetime

let validate_start_due_duration start due duration =
  let* () =
    match start with
    | None -> Ok ()
    | Some (params, value) ->
        Date.validate_date_or_datetime_params ~property:"VTODO DTSTART" params
          value
  in
  let* () =
    match due with
    | None -> Ok ()
    | Some (params, value) ->
        Date.validate_date_or_datetime_params ~property:"VTODO DUE" params value
  in
  let* () =
    match duration with
    | None -> Ok ()
    | Some (params, duration) ->
        Date.validate_duration_params ~property:"VTODO DURATION" params duration
  in
  let* () =
    match (due, duration) with
    | Some _, Some _ ->
        Error (`Msg "VTODO cannot contain both DUE and DURATION")
    | _ -> Ok ()
  in
  let* () =
    match duration with
    | Some (_, duration) when Ptime.Span.compare duration Ptime.Span.zero <= 0
      ->
        Error (`Msg "VTODO DURATION must be greater than zero")
    | Some _ when Option.is_none start ->
        Error (`Msg "VTODO DURATION requires DTSTART")
    | Some _ | None -> Ok ()
  in
  match (start, due) with
  | Some (_, start), Some (_, due) -> (
      if date_kind start <> date_kind due then
        Error (`Msg "VTODO DTSTART and DUE must use the same value type")
      else
        match
          Date.compare_ical_time ~floating_tz:Timedesc.Time_zone.utc start due
        with
        | Ok comparison when comparison >= 0 ->
            Error (`Msg "VTODO DUE must be later than DTSTART")
        | Ok _ -> Ok ()
        | Error error -> Error (`Msg (Date.string_of_conversion_error error)))
  | _ -> Ok ()

let status_of_props props =
  List.find_map
    (function `Status (_, status) -> Some status | _ -> None)
    props

let percent_value = function
  | `Percent (_, percent) -> Some percent
  | `Iana_prop (name, _, value)
    when String.equal (String.uppercase_ascii name) "PERCENT-COMPLETE" ->
      int_of_string_opt value
  | _ -> None

let is_percent_property = function
  | `Percent _ -> true
  | `Iana_prop (name, _, _value) ->
      String.equal (String.uppercase_ascii name) "PERCENT-COMPLETE"
  | _ -> false

let percent_of_props props = List.find_map percent_value props

let completed_of_props props =
  List.find_map
    (function `Completed (_, completed) -> Some completed | _ -> None)
    props

let apply_state ~now ~status_patch ~percent_patch props =
  let current_status = status_of_props props in
  let current_percent = percent_of_props props in
  let current_completed = completed_of_props props in
  let requested_status = Patch.apply status_patch ~current:current_status in
  let requested_percent = Patch.apply percent_patch ~current:current_percent in
  let* () = validate_status requested_status in
  let* () = validate_percent requested_percent in
  let* () =
    match (status_patch, percent_patch) with
    | Patch.Set `Completed, Patch.Set percent when percent <> 100 ->
        Error (`Msg "COMPLETED status conflicts with percent below 100")
    | Patch.Set status, Patch.Set 100 when status <> `Completed ->
        Error (`Msg "Percent 100 conflicts with a non-COMPLETED status")
    | Patch.Clear, Patch.Set 100 ->
        Error (`Msg "Percent 100 requires COMPLETED status")
    | _ -> Ok ()
  in
  let status, percent =
    match (status_patch, percent_patch) with
    | Patch.Set `Completed, Patch.Clear -> (Some `Completed, None)
    | Patch.Set `Completed, _ -> (Some `Completed, Some 100)
    | _, Patch.Set 100 -> (Some `Completed, Some 100)
    | (Patch.Set _ | Patch.Clear), Patch.Keep when current_percent = Some 100 ->
        (requested_status, None)
    | Patch.Keep, Patch.Set percent when current_status = Some `Completed ->
        let status = if percent = 0 then `Needs_action else `In_process in
        (Some status, Some percent)
    | _ -> (requested_status, requested_percent)
  in
  let completed =
    match (status_patch, percent_patch, status) with
    | Patch.Keep, Patch.Keep, _ -> current_completed
    | _, _, Some `Completed ->
        if status_patch = Patch.Set `Completed || percent_patch = Patch.Set 100
        then Some now
        else Some (Option.value ~default:now current_completed)
    | _, _, _ -> None
  in
  let props =
    List.filter
      (function
        | `Status _ | `Completed _ -> false
        | property when is_percent_property property -> false
        | _ -> true)
      props
  in
  let props =
    match status with
    | Some status -> `Status (Params.empty, status) :: props
    | None -> props
  in
  let props =
    match percent with
    | Some percent ->
        `Iana_prop ("PERCENT-COMPLETE", Params.empty, string_of_int percent)
        :: props
    | None -> props
  in
  let props =
    match completed with
    | Some completed -> `Completed (Params.empty, completed) :: props
    | None -> props
  in
  Ok props

let is_parent_property = function
  | `Related (params, _) | `Iana_prop ("RELATED", params, _) -> (
      match Params.find Reltype params with
      | Some `Parent | None -> true
      | Some (`Child | `Sibling | `Ianatoken _ | `Xname _) -> false)
  | _ -> false

let validate_singleton name values =
  Property_validation.validate_singleton ~component:Component_kind.Todo
    ~required:false name values

let validate_required_singleton name values =
  Property_validation.validate_singleton ~component:Component_kind.Todo
    ~required:true name values

let validate_todo_fields props alarms =
  let* () = Property_validation.validate_todo_properties props in
  let uids =
    List.filter_map (function `Uid (_, value) -> Some value | _ -> None) props
  in
  let timestamps =
    List.filter_map
      (function `Dtstamp (_, value) -> Some value | _ -> None)
      props
  in
  let statuses =
    List.filter_map
      (function `Status (_, value) -> Some value | _ -> None)
      props
  in
  let priorities =
    List.filter_map
      (function `Priority (_, value) -> Some value | _ -> None)
      props
  in
  let percent_properties = List.filter is_percent_property props in
  let percent_values = List.map percent_value percent_properties in
  let completed =
    List.filter_map
      (function `Completed (_, value) -> Some value | _ -> None)
      props
  in
  let starts =
    List.filter_map (function `Dtstart value -> Some value | _ -> None) props
  in
  let dues =
    List.filter_map (function `Due value -> Some value | _ -> None) props
  in
  let durations =
    List.filter_map (function `Duration value -> Some value | _ -> None) props
  in
  let parents = List.filter is_parent_property props in
  let recurrence_properties =
    List.filter
      (function
        | `Rrule _ | `Recur_id _ | `Rdate _ | `Exdate _ -> true | _ -> false)
      props
  in
  let singleton_properties =
    List.filter_map
      (function
        | `Class _ -> Some "CLASS"
        | `Created _ -> Some "CREATED"
        | `Description _ -> Some "DESCRIPTION"
        | `Geo _ -> Some "GEO"
        | `Lastmod _ -> Some "LAST-MODIFIED"
        | `Location _ -> Some "LOCATION"
        | `Organizer _ -> Some "ORGANIZER"
        | `Recur_id _ -> Some "RECURRENCE-ID"
        | `Rrule _ -> Some "RRULE"
        | `Seq _ -> Some "SEQUENCE"
        | `Summary _ -> Some "SUMMARY"
        | `Url _ -> Some "URL"
        | _ -> None)
      props
  in
  let rec validate_property_singletons seen = function
    | [] -> Ok ()
    | name :: _ when List.mem name seen ->
        Error (`Msg (Printf.sprintf "VTODO contains duplicate %s" name))
    | name :: rest -> validate_property_singletons (name :: seen) rest
  in
  let* () = validate_required_singleton "UID" uids in
  let* () = validate_required_singleton "DTSTAMP" timestamps in
  let* () =
    match uids with
    | [ uid ] when String.trim uid <> "" -> Ok ()
    | [ _ ] -> Error (`Msg "VTODO UID must not be empty")
    | _ -> assert false
  in
  let* () = validate_singleton "STATUS" statuses in
  let* () = validate_singleton "PRIORITY" priorities in
  let* () = validate_singleton "PERCENT-COMPLETE" percent_properties in
  let* () = validate_singleton "COMPLETED" completed in
  let* () = validate_singleton "DTSTART" starts in
  let* () = validate_singleton "DUE" dues in
  let* () = validate_singleton "DURATION" durations in
  let* () = validate_singleton "parent RELATED-TO" parents in
  let* () = validate_property_singletons [] singleton_properties in
  let* () =
    if recurrence_properties = [] then Ok ()
    else
      Error
        (`Msg
           "Recurring VTODO components are not supported; refusing to return \
            an incomplete schedule")
  in
  let* status =
    match statuses with
    | [] -> Ok None
    | [ value ] -> Ok (Some value)
    | _ -> assert false
  in
  let* priority =
    match priorities with
    | [] -> Ok None
    | [ value ] -> Ok (Some value)
    | _ -> assert false
  in
  let* percent =
    match percent_values with
    | [] -> Ok None
    | [ Some value ] -> Ok (Some value)
    | [ None ] -> Error (`Msg "VTODO PERCENT-COMPLETE must be an integer")
    | _ -> assert false
  in
  let start =
    match starts with [] -> None | [ value ] -> Some value | _ -> assert false
  in
  let due =
    match dues with [] -> None | [ value ] -> Some value | _ -> assert false
  in
  let duration =
    match durations with
    | [] -> None
    | [ value ] -> Some value
    | _ -> assert false
  in
  let* () = validate_status status in
  let* () = validate_priority priority in
  let* () = validate_percent percent in
  let* () = validate_start_due_duration start due duration in
  let* () =
    match (status, percent) with
    | Some `Completed, Some value when value <> 100 ->
        Error (`Msg "COMPLETED status conflicts with percent below 100")
    | Some status, Some 100 when status <> `Completed ->
        Error (`Msg "Percent 100 conflicts with a non-COMPLETED status")
    | None, Some 100 -> Error (`Msg "Percent 100 requires COMPLETED status")
    | _ -> Ok ()
  in
  let* () =
    match (completed, status) with
    | _ :: _, Some `Completed -> Ok ()
    | _ :: _, _ -> Error (`Msg "COMPLETED timestamp requires COMPLETED status")
    | [], _ -> Ok ()
  in
  let* () = Alarm.validate_all alarms in
  Alarm.validate_references ~has_start:(Option.is_some start)
    ~has_end:(Option.is_some due || Option.is_some duration)
    alarms

let with_updated_fields _t props alarms = { props; alarms }

let create ~now ?summary ?start ?due ?duration ?description ?categories ?status
    ?priority ?percent ?parent ?(alarms = []) () =
  let uuid = Fresh_id.generate () in
  let uid = (Params.empty, uuid) in
  let* () = validate_status status in
  let* () = validate_priority priority in
  let* () = validate_percent percent in
  let* () = validate_start_due_duration start due duration in
  let* () = Alarm.validate_all alarms in
  let* () =
    Alarm.validate_references ~has_start:(Option.is_some start)
      ~has_end:(Option.is_some due || Option.is_some duration)
      alarms
  in
  let* () =
    match parent with
    | Some parent when String.equal parent uuid ->
        Error (`Msg "A todo cannot be its own parent")
    | _ -> Ok ()
  in
  let props = [ `Dtstamp (Params.empty, now); `Uid uid ] in
  let props =
    match summary with
    | Some s -> `Summary (Params.empty, s) :: props
    | None -> props
  in
  let props =
    match start with Some s -> `Dtstart s :: props | None -> props
  in
  let props = match due with Some d -> `Due d :: props | None -> props in
  let props =
    match duration with Some d -> `Duration d :: props | None -> props
  in
  let props =
    match description with
    | Some d -> `Description (Params.empty, d) :: props
    | None -> props
  in
  let props =
    match categories with
    | Some cats -> `Categories (Params.empty, cats) :: props
    | None -> props
  in
  let props =
    match priority with
    | Some p -> `Priority (Params.empty, p) :: props
    | None -> props
  in
  let props =
    match parent with
    | Some parent_uid ->
        let params = Params.empty |> Params.add Reltype `Parent in
        `Related (params, parent_uid) :: props
    | None -> props
  in
  let* props =
    apply_state ~now
      ~status_patch:
        (Option.fold ~none:Patch.Keep
           ~some:(fun value -> Patch.Set value)
           status)
      ~percent_patch:
        (Option.fold ~none:Patch.Keep
           ~some:(fun value -> Patch.Set value)
           percent)
      props
  in
  let* () = validate_todo_fields props alarms in
  Ok { props; alarms }

let edit ~now ?(summary = Patch.Keep) ?(start = Patch.Keep) ?(due = Patch.Keep)
    ?(duration = Patch.Keep) ?(description = Patch.Keep)
    ?(categories = Patch.Keep) ?(status = Patch.Keep) ?(priority = Patch.Keep)
    ?(percent = Patch.Keep) ?(parent = Patch.Keep) ?(alarms = Patch.Keep) t =
  if
    summary = Patch.Keep && start = Patch.Keep && due = Patch.Keep
    && duration = Patch.Keep && description = Patch.Keep
    && categories = Patch.Keep && status = Patch.Keep && priority = Patch.Keep
    && percent = Patch.Keep && parent = Patch.Keep && alarms = Patch.Keep
  then Ok t
  else
    let* () =
      match alarms with
      | Patch.Set alarms -> Alarm.validate_all alarms
      | Patch.Keep | Patch.Clear -> Ok ()
    in
    let* () =
      validate_priority
        (match priority with
        | Patch.Set value -> Some value
        | Patch.Keep | Patch.Clear -> None)
    in
    let* () =
      match parent with
      | Patch.Set parent when String.equal parent (get_id t) ->
          Error (`Msg "A todo cannot be its own parent")
      | _ -> Ok ()
    in
    let props =
      List.filter (function `Dtstamp _ -> false | _ -> true) t.props
      |> fun props -> `Dtstamp (Params.empty, now) :: props
    in
    let props =
      props
      |> Patch.replace_in_list
           (function `Summary _ -> true | _ -> false)
           (fun value -> `Summary (Params.empty, value))
           summary
      |> Patch.replace_in_list
           (function `Dtstart _ -> true | _ -> false)
           (fun value -> `Dtstart value)
           start
      |> Patch.replace_in_list
           (function `Due _ -> true | _ -> false)
           (fun value -> `Due value)
           due
      |> Patch.replace_in_list
           (function `Duration _ -> true | _ -> false)
           (fun value -> `Duration value)
           duration
      |> Patch.replace_in_list
           (function `Description _ -> true | _ -> false)
           (fun value -> `Description (Params.empty, value))
           description
      |> Patch.replace_in_list
           (function `Categories _ -> true | _ -> false)
           (fun value -> `Categories (Params.empty, value))
           categories
      |> Patch.replace_in_list
           (function `Priority _ -> true | _ -> false)
           (fun value -> `Priority (Params.empty, value))
           priority
      |> Patch.replace_in_list is_parent_property
           (fun value ->
             let params = Params.empty |> Params.add Reltype `Parent in
             `Related (params, value))
           parent
    in
    let start_value =
      List.find_map (function `Dtstart value -> Some value | _ -> None) props
    in
    let due_value =
      List.find_map (function `Due value -> Some value | _ -> None) props
    in
    let duration_value =
      List.find_map (function `Duration value -> Some value | _ -> None) props
    in
    let* () =
      validate_start_due_duration start_value due_value duration_value
    in
    let* props =
      apply_state ~now ~status_patch:status ~percent_patch:percent props
    in
    let alarms =
      match alarms with
      | Patch.Keep -> t.alarms
      | Patch.Clear -> []
      | Patch.Set alarms -> alarms
    in
    let* () = Alarm.validate_all alarms in
    let* () =
      Alarm.validate_references
        ~has_start:(Option.is_some start_value)
        ~has_end:(Option.is_some due_value || Option.is_some duration_value)
        alarms
    in
    let* () = validate_todo_fields props alarms in
    Ok (with_updated_fields t props alarms)

let of_ical_body (props, alarms) =
  let* () = validate_todo_fields props alarms in
  Ok { props; alarms }

let to_ical_todo t = t.props

let get_summary t =
  List.find_map (function `Summary (_, s) -> Some s | _ -> None) t.props

let get_start_time t =
  List.find_map
    (function `Dtstart (_, value) -> Some value | _ -> None)
    t.props

let get_duration t =
  List.find_map
    (function `Duration (_, duration) -> Some duration | _ -> None)
    t.props

let get_due_time t =
  List.find_map (function `Due (_, value) -> Some value | _ -> None) t.props

let resolve_time ~floating_tz = function
  | None -> Ok None
  | Some value ->
      Result.map Option.some (Date.ptime_of_ical_result ~floating_tz value)

let get_start_result ~floating_tz t =
  resolve_time ~floating_tz (get_start_time t)

let get_due_result ~floating_tz t = resolve_time ~floating_tz (get_due_time t)

let get_description t =
  List.find_map (function `Description (_, d) -> Some d | _ -> None) t.props

let get_categories t =
  List.filter_map
    (function `Categories (_, cats) -> Some cats | _ -> None)
    t.props
  |> List.flatten

let get_status t =
  List.find_map (function `Status (_, s) -> Some s | _ -> None) t.props

let get_priority t =
  List.find_map (function `Priority (_, p) -> Some p | _ -> None) t.props

let get_percent t = List.find_map percent_value t.props

let get_completed t =
  List.find_map (function `Completed (_, t) -> Some t | _ -> None) t.props

let get_alarms t = t.alarms

let get_related_parent t =
  List.find_map
    (function
      | `Related (params, uid) -> (
          match Icalendar.Params.find Reltype params with
          | Some `Parent | None -> Some uid
          | _ -> None)
      | `Iana_prop ("RELATED", params, uid) -> (
          match Icalendar.Params.find Reltype params with
          | Some `Parent | None -> Some uid
          | _ -> None)
      | _ -> None)
    t.props

let is_completed t =
  match get_status t with Some `Completed -> true | _ -> false

let is_overdue_at ~now ~tz t =
  if is_completed t then Ok false
  else
    match get_due_time t with
    | None -> Ok false
    | Some (`Date _ as due_time) ->
        let* due_start = Date.ptime_of_ical_result ~floating_tz:tz due_time in
        let* due_end = Date.add_days_result ~tz due_start 1 in
        Ok (Ptime.compare now due_end >= 0)
    | Some due_time ->
        let* due = Date.ptime_of_ical_result ~floating_tz:tz due_time in
        Ok (Ptime.compare now due > 0)

type todo_tree = { todo : t; children : todo_tree list }

let validate_parent_graph todos =
  let todo_map = Hashtbl.create (List.length todos) in
  let rec index = function
    | [] -> Ok ()
    | todo :: rest ->
        let id = get_id todo in
        if id = "" then Error (`Msg "A todo in the parent graph has no UID")
        else if Hashtbl.mem todo_map id then
          Error
            (`Msg (Printf.sprintf "Duplicate todo UID %S in parent graph" id))
        else (
          Hashtbl.add todo_map id todo;
          index rest)
  in
  let* () = index todos in
  let rec visit trail todo =
    let id = get_id todo in
    if List.mem id trail then
      Error
        (`Msg
           (Printf.sprintf "Todo parent cycle detected: %s"
              (String.concat " -> " (List.rev (id :: trail)))))
    else
      match get_related_parent todo with
      | None -> Ok ()
      | Some parent_id when String.equal parent_id id ->
          Error (`Msg (Printf.sprintf "Todo %S is its own parent" id))
      | Some parent_id -> (
          match Hashtbl.find_opt todo_map parent_id with
          | None ->
              Error
                (`Msg
                   (Printf.sprintf "Todo %S refers to missing parent %S" id
                      parent_id))
          | Some parent -> visit (id :: trail) parent)
  in
  let rec validate = function
    | [] -> Ok ()
    | todo :: rest ->
        let* () = visit [] todo in
        validate rest
  in
  validate todos

let get_ancestors ~all_todos todo =
  let* () = validate_parent_graph all_todos in
  let todo_map = Hashtbl.create (List.length all_todos) in
  List.iter (fun item -> Hashtbl.add todo_map (get_id item) item) all_todos;
  let rec collect acc current =
    match get_related_parent current with
    | None -> Ok (List.rev acc)
    | Some parent_id ->
        let parent = Hashtbl.find todo_map parent_id in
        collect (parent :: acc) parent
  in
  collect [] todo

let expand_with_ancestors ~all_todos ~filtered_todos =
  let* () = validate_parent_graph all_todos in
  let* ancestor_lists =
    let rec collect acc = function
      | [] -> Ok (List.rev acc)
      | todo :: rest ->
          let* ancestors = get_ancestors ~all_todos todo in
          collect (ancestors :: acc) rest
    in
    collect [] filtered_todos
  in
  let ancestors = List.concat ancestor_lists in
  let selected_ids = Hashtbl.create 100 in
  List.iter
    (fun todo -> Hashtbl.replace selected_ids (get_id todo) ())
    filtered_todos;
  List.iter
    (fun todo -> Hashtbl.replace selected_ids (get_id todo) ())
    ancestors;
  Ok
    (List.filter (fun todo -> Hashtbl.mem selected_ids (get_id todo)) all_todos)

let build_todo_tree_unchecked todos =
  let todo_map =
    List.fold_left (fun acc todo -> (get_id todo, todo) :: acc) [] todos
    |> List.to_seq |> Hashtbl.of_seq
  in
  let rec build_tree visited todo =
    let id = get_id todo in
    if List.mem id visited then { todo; children = [] }
    else
      let visited = id :: visited in
      let children =
        List.filter_map
          (fun t ->
            match get_related_parent t with
            | Some parent_id when parent_id = id -> Some (build_tree visited t)
            | _ -> None)
          todos
      in
      { todo; children }
  in
  List.filter_map
    (fun todo ->
      match get_related_parent todo with
      | None -> Some (build_tree [] todo)
      | Some parent_id ->
          if Hashtbl.mem todo_map parent_id then None
          else Some (build_tree [] todo))
    todos

let build_todo_tree todos =
  let* () = validate_parent_graph todos in
  Ok (build_todo_tree_unchecked todos)

let compute_alarm_fire_time_result ~floating_tz todo alarm =
  let conversion_result result =
    Result.map_error
      (fun error -> `Msg (Date.string_of_conversion_error error))
      result
  in
  let required label result =
    let* value = conversion_result result in
    match value with
    | Some value -> Ok value
    | None -> Error (`Msg (label ^ "-relative VALARM has no reference value"))
  in
  let end_reference () =
    let* due = conversion_result (get_due_result ~floating_tz todo) in
    match due with
    | Some due -> Ok due
    | None -> (
        let* start = required "END" (get_start_result ~floating_tz todo) in
        match get_duration todo with
        | None -> Error (`Msg "END-relative VALARM requires DUE or DURATION")
        | Some duration -> (
            match Ptime.add_span start duration with
            | Some end_ -> Ok end_
            | None -> Error (`Msg "VTODO end is out of range")))
  in
  match Alarm.trigger alarm with
  | params, `Duration span ->
      let* reference =
        match Params.find Related params with
        | Some `End -> Result.map Option.some (end_reference ())
        | Some `Start | None ->
            Result.map Option.some
              (required "START" (get_start_result ~floating_tz todo))
      in
      Ok (Option.bind reference (fun instant -> Ptime.add_span instant span))
  | _, `Datetime instant -> Ok (Some instant)

let compute_alarm_fires_result ~floating_tz ~from ~to_ todo =
  let max_fires = 100_000 in
  let initial : (t Alarm.fire list * int, [ `Msg of string ]) result =
    Ok ([], 0)
  in
  List.mapi (fun alarm_index alarm -> (alarm_index, alarm)) (get_alarms todo)
  |> List.fold_left
       (fun result (alarm_index, alarm) ->
         let* fires, count = result in
         let* fire_time =
           compute_alarm_fire_time_result ~floating_tz todo alarm
         in
         match fire_time with
         | None -> Ok (fires, count)
         | Some fire_time ->
             let* fire_times =
               Alarm.repeated_instants ~max_repetitions:max_fires fire_time
                 alarm
             in
             List.fold_left
               (fun result fire_time ->
                 let* fires, count = result in
                 let after_from =
                   match from with
                   | None -> true
                   | Some f -> Ptime.compare fire_time f >= 0
                 in
                 let before_to = Ptime.compare fire_time to_ < 0 in
                 if not (after_from && before_to) then Ok (fires, count)
                 else if count >= max_fires then
                   Error
                     (`Msg
                        (Printf.sprintf
                           "alarm expansion exceeded the %d-fire safety limit"
                           max_fires))
                 else
                   Ok
                     ( { Alarm.fire_time; owner = todo; alarm; alarm_index }
                       :: fires,
                       count + 1 ))
               (Ok (fires, count))
               fire_times)
       initial
  |> Result.map (fun (fires, _) -> List.rev fires)
