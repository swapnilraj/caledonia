open Cmdliner
open Caledonia_lib
open Event_args
open Component_args

let parse_status = Component_status.of_string
let clear_requested clear_fields field = List.mem field clear_fields

let patch_of_option ~clear_fields field value =
  match (clear_requested clear_fields field, value) with
  | true, Some _ ->
      Error
        (`Msg (Printf.sprintf "Cannot set and clear %s in the same edit" field))
  | true, None -> Ok Patch.Clear
  | false, Some value -> Ok (Patch.Set value)
  | false, None -> Ok Patch.Keep

let check_clear_fields ~allowed clear_fields =
  match
    List.find_opt (fun field -> not (List.mem field allowed)) clear_fields
  with
  | None -> Ok ()
  | Some field ->
      Error
        (`Msg
           (Printf.sprintf "Field %s cannot be cleared on this component type"
              field))

let conversion_error error = `Msg (Date.string_of_conversion_error error)

let timezone_or ~fallback = function
  | None -> Ok fallback
  | Some tzid -> (
      match Timedesc.Time_zone.make tzid with
      | Some timezone -> Ok timezone
      | None -> Error (`Msg (Printf.sprintf "Unknown timezone %S" tzid)))

let local_date_string ~timezone instant =
  let ( let* ) = Result.bind in
  let* local =
    Date.ptime_to_timedesc_result ~tz:timezone instant
    |> Result.map_error conversion_error
  in
  Ok
    (Printf.sprintf "%04d-%02d-%02d" (Timedesc.year local)
       (Timedesc.month local) (Timedesc.day local))

let run ~component_id ~summary ~start_date ~start_time ~end_date ~end_time
    ~location ~description ~recur ~categories ~due_date ~due_time ~priority
    ~duration ~percent ~status ~parent ~no_parent ~alarms ~no_alarms
    ~clear_fields ?timezone ?end_timezone ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let now = Ptime_clock.now () in
  let floating_tz = Date.local_timezone () in
  let* components = Calendar_dir.get_components ~fs calendar_dir in
  let* component =
    Command_common.find_unique_component ~id:component_id components
  in
  let component_type = Component.component_type component in
  let supplied =
    [
      ("end-date", Option.is_some end_date);
      ("end-time", Option.is_some end_time);
      ("end-timezone", Option.is_some end_timezone);
      ("location", Option.is_some location);
      ("recur", Option.is_some recur);
      ("due", Option.is_some due_date);
      ("due-time", Option.is_some due_time);
      ("duration", Option.is_some duration);
      ("priority", Option.is_some priority);
      ("percent", Option.is_some percent);
      ("status", Option.is_some status);
      ("parent", Option.is_some parent);
      ("no-parent", no_parent);
      ("alarm", alarms <> []);
      ("no-alarms", no_alarms);
    ]
  in
  let component_type_name, allowed =
    match component_type with
    | Component_kind.Event ->
        ( "event",
          [
            "end-date";
            "end-time";
            "end-timezone";
            "location";
            "recur";
            "alarm";
            "no-alarms";
          ] )
    | Component_kind.Todo ->
        ( "todo",
          [
            "due";
            "due-time";
            "duration";
            "priority";
            "percent";
            "status";
            "parent";
            "no-parent";
            "alarm";
            "no-alarms";
          ] )
    | Component_kind.Journal -> ("journal", [ "status" ])
  in
  let* () =
    validate_component_options ~component_type:component_type_name ~allowed
      supplied
  in
  match component_type with
  | Component_kind.Event ->
      let* () =
        check_clear_fields
          ~allowed:
            [
              "summary";
              "start";
              "end";
              "location";
              "description";
              "categories";
              "recurrence";
              "alarms";
            ]
          clear_fields
      in
      let* e =
        match Component.to_event component with
        | Some e -> Ok e
        | None -> Error (`Msg "Failed to extract event from component")
      in
      let start_date =
        match (start_date, start_time) with
        | None, Some _ ->
            let* start =
              Event.get_start_result ~floating_tz e
              |> Result.map_error conversion_error
            in
            let* timezone =
              timezone_or ~fallback:floating_tz (Event.get_start_timezone e)
            in
            Result.map Option.some (local_date_string ~timezone start)
        | _ -> Ok start_date
      in
      let* start_date = start_date in
      let timezone =
        match (timezone, start_date, start_time) with
        | None, _, Some _ -> Event.get_start_timezone e
        | _ -> timezone
      in
      let* start = parse_start ~now ~start_date ~start_time ~timezone in
      let* end_ =
        let end_date =
          match (end_date, end_time) with
          | None, Some _ ->
              let* end_ =
                Event.get_end_result ~floating_tz e
                |> Result.map_error conversion_error
              in
              let* fallback =
                match end_ with
                | Some end_ -> Ok end_
                | None ->
                    Event.get_start_result ~floating_tz e
                    |> Result.map_error conversion_error
              in
              let* timezone =
                timezone_or ~fallback:floating_tz (Event.get_end_timezone e)
              in
              Result.map Option.some (local_date_string ~timezone fallback)
          | _ -> Ok end_date
        in
        let* end_date = end_date in
        let end_timezone =
          match (end_date, end_time, end_timezone) with
          | _, Some _, None -> (
              match Event.get_end_timezone e with
              | Some _ as tz -> tz
              | None -> timezone)
          | _ -> end_timezone
        in
        parse_end ~now ~end_date ~end_time ~end_timezone
      in
      let* recurrence =
        match recur with
        | Some r ->
            let* p = parse_recurrence r in
            Ok (Some p)
        | None -> Ok None
      in
      let categories =
        match categories with
        | Some s -> Some (String.split_on_char ',' s |> List.map String.trim)
        | None -> None
      in
      let* alarms_patch =
        let clear = no_alarms || clear_requested clear_fields "alarms" in
        if clear && alarms <> [] then
          Error (`Msg "Cannot set and clear alarms in the same edit")
        else if clear then Ok Patch.Clear
        else
          match alarms with
          | [] -> Ok Patch.Keep
          | strs ->
              let* alarms = parse_alarms strs in
              Ok (Patch.Set alarms)
      in
      let* summary_patch = patch_of_option ~clear_fields "summary" summary in
      let* start_patch = patch_of_option ~clear_fields "start" start in
      let* end_patch = patch_of_option ~clear_fields "end" end_ in
      let* location_patch = patch_of_option ~clear_fields "location" location in
      let* description_patch =
        patch_of_option ~clear_fields "description" description
      in
      let* categories_patch =
        patch_of_option ~clear_fields "categories" categories
      in
      let* recurrence_patch =
        patch_of_option ~clear_fields "recurrence" recurrence
      in
      let recurrence_params =
        match recurrence_patch with
        | Patch.Set (params, _, _) -> Some params
        | _ -> None
      in
      let recurrence_date_until =
        match recurrence_patch with
        | Patch.Set (_, _, date_until) -> date_until
        | Patch.Keep | Patch.Clear -> None
      in
      let recurrence_patch =
        match recurrence_patch with
        | Patch.Keep -> Patch.Keep
        | Patch.Clear -> Patch.Clear
        | Patch.Set (_, recurrence, _) -> Patch.Set recurrence
      in
      let* modified =
        Event.edit_patch ~now ~summary:summary_patch ~start:start_patch
          ~end_:end_patch ~location:location_patch
          ~description:description_patch ~categories:categories_patch
          ~recurrence:recurrence_patch ?recurrence_params ?recurrence_date_until
          ~alarms:alarms_patch e
      in
      let* _ =
        Calendar_dir.replace_stored_component ~fs calendar_dir
          ~original:component
          ~replacement:(Component.event_body modified)
        |> Command_common.storage_result
      in
      Output.print_terminal_stdout_line
        (Printf.sprintf "Event %s updated." component_id);
      Ok ()
  | Component_kind.Todo ->
      let* () =
        check_clear_fields
          ~allowed:
            [
              "summary";
              "start";
              "due";
              "duration";
              "description";
              "categories";
              "status";
              "priority";
              "percent";
              "parent";
              "alarms";
            ]
          clear_fields
      in
      let* t =
        match Component.to_todo component with
        | Some t -> Ok t
        | None -> Error (`Msg "Failed to extract todo from component")
      in
      let start_timezone =
        if Option.is_some start_date then timezone else None
      in
      let due_timezone = if Option.is_some due_date then timezone else None in
      let* start =
        parse_start ~now ~start_date ~start_time ~timezone:start_timezone
      in
      let* due =
        parse_start ~now ~start_date:due_date ~start_time:due_time
          ~timezone:due_timezone
      in
      let* duration =
        match duration with
        | None -> Ok None
        | Some value ->
            Result.map
              (fun span -> Some (Icalendar.Params.empty, span))
              (parse_duration value)
      in
      let categories =
        match categories with
        | Some s -> Some (String.split_on_char ',' s |> List.map String.trim)
        | None -> None
      in
      let* status =
        match status with
        | Some s ->
            let* st = parse_status s in
            Ok (Some st)
        | None -> Ok None
      in
      let parent_patch =
        let clear = no_parent || clear_requested clear_fields "parent" in
        match (clear, parent) with
        | true, Some _ ->
            Error (`Msg "Cannot set and clear parent in the same edit")
        | true, None -> Ok Patch.Clear
        | false, Some parent -> Ok (Patch.Set parent)
        | false, None -> Ok Patch.Keep
      in
      let* parent_patch = parent_patch in
      let* alarms_patch =
        let clear = no_alarms || clear_requested clear_fields "alarms" in
        if clear && alarms <> [] then
          Error (`Msg "Cannot set and clear alarms in the same edit")
        else if clear then Ok Patch.Clear
        else
          match alarms with
          | [] -> Ok Patch.Keep
          | strs ->
              let* alarms = parse_alarms strs in
              Ok (Patch.Set alarms)
      in
      let* summary_patch = patch_of_option ~clear_fields "summary" summary in
      let* start_patch = patch_of_option ~clear_fields "start" start in
      let* due_patch = patch_of_option ~clear_fields "due" due in
      let* duration_patch = patch_of_option ~clear_fields "duration" duration in
      let* () =
        match (due_patch, duration_patch) with
        | Patch.Set _, Patch.Set _ ->
            Error (`Msg "Cannot combine --duration with --due or --due-time")
        | _ -> Ok ()
      in
      let* description_patch =
        patch_of_option ~clear_fields "description" description
      in
      let* categories_patch =
        patch_of_option ~clear_fields "categories" categories
      in
      let* status_patch = patch_of_option ~clear_fields "status" status in
      let* priority_patch = patch_of_option ~clear_fields "priority" priority in
      let* percent_patch = patch_of_option ~clear_fields "percent" percent in
      let* modified =
        Todo.edit ~now ~summary:summary_patch ~start:start_patch ~due:due_patch
          ~duration:duration_patch ~description:description_patch
          ~categories:categories_patch ~status:status_patch
          ~priority:priority_patch ~percent:percent_patch ~parent:parent_patch
          ~alarms:alarms_patch t
      in
      let* _ =
        Calendar_dir.replace_stored_component ~fs calendar_dir
          ~original:component
          ~replacement:(Component.todo_body modified)
        |> Command_common.storage_result
      in
      Output.print_terminal_stdout_line
        (Printf.sprintf "Todo %s updated." component_id);
      Ok ()
  | Component_kind.Journal ->
      let* () =
        check_clear_fields
          ~allowed:[ "summary"; "start"; "description"; "categories"; "status" ]
          clear_fields
      in
      let* j =
        match Component.to_journal component with
        | Some j -> Ok j
        | None -> Error (`Msg "Failed to extract journal from component")
      in
      let* start = parse_start ~now ~start_date ~start_time ~timezone in
      let categories =
        match categories with
        | Some s -> Some (String.split_on_char ',' s |> List.map String.trim)
        | None -> None
      in
      let* status =
        match status with
        | Some s ->
            let* st = parse_status s in
            Ok (Some st)
        | None -> Ok None
      in
      let* summary_patch = patch_of_option ~clear_fields "summary" summary in
      let* start_patch = patch_of_option ~clear_fields "start" start in
      let* description_patch =
        patch_of_option ~clear_fields "description" description
      in
      let* categories_patch =
        patch_of_option ~clear_fields "categories" categories
      in
      let* status_patch = patch_of_option ~clear_fields "status" status in
      let* modified =
        Journal.edit ~now ~summary:summary_patch ~start:start_patch
          ~description:description_patch ~categories:categories_patch
          ~status:status_patch j
      in
      let* _ =
        Calendar_dir.replace_stored_component ~fs calendar_dir
          ~original:component
          ~replacement:(Component.journal_body modified)
        |> Command_common.storage_result
      in
      Output.print_terminal_stdout_line
        (Printf.sprintf "Journal %s updated." component_id);
      Ok ()

let component_id_arg =
  let doc = "ID of the component to edit" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"ID" ~doc)

let status_arg =
  let doc =
    "Status (for todos: completed, needs-action, in-process, cancelled; for \
     journals: draft, final, cancelled)"
  in
  Arg.(value & opt (some string) None & info [ "status" ] ~docv:"STATUS" ~doc)

let cmd ~fs calendar_dir =
  let run component_id summary start_date start_time end_date end_time location
      description recur categories due_date due_time duration priority percent
      status parent no_parent alarms no_alarms clear_fields timezone
      end_timezone () =
    match
      run ~component_id ~summary ~start_date ~start_time ~end_date ~end_time
        ~location ~description ~recur ~categories ~due_date ~due_time ~priority
        ~duration ~percent ~status ~parent ~no_parent ~alarms ~no_alarms
        ~clear_fields ?timezone ?end_timezone ~fs calendar_dir
    with
    | Error (`Msg msg) ->
        Output.print_error "Error" msg;
        1
    | Error (`Conflict msg) ->
        Output.print_error "Conflict" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(
      const run $ component_id_arg $ optional_summary_arg $ start_date_arg
      $ start_time_arg $ end_date_arg $ end_time_arg $ location_arg
      $ description_arg $ recur_arg $ categories_arg $ due_date_arg
      $ due_time_arg $ duration_arg $ priority_arg $ percent_arg $ status_arg
      $ parent_arg $ no_parent_flag $ alarm_arg $ no_alarms_flag
      $ clear_fields_arg $ timezone_arg $ end_timezone_arg)
  in
  let doc = "Edit an existing calendar component" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Edit an existing component (event, todo, or journal) in your calendar \
         by its ID.";
      `P
        "Specify the component ID as the first argument, and use options to \
         change details.";
      `S Manpage.s_examples;
      `I ("Change the summary:", "caled edit <id> --summary \"New Title\"");
      `I ("Mark a todo as completed:", "caled edit <id> --status completed");
      `I
        ( "Set todo priority and percent:",
          "caled edit <id> --priority 1 --percent 50" );
      `I
        ( "Replace a todo due date with a duration:",
          "caled edit <id> --clear due --duration 2h" );
      `I ("Update categories:", "caled edit <id> --categories work,urgent");
      `I ("Clear an optional field:", "caled edit <id> --clear description");
      `S Manpage.s_options;
    ]
    @ date_format_manpage_entries @ recurrence_format_manpage_entries
    @ alarm_format_manpage_entries
    @ [ `S Manpage.s_see_also ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "edit" ~doc ~man ~exits:exit_info in
  Cmd.v info term
