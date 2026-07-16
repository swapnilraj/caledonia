open Cmdliner
open Caledonia_lib
open Event_args
open Component_args

let parse_categories cats =
  match cats with
  | None -> []
  | Some s -> String.split_on_char ',' s |> List.map String.trim

let run_event ~summary ~start_date ~start_time ~end_date ~end_time ~location
    ~description ~recur ~categories ~alarms ~calendar_name ?timezone
    ?end_timezone ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let now = Ptime_clock.now () in
  let* start = parse_start ~now ~start_date ~start_time ~timezone in
  let* start =
    match start with
    | Some s -> Ok s
    | None -> Error (`Msg "Start date required for events")
  in
  let* end_ =
    let end_date =
      match (end_date, end_time) with
      | None, Some _ -> start_date
      | _ -> end_date
    in
    let end_date =
      match (start_date, start_time, end_date, end_time) with
      | Some _, None, None, None -> start_date
      | _ -> end_date
    in
    let end_timezone =
      match (end_date, end_time, end_timezone) with
      | Some _, Some _, None -> timezone
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
  let categories = parse_categories categories in
  let categories_opt = match categories with [] -> None | cats -> Some cats in
  let* alarms = parse_alarms alarms in
  let recurrence_params =
    Option.map (fun (params, _, _) -> params) recurrence
  in
  let recurrence_date_until =
    match recurrence with Some (_, _, date) -> date | None -> None
  in
  let recurrence =
    Option.map (fun (_, recurrence, _) -> recurrence) recurrence
  in
  let* event =
    Event.create ~now ~summary ~start ?end_ ?location ?description
      ?categories:categories_opt ?recurrence ?recurrence_params
      ?recurrence_date_until ~alarms ()
  in
  let* _ =
    Calendar_dir.create_stored_component ~fs calendar_dir
      ~calendar_key:calendar_name
      (Component.event_body event)
    |> Command_common.storage_result
  in
  Output.print_terminal_stdout_line
    (Printf.sprintf "Event created with ID: %s" (Event.get_id event));
  Ok ()

let run_todo ~summary ~start_date ~start_time ~due_date ~due_time ~description
    ~duration ~categories ~priority ~percent ~status ~parent ~alarms
    ~calendar_name ?timezone ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let now = Ptime_clock.now () in
  let* () =
    if
      Option.is_some duration
      && (Option.is_some due_date || Option.is_some due_time)
    then Error (`Msg "Cannot combine --duration with --due or --due-time")
    else Ok ()
  in
  let start_timezone = if Option.is_some start_date then timezone else None in
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
  let categories = parse_categories categories in
  let* alarms = parse_alarms alarms in
  let* todo =
    Todo.create ~now ?summary ?start ?due ?duration ?description ~categories
      ?status ?priority ?percent ?parent ~alarms ()
  in
  let* _ =
    Calendar_dir.create_stored_component ~fs calendar_dir
      ~calendar_key:calendar_name (Component.todo_body todo)
    |> Command_common.storage_result
  in
  Output.print_terminal_stdout_line
    (Printf.sprintf "Todo created with ID: %s" (Todo.get_id todo));
  Ok ()

let run_journal ~summary ~start_date ~start_time ~description ~categories
    ~status ~calendar_name ?timezone ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let now = Ptime_clock.now () in
  let* start = parse_start ~now ~start_date ~start_time ~timezone in
  let categories = parse_categories categories in
  let* journal =
    Journal.create ~now ?summary ?start ?description ~categories ?status ()
  in
  let* _ =
    Calendar_dir.create_stored_component ~fs calendar_dir
      ~calendar_key:calendar_name
      (Component.journal_body journal)
    |> Command_common.storage_result
  in
  Output.print_terminal_stdout_line
    (Printf.sprintf "Journal created with ID: %s" (Journal.get_id journal));
  Ok ()

let run ~component_type ~summary ~start_date ~start_time ~end_date ~end_time
    ~location ~description ~recur ~categories ~due_date ~due_time ~priority
    ~duration ~percent ~status ~parent ~alarms ~calendar_name ?timezone
    ?end_timezone ~fs calendar_dir =
  let ( let* ) = Result.bind in
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
      ("alarm", alarms <> []);
    ]
  in
  let allowed =
    match component_type with
    | "event" ->
        [ "end-date"; "end-time"; "end-timezone"; "location"; "recur"; "alarm" ]
    | "todo" ->
        [
          "due";
          "due-time";
          "duration";
          "priority";
          "percent";
          "status";
          "parent";
          "alarm";
        ]
    | "journal" -> [ "status" ]
    | _ -> []
  in
  let* () = validate_component_options ~component_type ~allowed supplied in
  let* calendar_name =
    match Calendar_dir.resolve_calendar_key ~fs calendar_dir calendar_name with
    | Ok key -> Ok key
    | Error `Not_found ->
        Error
          (`Msg
             (Printf.sprintf
                "Unknown calendar %S; create its directory explicitly first"
                calendar_name))
    | Error (`Msg message) -> Error (`Msg message)
  in
  match component_type with
  | "event" ->
      run_event ~summary ~start_date ~start_time ~end_date ~end_time ~location
        ~description ~recur ~categories ~alarms ~calendar_name ?timezone
        ?end_timezone ~fs calendar_dir
  | "todo" ->
      run_todo ~summary:(Some summary) ~start_date ~start_time ~due_date
        ~due_time ~description ~duration ~categories ~priority ~percent ~status
        ~parent ~alarms ~calendar_name ?timezone ~fs calendar_dir
  | "journal" ->
      run_journal ~summary:(Some summary) ~start_date ~start_time ~description
        ~categories ~status ~calendar_name ?timezone ~fs calendar_dir
  | _ -> Error (`Msg "Invalid component type")

let cmd ~fs calendar_dir =
  let run component_type summary start_date start_time end_date end_time
      location description recur categories due_date due_time duration priority
      percent status parent alarms calendar_name timezone end_timezone () =
    match
      run ~component_type ~summary ~start_date ~start_time ~end_date ~end_time
        ~location ~description ~recur ~categories ~due_date ~due_time ~priority
        ~duration ~percent ~status ~parent ~alarms ~calendar_name ?timezone
        ?end_timezone ~fs calendar_dir
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
      const run $ component_type_arg $ required_summary_arg $ start_date_arg
      $ start_time_arg $ end_date_arg $ end_time_arg $ location_arg
      $ description_arg $ recur_arg $ categories_arg $ due_date_arg
      $ due_time_arg $ duration_arg $ priority_arg $ percent_arg $ status_arg
      $ parent_arg $ alarm_arg $ calendar_name_arg $ timezone_arg
      $ end_timezone_arg)
  in
  let doc = "Add a new calendar component (event, todo, or journal)" in
  let man =
    [
      `S Manpage.s_description;
      `P "Add a new event, todo, or journal entry to your calendar.";
      `P
        "Specify the component summary (title) as the first argument, and use \
         options to set other details. Use --type to specify the component \
         type.";
      `S Manpage.s_examples;
      `I
        ( "Add an event for today:",
          "caled add \"Meeting\" --date today --time 14:00" );
      `I
        ( "Add a todo with a due date:",
          "caled add --type todo \"Fix bug\" --due 2025-04-15 --priority 1" );
      `I
        ( "Add a todo with a duration:",
          "caled add --type todo \"Focus block\" --date 2025-04-15 --time \
           09:00 --duration 2h" );
      `I
        ( "Add a journal entry:",
          "caled add --type journal \"Daily notes\" --description \"...\" \
           --categories work,ideas" );
      `I
        ( "Add an event with location and description:",
          "caled add \"Lunch with Bob\" --date 2025-04-02 --time 12:30 \
           --location \"Pasta Restaurant\" --description \"Discuss project \
           plans\"" );
      `I
        ( "Add a todo with percent complete:",
          "caled add --type todo \"Write report\" --due 2025-05-01 --percent 50"
        );
      `S Manpage.s_options;
    ]
    @ date_format_manpage_entries @ recurrence_format_manpage_entries
    @ alarm_format_manpage_entries
    @ [ `S Manpage.s_see_also ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "add" ~doc ~man ~exits:exit_info in
  Cmd.v info term
