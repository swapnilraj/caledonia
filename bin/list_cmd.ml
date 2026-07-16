open Cmdliner
open Caledonia_lib
open Query_args

let run ?from_str ?to_str ~calendar:calendars ?count ~format ~today ~tomorrow
    ~week ~month ?timezone ~sort ~color ~component_type ~incomplete ~overdue ~fs
    calendar_dir =
  let ( let* ) = Result.bind in
  let now = Ptime_clock.now () in
  let* tz = Query_args.parse_timezone_result ~timezone in
  let* () =
    if
      (incomplete || overdue) && component_type <> "all"
      && component_type <> "todo"
    then
      Error
        (`Msg
           "--incomplete and --overdue are todo-only filters; use --type todo \
            or omit --type")
    else Ok ()
  in
  let* scope =
    resolve_temporal_scope ~tz ~now ~from_str ~to_str ~today ~tomorrow ~week
      ~month ~default:`One_month
  in
  let from, to_ =
    match scope with
    | Bounded { from; to_ } -> (from, to_)
    | Unbounded -> assert false
  in
  let* documents = Calendar_dir.get_documents ~fs calendar_dir in
  let components = List.concat_map Calendar_document.components documents in

  let component_types =
    match component_type with
    | "all" -> []
    | "event" -> [ Component_kind.Event ]
    | "todo" -> [ Component_kind.Todo ]
    | "journal" -> [ Component_kind.Journal ]
    | _ -> assert false
  in
  let criteria =
    Component_query.
      {
        no_criteria with
        calendars;
        component_types;
        completed = (if incomplete then Some false else None);
        overdue = (if overdue then Some true else None);
      }
  in
  let include_undated_todos =
    (component_type = "todo" || incomplete || overdue)
    && from_str = None && to_str = None && (not today) && (not tomorrow)
    && (not week) && not month
  in
  let* items =
    Component_query.run ~timezone:tz ~now ~from ~to_ ~include_undated_todos
      ~sort:(create_component_query_sort sort)
      ?limit:count ~criteria components
  in
  let get_color calendar_key =
    Calendar_dir.get_color ~fs calendar_dir calendar_key
  in
  let* () =
    match format with
    | `Text | `Entries -> Output.validate_human_items ~tz items
    | `Json | `Csv | `Ics | `Sexp -> Ok ()
  in
  match (items, format) with
  | [], (`Text | `Entries) ->
      print_endline "No components found.";
      Ok ()
  | _ -> Output.print_items ~documents ~format ~tz ~now ~get_color ~color items

let incomplete_arg =
  let doc = "Show only incomplete todos" in
  Arg.(value & flag & info [ "incomplete" ] ~doc)

let overdue_arg =
  let doc = "Show only overdue todos" in
  Arg.(value & flag & info [ "overdue" ] ~doc)

let component_type_filter_arg =
  let doc = "Filter by component type (all, event, todo, journal)" in
  let comp_type_enum = [ "all"; "event"; "todo"; "journal" ] in
  Arg.(
    value
    & opt (enum (List.map (fun s -> (s, s)) comp_type_enum)) "all"
    & info [ "type" ] ~docv:"TYPE" ~doc)

let cmd ~fs calendar_dir =
  let run from_str to_str calendars count format today tomorrow week month
      timezone sort color component_type incomplete overdue () =
    match
      run ?from_str ?to_str ~calendar:calendars ?count ~format ~today ~tomorrow
        ~week ~month ?timezone ~sort ~color ~component_type ~incomplete ~overdue
        ~fs calendar_dir
    with
    | Error (`Msg msg) ->
        Output.print_error "Error" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(
      const run $ from_arg $ to_arg $ calendar_arg $ count_arg $ format_arg
      $ today_arg $ tomorrow_arg $ week_arg $ month_arg $ timezone_arg
      $ sort_arg $ color_arg $ component_type_filter_arg $ incomplete_arg
      $ overdue_arg)
  in
  let doc = "List calendar components (events, todos, journals)" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "List calendar components within a specified date range. By default, \
         all components from today to one month from today are shown. You can \
         use date flags to show components for a specific time period, filter \
         by type, and sort with the --sort option.";
      `P
        "For todos, temporal filtering uses DTSTART when present and otherwise \
         DUE. Undated todos are also included when --type todo is used without \
         an explicit date selector.";
      `S Manpage.s_examples;
      `I ("List all events for today:", "caled list --today --type event");
      `I
        ( "List todos in the default window (including undated):",
          "caled list --type todo" );
      `I ("List incomplete todos:", "caled list --type todo --incomplete");
      `I ("List overdue todos:", "caled list --type todo --overdue");
      `I
        ( "List journal entries for the month:",
          "caled list --type journal --month" );
      `I ("List all components for the current week:", "caled list --week");
      `I
        ( "List components within a specific date range:",
          "caled list --from 2025-03-27 --to 2025-04-01" );
      `I
        ( "List components from a specific calendar:",
          "caled list --calendar work" );
      `I ("List components in JSON format:", "caled list --format json");
      `I ("Limit the number of components shown:", "caled list --count 5");
      `I
        ( "Sort by component type then start time:",
          "caled list --sort type --sort start" );
      `S Manpage.s_options;
    ]
    @ date_format_manpage_entries
    @ [ `S Manpage.s_see_also ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "list" ~doc ~man ~exits:exit_info in
  Cmd.v info term
