open Cmdliner
open Caledonia_lib
open Query_args

let run ?from_str ?to_str ~calendar ?count ?query_text ~summary ~description
    ~categories ~id ~format ~today ~tomorrow ~week ~month ~component_type
    ?timezone ~sort ~color ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let now = Ptime_clock.now () in
  let* tz = Query_args.parse_timezone_result ~timezone in
  let* scope =
    resolve_temporal_scope ~tz ~now ~from_str ~to_str ~today ~tomorrow ~week
      ~month ~default:`Unbounded
  in
  let* documents = Calendar_dir.get_documents ~fs calendar_dir in
  let all_components = List.concat_map Calendar_document.components documents in
  let component_types =
    match component_type with
    | "all" -> []
    | "event" -> [ Component_kind.Event ]
    | "todo" -> [ Component_kind.Todo ]
    | "journal" -> [ Component_kind.Journal ]
    | _ -> assert false
  in
  let text_fields =
    let selected =
      [
        (summary, Component_query.Summary);
        (description, Component_query.Description);
        (categories, Component_query.Categories);
      ]
      |> List.filter_map (fun (enabled, field) ->
          if enabled then Some field else None)
    in
    selected
  in
  let criteria =
    Component_query.
      {
        no_criteria with
        calendars = calendar;
        component_types;
        text = query_text;
        text_fields;
        id;
      }
  in
  let* items =
    match scope with
    | Unbounded ->
        Component_query.run_unbounded ~timezone:tz ~now
          ~include_todo_ancestors:true
          ~sort:(create_component_query_sort sort)
          ?limit:count ~criteria all_components
    | Bounded { from; to_ } ->
        Component_query.run ~timezone:tz ~now ~from ~to_
          ~include_todo_ancestors:true
          ~sort:(create_component_query_sort sort)
          ?limit:count ~criteria all_components
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

let query_text_arg =
  let doc =
    "Text to search for in summary, description, location, and categories."
  in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"TEXT" ~doc)

let summary_arg =
  let doc = "Search in event summaries only" in
  Arg.(value & flag & info [ "summary"; "s" ] ~doc)

let description_arg =
  let doc = "Search in descriptions only" in
  Arg.(value & flag & info [ "description"; "D" ] ~doc)

let categories_arg =
  let doc = "Search in categories only" in
  Arg.(value & flag & info [ "categories" ] ~doc)

let component_type_filter_arg =
  let doc = "Filter by component type (all, event, todo, journal)" in
  let comp_type_enum = [ "all"; "event"; "todo"; "journal" ] in
  Arg.(
    value
    & opt (enum (List.map (fun s -> (s, s)) comp_type_enum)) "all"
    & info [ "type" ] ~docv:"TYPE" ~doc)

let id_arg =
  let doc = "Search for a component with a specific ID" in
  Arg.(value & opt (some string) None & info [ "id"; "i" ] ~docv:"ID" ~doc)

let cmd ~fs calendar_dir =
  let run query_text from_str to_str calendars count format summary description
      categories id today tomorrow week month component_type timezone sort color
      () =
    match
      run ?from_str ?to_str ~calendar:calendars ?count ?query_text ~summary
        ~description ~categories ~id ~format ~today ~tomorrow ~week ~month
        ~component_type ?timezone ~sort ~color ~fs calendar_dir
    with
    | Error (`Msg msg) ->
        Output.print_error "Error" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(
      const run $ query_text_arg $ from_arg $ to_arg $ calendar_arg $ count_arg
      $ format_arg $ summary_arg $ description_arg $ categories_arg $ id_arg
      $ today_arg $ tomorrow_arg $ week_arg $ month_arg
      $ component_type_filter_arg $ timezone_arg $ sort_arg $ color_arg)
  in
  let doc = "Search calendar components for specific text" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Search calendar components for text in summary, description, \
         location, or categories fields. By default, the search looks across \
         all four text fields in every component master regardless of date. \
         Recurring series are returned once as their authored master and are \
         not expanded, because an unbounded recurrence can contain infinitely \
         many instances. The --summary, --description, and --categories flags \
         restrict the search to those selected fields; location is included by \
         the default search. You can use date flags to show components for a \
         specific time period; recurring events are then expanded only inside \
         that range and protected by a 100,000-instance work limit. Results \
         can be ordered with the --sort option. When --from is given without \
         --to, the range covers one calendar month from that local date; --to \
         without --from has no lower bound.";
      `S Manpage.s_examples;
      `I ("Search for 'meeting' in all components:", "caled search meeting");
      `I
        ( "Search for 'interview' in summaries only:",
          "caled search --summary interview" );
      `I ("Search for 'work' in categories:", "caled search --categories work");
      `I ("Search for todos only:", "caled search --type todo");
      `I
        ( "Search for 'project' in components this month:",
          "caled search --month project" );
      `S Manpage.s_options;
    ]
    @ date_format_manpage_entries
    @ [ `S Manpage.s_see_also ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "search" ~doc ~man ~exits:exit_info in
  Cmd.v info term
