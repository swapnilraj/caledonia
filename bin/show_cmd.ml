open Cmdliner
open Caledonia_lib

let run ~component_id ~format ~color ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let now = Ptime_clock.now () in
  let* documents = Calendar_dir.get_documents ~fs calendar_dir in
  let components = List.concat_map Calendar_document.components documents in
  let* component =
    Command_common.find_unique_component ~id:component_id components
  in
  let get_color calendar_key =
    Calendar_dir.get_color ~fs calendar_dir calendar_key
  in
  let tz = Date.local_timezone () in
  let items = [ Component_query.Stored component ] in
  let* () =
    match format with
    | `Text | `Entries -> Output.validate_human_items ~tz items
    | `Json | `Csv | `Ics | `Sexp -> Ok ()
  in
  Output.print_items ~documents ~format ~tz ~now ~get_color ~color items

let component_id_arg =
  let doc = "ID of the component to show" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"ID" ~doc)

let format_arg =
  let doc = "Output format (text, entries, json, csv, ics, sexp)" in
  Arg.(
    value
    & opt (enum Query_args.format_enum) `Entries
    & info [ "format"; "o" ] ~docv:"FORMAT" ~doc)

let cmd ~fs calendar_dir =
  let run component_id format color () =
    match run ~component_id ~format ~color ~fs calendar_dir with
    | Error (`Msg msg) ->
        Output.print_error "Error" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(const run $ component_id_arg $ format_arg $ Query_args.color_arg)
  in
  let doc = "Show details of a specific component" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Show detailed information about a specific component (event, todo, or \
         journal) by its ID.";
      `P "You can find component IDs by using the `list` or `search` commands.";
      `S Manpage.s_examples;
      `P "Show component details:";
      `P "  caled show 12345678-1234-5678-1234-567812345678";
      `P "Show component details in JSON format:";
      `P "  caled show 12345678-1234-5678-1234-567812345678 --format json";
      `S Manpage.s_options;
    ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "show" ~doc ~man ~exits:exit_info in
  Cmd.v info term
