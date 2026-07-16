open Cmdliner
open Caledonia_lib

let validate_component_options ~component_type ~allowed supplied =
  match
    List.find_opt
      (fun (option_name, present) ->
        present && not (List.mem option_name allowed))
      supplied
  with
  | None -> Ok ()
  | Some (option_name, _) ->
      Error
        (`Msg
           (Printf.sprintf "--%s is not valid for %s components" option_name
              component_type))

let component_type_arg =
  let doc = "Type of component to add (event, todo, journal)" in
  let comp_type_enum = [ "event"; "todo"; "journal" ] in
  Arg.(
    value
    & opt (enum (List.map (fun s -> (s, s)) comp_type_enum)) "event"
    & info [ "type" ] ~docv:"TYPE" ~doc)

let categories_arg =
  let doc = "Comma-separated list of categories" in
  Arg.(
    value
    & opt (some string) None
    & info [ "categories"; "C" ] ~docv:"CATEGORIES" ~doc)

let priority_arg =
  let doc = "Priority level (0-9; 0 is undefined and 1 is highest)" in
  Arg.(
    value & opt (some int) None & info [ "priority"; "p" ] ~docv:"PRIORITY" ~doc)

let due_date_arg =
  let doc = "Due date for todo (YYYY-MM-DD)" in
  Arg.(value & opt (some string) None & info [ "due" ] ~docv:"DUE_DATE" ~doc)

let due_time_arg =
  let doc = "Due time for todo (HH:MM)" in
  Arg.(
    value & opt (some string) None & info [ "due-time" ] ~docv:"DUE_TIME" ~doc)

let duration_arg =
  let doc =
    "VTODO duration after DTSTART (for example, 30m, 2h15m, or 1d). Requires a \
     DTSTART (supply --date when adding) and cannot be combined with --due."
  in
  Arg.(
    value & opt (some string) None & info [ "duration" ] ~docv:"DURATION" ~doc)

let percent_arg =
  let doc = "Percent complete (0-100)" in
  Arg.(value & opt (some int) None & info [ "percent" ] ~docv:"PERCENT" ~doc)

let status_arg =
  let doc =
    "Status (draft, final, cancelled, needs-action, completed, in-process, \
     tentative, confirmed)"
  in
  Arg.(
    value
    & opt (some (enum Component_status.bindings)) None
    & info [ "status" ] ~docv:"STATUS" ~doc)

let parent_arg =
  let doc = "Parent todo UID (for subtasks)" in
  Arg.(
    value & opt (some string) None & info [ "parent" ] ~docv:"PARENT_UID" ~doc)

let no_parent_flag =
  let doc = "Remove parent relationship" in
  Arg.(value & flag & info [ "no-parent" ] ~doc)

let no_alarms_flag =
  let doc = "Remove all alarms" in
  Arg.(value & flag & info [ "no-alarms" ] ~doc)

let clear_fields_arg =
  let fields =
    [
      "summary";
      "start";
      "end";
      "location";
      "description";
      "categories";
      "recurrence";
      "alarms";
      "due";
      "duration";
      "priority";
      "percent";
      "status";
      "parent";
    ]
  in
  let doc =
    "Explicitly clear an optional field. May be repeated. A field cannot be  \n\
    \     set and cleared in the same edit."
  in
  Arg.(
    value
    & opt_all (enum (List.map (fun field -> (field, field)) fields)) []
    & info [ "clear" ] ~docv:"FIELD" ~doc)
