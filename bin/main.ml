open Cmdliner

let list_cmd = List_cmd.cmd
let search_cmd = Search_cmd.cmd
let show_cmd = Show_cmd.cmd
let add_cmd = Add_cmd.cmd
let delete_cmd = Delete_cmd.cmd
let edit_cmd = Edit_cmd.cmd
let server_cmd = Server_cmd.cmd
let alarms_cmd = Alarms_cmd.cmd
let alarm_daemon_cmd = Alarm_daemon_cmd.cmd

let doc =
  "Command-line calendar tool for managing local .ics files with support for \
   events, todos, and journals"

let version = "0.5.0"

let arguments_before_end_of_options argv =
  let rec collect index arguments =
    if index >= Array.length argv || argv.(index) = "--" then List.rev arguments
    else collect (index + 1) (argv.(index) :: arguments)
  in
  collect 1 []

let is_help_argument argument =
  argument = "-h" || argument = "--help"
  || String.starts_with ~prefix:"--help=" argument

let informational_request argv =
  let arguments = arguments_before_end_of_options argv in
  arguments = []
  || List.exists is_help_argument arguments
  ||
  match arguments with
  | [ "--version" ] | [ _; "--version" ] -> true
  | _ -> false

let has_misplaced_version argv =
  let arguments = arguments_before_end_of_options argv in
  List.mem "--version" arguments
  &&
  match arguments with
  | [ "--version" ] | [ _; "--version" ] -> false
  | _ -> true

let resolve_calendar_dir_path () =
  match Sys.getenv_opt "CALENDAR_DIR" with
  | Some dir -> Ok dir
  | None -> (
      match Sys.getenv_opt "HOME" with
      | Some home -> Ok (Filename.concat home ".calendar")
      | None -> (
          try
            let home = (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir in
            Ok (Filename.concat home ".calendar")
          with Not_found ->
            Error
              "Cannot determine the default calendar directory; set \
               CALENDAR_DIR or HOME"))

let main env =
  let exit_info =
    [
      Cmd.Exit.info ~doc:"on success." 0;
      Cmd.Exit.info
        ~doc:
          "on error (including invalid date format, file access issues, or \
           other errors)."
        1;
    ]
  in
  let man =
    [
      `S Manpage.s_environment;
      `P
        "CALENDAR_DIR selects the calendar root. If unset, Caledonia uses \
         ~/.calendar/ (resolved from HOME, then the current user's account).";
    ]
  in
  let info = Cmd.info "caled" ~version ~doc ~exits:exit_info ~man in
  let default =
    Term.(ret (const (fun () -> `Help (`Pager, None)) $ const ()))
  in
  let fs = Eio.Stdenv.fs env in
  let eval calendar_dir =
    match
      Cmd.eval_value
        (Cmd.group info ~default
           [
             list_cmd ~fs calendar_dir;
             search_cmd ~fs calendar_dir;
             show_cmd ~fs calendar_dir;
             add_cmd ~fs calendar_dir;
             edit_cmd ~fs calendar_dir;
             delete_cmd ~fs calendar_dir;
             server_cmd ~stdin:(Eio.Stdenv.stdin env)
               ~stdout:(Eio.Stdenv.stdout env) ~fs calendar_dir;
             alarms_cmd ~fs calendar_dir;
             alarm_daemon_cmd ~clock:(Eio.Stdenv.clock env) ~fs calendar_dir;
           ])
    with
    | Ok (`Ok f) -> f ()
    | Ok _ -> 0
    | Error _ -> 1
  in
  if has_misplaced_version Sys.argv then (
    Output.print_error "Error"
      "--version must be used before a command or immediately after the \
       command name";
    1)
  else if informational_request Sys.argv then
    eval (Caledonia_lib.Calendar_dir.of_path "")
  else
    match resolve_calendar_dir_path () with
    | Error message ->
        Output.print_error "Error" message;
        1
    | Ok calendar_dir_path -> (
        match Caledonia_lib.Calendar_dir.create ~fs calendar_dir_path with
        | Error (`Msg e) ->
            Output.print_error "Error" e;
            1
        | Ok calendar_dir -> eval calendar_dir)

let () = Eio_main.run @@ fun env -> exit (main env)
