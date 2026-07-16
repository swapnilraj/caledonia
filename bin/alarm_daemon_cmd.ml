open Cmdliner
open Caledonia_lib

type notifier = Auto | Notify_send | Osascript | Stdout | Disabled

let find_executable name =
  let is_executable path =
    try
      Unix.access path [ Unix.X_OK ];
      not (Sys.is_directory path)
    with Unix.Unix_error _ | Sys_error _ -> false
  in
  String.split_on_char ':' (Option.value ~default:"" (Sys.getenv_opt "PATH"))
  |> List.find_map (fun directory ->
      let path = Filename.concat directory name in
      if is_executable path then Some path else None)

let resolve_notifier = function
  | Disabled -> Ok `Disabled
  | Stdout -> Ok `Stdout
  | Notify_send -> (
      match find_executable "notify-send" with
      | Some path -> Ok (`Notify_send path)
      | None -> Error (`Msg "notify-send was requested but is not installed"))
  | Osascript -> (
      match find_executable "osascript" with
      | Some path -> Ok (`Osascript path)
      | None -> Error (`Msg "osascript was requested but is not installed"))
  | Auto -> (
      match find_executable "notify-send" with
      | Some path -> Ok (`Notify_send path)
      | None -> (
          match find_executable "osascript" with
          | Some path -> Ok (`Osascript path)
          | None ->
              Error
                (`Msg
                   "no desktop notifier is available; install notify-send, use \
                    --notifier osascript on macOS, or choose --notifier stdout")
          ))

let wait_for_process ~clock ~timeout_seconds pid =
  let monotonic_now () = Eio.Time.now clock in
  let deadline = Monotonic_deadline.after ~now:monotonic_now timeout_seconds in
  let rec wait () =
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ when Monotonic_deadline.reached ~now:monotonic_now deadline ->
        Unix.kill pid Sys.sigkill;
        ignore (Unix.waitpid [] pid);
        Error
          (`Msg
             (Printf.sprintf "notifier timed out after %.3g seconds"
                timeout_seconds))
    | 0, _ ->
        Eio.Time.sleep clock 0.05;
        wait ()
    | _, Unix.WEXITED 0 -> Ok ()
    | _, Unix.WEXITED code ->
        Error (`Msg (Printf.sprintf "notifier exited with status %d" code))
    | _, Unix.WSIGNALED signal | _, Unix.WSTOPPED signal ->
        Error (`Msg (Printf.sprintf "notifier stopped by signal %d" signal))
  in
  wait ()

let send_notification ~clock ~backend ~summary ~trigger_str ~fire_time_str
    ~calendar =
  let sanitize = Format_utils.sanitize_terminal_line in
  let summary = sanitize summary in
  let body =
    Printf.sprintf "%s — %s (%s)" fire_time_str trigger_str calendar |> sanitize
  in
  match backend with
  | `Disabled -> Ok ()
  | `Stdout ->
      Printf.printf "ALARM: %s — %s\n%!" summary body;
      Ok ()
  | `Notify_send executable -> (
      let argv =
        [| executable; "-u"; "critical"; "-a"; "Caledonia"; summary; body |]
      in
      try
        let pid =
          Unix.create_process executable argv Unix.stdin Unix.stdout Unix.stderr
        in
        wait_for_process ~clock ~timeout_seconds:10.0 pid
      with Unix.Unix_error (error, operation, path) ->
        Error
          (`Msg
             (Printf.sprintf "notifier %s failed for %s: %s" operation path
                (Unix.error_message error))))
  | `Osascript executable -> (
      let script =
        "on run argv\n"
        ^ "display notification (item 2 of argv) with title (item 1 of argv)\n"
        ^ "end run"
      in
      let argv = [| executable; "-e"; script; summary; body |] in
      try
        let pid =
          Unix.create_process executable argv Unix.stdin Unix.stdout Unix.stderr
        in
        wait_for_process ~clock ~timeout_seconds:10.0 pid
      with Unix.Unix_error (error, operation, path) ->
        Error
          (`Msg
             (Printf.sprintf "notifier %s failed for %s: %s" operation path
                (Unix.error_message error))))

let notification_fields ~timezone (alarm_fire : Alarm_query.fire) =
  let summary =
    Option.value ~default:"(no summary)"
      (Component_query.get_summary alarm_fire.owner)
  in
  let local = Date.ptime_to_timedesc ~tz:timezone alarm_fire.fire_time in
  let fire_time =
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (Timedesc.year local)
      (Timedesc.month local) (Timedesc.day local) (Timedesc.hour local)
      (Timedesc.minute local) (Timedesc.second local)
  in
  ( summary,
    Format_utils.format_alarm_trigger_text alarm_fire.alarm,
    fire_time,
    Component_query.get_calendar_name alarm_fire.owner )

let run ~clock ~now ~fs calendar_dir ~notifier ~rescan_interval ~grace () =
  let ( let* ) = Result.bind in
  let timezone = Date.local_timezone () in
  let* backend = resolve_notifier notifier in
  let root = Calendar_dir.get_path calendar_dir in
  let state_path = Filename.concat root ".caledonia-alarm-state.json" in
  let* watcher = Alarm_watcher.create root in
  Fun.protect
    ~finally:(fun () -> Alarm_watcher.close watcher)
    (fun () ->
      Output.print_terminal_stdout_line
        (Printf.sprintf "Alarm daemon started for %s using %s" root
           Alarm_watcher.backend_name);
      let report = Output.print_terminal_stderr_line in
      let driver : Alarm_daemon_core.driver =
        {
          now;
          wait =
            (fun ~timeout ->
              match Alarm_watcher.wait ~clock ~timeout watcher with
              | Ok Alarm_watcher.Timer -> Alarm_daemon_core.Timer
              | Ok Alarm_watcher.Changed -> Alarm_daemon_core.Changed
              | Ok Alarm_watcher.Overflow -> Alarm_daemon_core.Overflow
              | Error (`Msg message) -> Alarm_daemon_core.Watcher_error message);
          sleep = Eio.Time.sleep clock;
          load_state = (fun () -> Alarm_daemon_core.load_state_file state_path);
          save_state = Alarm_daemon_core.save_state_file state_path;
          load_fires =
            (fun ~from ~to_ ->
              let* components =
                Calendar_dir.get_components_tolerant ~report ~fs calendar_dir
              in
              Alarm_query.run_result ~floating_tz:timezone ~from:(Some from)
                ~to_ components);
          notify =
            (fun alarm_fire ->
              let summary, trigger_str, fire_time_str, calendar =
                notification_fields ~timezone alarm_fire
              in
              send_notification ~clock ~backend ~summary ~trigger_str
                ~fire_time_str ~calendar);
          report;
        }
      in
      let* _state = Alarm_daemon_core.run ~grace ~rescan_interval driver in
      Ok ())

let notifier_arg =
  let values =
    [
      ("auto", Auto);
      ("notify-send", Notify_send);
      ("osascript", Osascript);
      ("stdout", Stdout);
      ("none", Disabled);
    ]
  in
  let doc =
    "Notification backend: auto, notify-send, osascript, stdout, or none. Auto \
     reports an error when no desktop backend is installed."
  in
  Arg.(
    value & opt (enum values) Auto & info [ "notifier" ] ~docv:"BACKEND" ~doc)

let positive_float name value =
  match float_of_string_opt value with
  | Some value when value > 0. -> Ok value
  | Some _ -> Error (`Msg (name ^ " must be greater than zero"))
  | None -> Error (`Msg (name ^ " must be a number"))

let rescan_interval_arg =
  let converter =
    Arg.conv (positive_float "rescan interval", Format.pp_print_float)
  in
  let doc =
    "Maximum seconds between safety rescans. inotify changes trigger earlier \
     rescans on Linux."
  in
  Arg.(
    value & opt converter 60.0 & info [ "rescan-interval" ] ~docv:"SECONDS" ~doc)

let grace_arg =
  let positive value =
    match int_of_string_opt value with
    | Some value when value >= 0 -> Ok value
    | _ -> Error (`Msg "grace period must be a non-negative integer")
  in
  let converter = Arg.conv (positive, Format.pp_print_int) in
  let doc = "Maximum seconds of missed alarms to recover after downtime." in
  Arg.(value & opt converter 300 & info [ "grace" ] ~docv:"SECONDS" ~doc)

let cmd ~clock ~fs calendar_dir =
  let run notifier rescan_interval grace () =
    match
      run ~clock ~now:Ptime_clock.now ~fs calendar_dir ~notifier
        ~rescan_interval ~grace ()
    with
    | Ok () -> 0
    | Error (`Msg message) ->
        Output.print_error "Error" message;
        1
  in
  let term =
    Term.(const run $ notifier_arg $ rescan_interval_arg $ grace_arg)
  in
  let doc = "Run the reliable alarm notification daemon" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Watch calendar files recursively with inotify on Linux and reconcile \
         with periodic full scans. Other platforms use the same safety scan as \
         a portable fallback. Failed delivery counts are persisted across \
         restarts. Each notification receives at most three attempts; the \
         watermark advances to the oldest retry so successful prefixes do not \
         grow the replay window. A final failure is reported and dropped.";
      `S Manpage.s_examples;
      `I ("Use Linux notify-send:", "caled alarm-daemon --notifier notify-send");
      `I
        ( "Use macOS Notification Center:",
          "caled alarm-daemon --notifier osascript" );
      `I ("Test in a terminal:", "caled alarm-daemon --notifier stdout");
    ]
  in
  let exits =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  Cmd.v (Cmd.info "alarm-daemon" ~doc ~man ~exits) term
