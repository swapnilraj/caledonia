open Cmdliner
open Caledonia_lib
open Query_args

let format_alarm_fire ?tz ?get_color (af : Alarm_query.fire) =
  let fire_time = af.fire_time in
  let summary =
    match Component_query.get_summary af.owner with
    | Some s -> Format_utils.sanitize_terminal_line s
    | None -> "(no summary)"
  in
  let calendar =
    Component_query.get_calendar_name af.owner
    |> Format_utils.sanitize_terminal_line
  in
  let calendar_key = Component_query.get_calendar_key af.owner in
  let trigger_str = Format_utils.format_alarm_trigger_text af.alarm in
  let fire_str = Format_utils.format_date ?tz fire_time in
  let fire_time_str =
    let timezone = Option.value tz ~default:Timedesc.Time_zone.utc in
    let dt = Date.ptime_to_timedesc ~tz:timezone fire_time in
    Printf.sprintf "%02d:%02d" (Timedesc.hour dt) (Timedesc.minute dt)
  in
  let color = match get_color with Some f -> f calendar_key | None -> None in
  let cal_str =
    match color with
    | Some c -> Format_utils.colorize ~color:c calendar
    | None -> calendar
  in
  Printf.sprintf "%s %s  %s  %s  %s" fire_str fire_time_str trigger_str summary
    cal_str

let format_alarm_fires_text ?tz ?get_color fires =
  if fires = [] then "No alarms in range."
  else
    let data =
      List.map
        (fun (af : Alarm_query.fire) ->
          let fire_date = Format_utils.format_date ?tz af.fire_time in
          let fire_time =
            let timezone = Option.value tz ~default:Timedesc.Time_zone.utc in
            let dt = Date.ptime_to_timedesc ~tz:timezone af.fire_time in
            Printf.sprintf "%02d:%02d" (Timedesc.hour dt) (Timedesc.minute dt)
          in
          let trigger_str = Format_utils.format_alarm_trigger_text af.alarm in
          let summary =
            match Component_query.get_summary af.owner with
            | Some s -> Format_utils.sanitize_terminal_line s
            | None -> "(no summary)"
          in
          let calendar =
            Component_query.get_calendar_name af.owner
            |> Format_utils.sanitize_terminal_line
          in
          let calendar_key = Component_query.get_calendar_key af.owner in
          (fire_date, fire_time, trigger_str, summary, calendar, calendar_key))
        fires
    in
    let max_date = Format_utils.max_width (fun (d, _, _, _, _, _) -> d) data in
    let max_time = Format_utils.max_width (fun (_, t, _, _, _, _) -> t) data in
    let max_trigger =
      Format_utils.max_width (fun (_, _, tr, _, _, _) -> tr) data
    in
    let max_summary =
      Format_utils.max_width (fun (_, _, _, s, _, _) -> s) data
    in
    let max_cal = Format_utils.max_width (fun (_, _, _, _, c, _) -> c) data in
    List.map
      (fun (fire_date, fire_time, trigger_str, summary, calendar, calendar_key)
         ->
        let color =
          match get_color with Some f -> f calendar_key | None -> None
        in
        Printf.sprintf "%s %s  %s  %s  %s"
          (Format_utils.pad_to_width max_date fire_date)
          (Format_utils.pad_to_width max_time fire_time)
          (Format_utils.pad_to_width max_trigger trigger_str)
          (Format_utils.pad_to_width max_summary summary)
          (Format_utils.pad_to_width ?color max_cal calendar))
      data
    |> String.concat "\n"

let format_alarm_fires_json fires =
  let json_fires =
    List.map
      (fun (af : Alarm_query.fire) ->
        let trigger_str = Format_utils.format_alarm_trigger_text af.alarm in
        `Assoc
          [
            ("schema_version", `Int 1);
            ( "fire_time",
              `String (Ptime.to_rfc3339 ~frac_s:0 ~tz_offset_s:0 af.fire_time)
            );
            ("trigger", `String trigger_str);
            ( "summary",
              match Component_query.get_summary af.owner with
              | Some s -> `String s
              | None -> `Null );
            ("calendar", `String (Component_query.get_calendar_name af.owner));
            ("calendar_key", `String (Component_query.get_calendar_key af.owner));
            ("component_id", `String (Component_query.get_id af.owner));
          ])
      fires
  in
  Yojson.Safe.to_string (`List json_fires)

let format_alarm_fires_entries ?tz fires =
  List.map
    (fun (af : Alarm_query.fire) ->
      let trigger_str = Format_utils.format_alarm_trigger_text af.alarm in
      let summary =
        match Component_query.get_summary af.owner with
        | Some s -> Format_utils.sanitize_terminal_line s
        | None -> "(no summary)"
      in
      let calendar =
        Component_query.get_calendar_name af.owner
        |> Format_utils.sanitize_terminal_line
      in
      let fire_str =
        match tz with
        | Some tz -> Format_utils.format_date ~tz af.fire_time
        | None -> Format_utils.format_date af.fire_time
      in
      let fire_time_str =
        let timezone = Option.value tz ~default:Timedesc.Time_zone.utc in
        let dt = Date.ptime_to_timedesc ~tz:timezone af.fire_time in
        Printf.sprintf "%02d:%02d" (Timedesc.hour dt) (Timedesc.minute dt)
      in
      Printf.sprintf
        "Summary: %s\nFire Time: %s %s\nTrigger: %s\nCalendar: %s\nID: %s\n"
        summary fire_str fire_time_str trigger_str calendar
        (Component_query.get_id af.owner |> Format_utils.sanitize_terminal_line))
    fires
  |> String.concat "\n"

let run ?from_str ?to_str ~calendar:calendars ~format ~today ~tomorrow ~week
    ~month ?timezone ~color ~fs calendar_dir =
  let ( let* ) = Result.bind in
  let now = Ptime_clock.now () in
  let* tz = Query_args.parse_timezone_result ~timezone in
  let* scope =
    resolve_temporal_scope ~tz ~now ~from_str ~to_str ~today ~tomorrow ~week
      ~month ~default:`One_month
  in
  let from, to_ =
    match scope with
    | Bounded { from; to_ } -> (from, to_)
    | Unbounded -> assert false
  in
  let* components = Calendar_dir.get_components ~fs calendar_dir in
  let components =
    match calendars with
    | [] -> components
    | keys ->
        List.filter
          (fun component ->
            List.mem (Component.get_calendar_key component) keys)
          components
  in
  let* fires = Alarm_query.run_result ~floating_tz:tz ~from ~to_ components in
  let get_color calendar_key =
    Calendar_dir.get_color ~fs calendar_dir calendar_key
  in
  let output =
    match (fires, format) with
    | [], `Text -> "No alarms in range."
    | _ -> (
        match format with
        | `Text when Output.color_enabled color ->
            format_alarm_fires_text ~tz ~get_color fires
        | `Text -> format_alarm_fires_text ~tz fires
        | `Json -> format_alarm_fires_json fires
        | `Entries -> format_alarm_fires_entries ~tz fires
        | _ -> format_alarm_fires_text ~tz fires)
  in
  (match format with
  | `Json -> if output <> "" then Printf.printf "%s%!" output
  | `Text | `Entries -> if output <> "" then Printf.printf "%s\n%!" output
  | _ -> assert false);
  Ok ()

let format_arg =
  let doc = "Output format (text, json, entries)" in
  Arg.(
    value
    & opt
        (enum [ ("text", `Text); ("json", `Json); ("entries", `Entries) ])
        `Text
    & info [ "format"; "o" ] ~docv:"FORMAT" ~doc)

let cmd ~fs calendar_dir =
  let run from_str to_str calendars format today tomorrow week month timezone
      color () =
    match
      run ?from_str ?to_str ~calendar:calendars ~format ~today ~tomorrow ~week
        ~month ?timezone ~color ~fs calendar_dir
    with
    | Error (`Msg msg) ->
        Output.print_error "Error" msg;
        1
    | Ok () -> 0
  in
  let term =
    Term.(
      const run $ from_arg $ to_arg $ calendar_arg $ format_arg $ today_arg
      $ tomorrow_arg $ week_arg $ month_arg $ timezone_arg $ color_arg)
  in
  let doc = "List alarm fire times within a date range" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "List when alarms fire within a specified date range. Shows the fire \
         time, trigger offset, component summary, and calendar for each alarm.";
      `P "By default, shows alarms firing from today to one month from today.";
      `S Manpage.s_examples;
      `I ("List alarms firing today:", "caled alarms --today");
      `I ("List alarms for the week:", "caled alarms --week");
      `I
        ( "List alarms in a date range:",
          "caled alarms --from 2025-04-01 --to 2025-04-30" );
      `I ("List alarms in JSON format:", "caled alarms --today --format json");
      `S Manpage.s_options;
    ]
    @ date_format_manpage_entries
    @ [ `S Manpage.s_see_also ]
  in
  let exit_info =
    [ Cmd.Exit.info ~doc:"on success." 0; Cmd.Exit.info ~doc:"on error." 1 ]
  in
  let info = Cmd.info "alarms" ~doc ~man ~exits:exit_info in
  Cmd.v info term
