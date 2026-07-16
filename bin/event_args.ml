open Cmdliner
open Caledonia_lib

let calendar_name_arg =
  let doc = "Calendar to add the event to" in
  Arg.(
    required
    & opt (some string) None
    & info [ "calendar"; "c" ] ~docv:"CALENDAR" ~doc)

let required_summary_arg =
  let doc = "Event summary/title" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"SUMMARY" ~doc)

let optional_summary_arg =
  let doc = "Event summary/title" in
  Arg.(
    value
    & opt (some string) None
    & info [ "summary"; "s" ] ~docv:"SUMMARY" ~doc)

let start_date_arg =
  let doc = "Event start date (YYYY-MM-DD)" in
  Arg.(value & opt (some string) None & info [ "date"; "d" ] ~docv:"DATE" ~doc)

let start_time_arg =
  let doc = "Event start time (HH:MM)" in
  Arg.(value & opt (some string) None & info [ "time"; "t" ] ~docv:"TIME" ~doc)

let end_date_arg =
  let doc =
    "Inclusive event end date (YYYY-MM-DD). An all-day event defaults to one \
     day; an end time without an end date uses DATE."
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "end-date"; "e" ] ~docv:"END_DATE" ~doc)

let end_time_arg =
  let doc = "Event end time (HH:MM)" in
  Arg.(
    value
    & opt (some string) None
    & info [ "end-time"; "T" ] ~docv:"END_TIME" ~doc)

let timezone_arg =
  let doc =
    "Timezone to add events to (e.g., 'America/New_York', 'UTC', \
     'Europe/London'). If not specified, will use the local timezone. For a \
     floating time (interpreted in the selected system/query timezone), use \
     'FLOATING'."
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "timezone"; "z" ] ~docv:"TIMEZONE" ~doc)

let end_timezone_arg =
  let doc = "The timezone of the end of the event. Defaults to TIMEZONE." in
  Arg.(
    value
    & opt (some string) None
    & info [ "end-timezone"; "Z" ] ~docv:"END_TIMEZONE" ~doc)

let location_arg =
  let doc = "Event location" in
  Arg.(
    value
    & opt (some string) None
    & info [ "location"; "l" ] ~docv:"LOCATION" ~doc)

let description_arg =
  let doc = "Event description" in
  Arg.(
    value
    & opt (some string) None
    & info [ "description"; "D" ] ~docv:"DESCRIPTION" ~doc)

let recur_arg =
  let doc = "See RECURRENCE section" in
  Arg.(
    value & opt (some string) None & info [ "recur"; "r" ] ~docv:"RECUR" ~doc)

let alarm_arg =
  let doc =
    "Add an alarm trigger before the event start (e.g., '15m', '1h', '1d', \
     '2h30m'). Can be specified multiple times."
  in
  Arg.(value & opt_all string [] & info [ "alarm"; "a" ] ~docv:"ALARM" ~doc)

let date_format_manpage_entries =
  [
    `S "DATE FORMATS";
    `P
      "The following are the possible date formats for the --date and \
       --end-date command line parameters. Note the value is dependent on \
       --date / --end-date, so --date 2025-03 --end-date 2025-03 will span the \
       month of March.";
    `I ("YYYY-MM-DD", "Specific date (e.g., 2025-3-27, zero-padding optional)");
    `I ("YYYY-MM", "Start/end of specific month (e.g., 2025-3 for March 2025)");
    `I ("YYYY", "Start/end of specific year (e.g., 2025)");
    `I ("today", "Current day");
    `I ("tomorrow", "Next day");
    `I ("yesterday", "Previous day");
    `I ("this-week", "Start/end of current week");
    `I ("next-week", "Start/end of next week");
    `I ("this-month", "Start/end of current month");
    `I ("next-month", "Start/end of next month");
    `I ("+Nd", "N days from today (e.g., +7d for a week from today)");
    `I ("-Nd", "N days before today (e.g., -7d for a week ago)");
    `I ("+Nw", "N weeks from today (e.g., +4w for 4 weeks from today)");
    `I ("+Nm", "N months from today (e.g., +2m for 2 months from today)");
  ]

let conversion_error error = `Msg (Date.string_of_conversion_error error)

let wall_clock_of_instant ~tz instant =
  let ( let* ) = Result.bind in
  let* local =
    Date.ptime_to_timedesc_result ~tz instant
    |> Result.map_error conversion_error
  in
  let date = (Timedesc.year local, Timedesc.month local, Timedesc.day local) in
  let time =
    (Timedesc.hour local, Timedesc.minute local, Timedesc.second local)
  in
  match Ptime.of_date_time (date, (time, 0)) with
  | Some wall_clock -> Ok wall_clock
  | None -> Error (`Msg "Calendar wall-clock value is out of range")

let parse_calendar_date ~tz ~now expression parameter =
  let ( let* ) = Result.bind in
  let* instant = Date.parse_date ~tz ~now expression parameter in
  let* local =
    Date.ptime_to_timedesc_result ~tz instant
    |> Result.map_error conversion_error
  in
  Ok (Timedesc.year local, Timedesc.month local, Timedesc.day local)

let exact_calendar_date value =
  match String.split_on_char '-' value with
  | [ year; month; day ] -> (
      match
        (int_of_string_opt year, int_of_string_opt month, int_of_string_opt day)
      with
      | Some year, Some month, Some day ->
          let date = (year, month, day) in
          Option.map (fun _ -> date) (Ptime.of_date date)
      | _ -> None)
  | _ -> None

let exact_clock_time value =
  match String.split_on_char ':' value with
  | [ hour; minute ] -> (
      match (int_of_string_opt hour, int_of_string_opt minute) with
      | Some hour, Some minute -> Some (hour, minute, 0)
      | _ -> None)
  | [ hour; minute; second ] -> (
      match
        ( int_of_string_opt hour,
          int_of_string_opt minute,
          int_of_string_opt second )
      with
      | Some hour, Some minute, Some second -> Some (hour, minute, second)
      | _ -> None)
  | _ -> None

let parse_calendar_datetime ~tz ~now ~date ~time parameter =
  let ( let* ) = Result.bind in
  let* date =
    match exact_calendar_date date with
    | Some date -> Ok date
    | None -> parse_calendar_date ~tz ~now date parameter
  in
  let* time =
    match exact_clock_time time with
    | Some time -> Ok time
    | None -> Error (`Msg "Time must use HH:MM or HH:MM:SS format")
  in
  let* wall_clock =
    match Ptime.of_date_time (date, (time, 0)) with
    | Some value -> Ok value
    | None -> Error (`Msg "Calendar wall-clock value is out of range")
  in
  let* _instant =
    Date.ptime_of_ical_result ~floating_tz:tz (`Datetime (`Local wall_clock))
    |> Result.map_error conversion_error
  in
  Ok wall_clock

let parse_calendar_wall_datetime ~now ~date ~time parameter =
  let ( let* ) = Result.bind in
  let* date =
    match exact_calendar_date date with
    | Some date -> Ok date
    | None ->
        parse_calendar_date ~tz:(Date.local_timezone ()) ~now date parameter
  in
  let* time =
    match exact_clock_time time with
    | Some time -> Ok time
    | None -> Error (`Msg "Time must use HH:MM or HH:MM:SS format")
  in
  match Ptime.of_date_time (date, (time, 0)) with
  | Some value -> Ok value
  | None -> Error (`Msg "Calendar wall-clock value is out of range")

let parse_calendar_date_expression ~now expression parameter =
  match exact_calendar_date expression with
  | Some date -> Ok date
  | None ->
      parse_calendar_date ~tz:(Date.local_timezone ()) ~now expression parameter

let add_calendar_days (year, month, day) days =
  match Timedesc.Date.Ymd.make ~year ~month ~day with
  | Error _ -> Error (`Msg "Calendar date is out of range")
  | Ok date ->
      let date = Timedesc.Date.add ~days date in
      Ok
        ( Timedesc.Date.year date,
          Timedesc.Date.month date,
          Timedesc.Date.day date )

let timezone_of_name tzid =
  match Timedesc.Time_zone.make tzid with
  | Some timezone -> Ok timezone
  | None -> Error (`Msg (Printf.sprintf "Unknown timezone %S" tzid))

let parse_start ~now ~start_date ~start_time ~timezone =
  let ( let* ) = Result.bind in
  match start_date with
  | None ->
      let* _ =
        match start_time with
        | None -> Ok ()
        | Some _ ->
            Error (`Msg "Can't specify a start time without a start date")
      in
      let* _ =
        match timezone with
        | None -> Ok ()
        | _ -> Error (`Msg "Can't specify a timezone without a start date")
      in
      Ok None
  | Some start_date -> (
      match start_time with
      | None ->
          let* _ =
            match timezone with
            | None -> Ok ()
            | _ -> Error (`Msg "Can't specify a timezone without a start time")
          in
          let* date = parse_calendar_date_expression ~now start_date `From in
          Ok (Some (Icalendar.Params.singleton Valuetype `Date, `Date date))
      | Some start_time -> (
          match timezone with
          | None ->
              let timezone = Date.local_timezone () in
              let tzid = Timedesc.Time_zone.name timezone in
              let* datetime =
                parse_calendar_datetime ~tz:timezone ~now ~date:start_date
                  ~time:start_time `From
              in
              Ok
                (Some
                   ( Icalendar.Params.empty,
                     `Datetime (`With_tzid (datetime, (false, tzid))) ))
          | Some "FLOATING" ->
              let* datetime =
                parse_calendar_wall_datetime ~now ~date:start_date
                  ~time:start_time `From
              in
              Ok (Some (Icalendar.Params.empty, `Datetime (`Local datetime)))
          | Some "UTC" ->
              let* datetime =
                parse_calendar_datetime ~tz:Timedesc.Time_zone.utc ~now
                  ~date:start_date ~time:start_time `From
              in
              Ok (Some (Icalendar.Params.empty, `Datetime (`Utc datetime)))
          | Some tzid ->
              let* timezone = timezone_of_name tzid in
              let* datetime =
                parse_calendar_datetime ~tz:timezone ~now ~date:start_date
                  ~time:start_time `From
              in
              Ok
                (Some
                   ( Icalendar.Params.empty,
                     `Datetime (`With_tzid (datetime, (false, tzid))) ))))

let parse_end ~now ~end_date ~end_time ~end_timezone =
  let ( let* ) = Result.bind in
  match end_date with
  | None ->
      let* _ =
        match end_time with
        | None -> Ok ()
        | Some _ -> Error (`Msg "Can't specify an end time without an end date")
      in
      let* _ =
        match end_timezone with
        | None -> Ok ()
        | Some _ ->
            Error (`Msg "Can't specify an end timezone without an end date")
      in
      Ok None
  | Some end_date -> (
      match end_time with
      | None ->
          let* _ =
            match end_timezone with
            | Some _ ->
                Error (`Msg "Can't specify an end timezone without an end time")
            | _ -> Ok ()
          in
          let* date = parse_calendar_date_expression ~now end_date `From in
          (* The CLI end date is inclusive; RFC DTEND;VALUE=DATE is exclusive,
             so persist midnight at the start of the following day. *)
          let* date = add_calendar_days date 1 in
          Ok
            (Some
               (`Dtend (Icalendar.Params.singleton Valuetype `Date, `Date date)))
      | Some end_time -> (
          match end_timezone with
          | None ->
              let timezone = Date.local_timezone () in
              let tzid = Timedesc.Time_zone.name timezone in
              let* datetime =
                parse_calendar_datetime ~tz:timezone ~now ~date:end_date
                  ~time:end_time `From
              in
              Ok
                (Some
                   (`Dtend
                      ( Icalendar.Params.empty,
                        `Datetime (`With_tzid (datetime, (false, tzid))) )))
          | Some "FLOATING" ->
              let* datetime =
                parse_calendar_wall_datetime ~now ~date:end_date ~time:end_time
                  `From
              in
              Ok
                (Some
                   (`Dtend (Icalendar.Params.empty, `Datetime (`Local datetime))))
          | Some "UTC" ->
              let* datetime =
                parse_calendar_datetime ~tz:Timedesc.Time_zone.utc ~now
                  ~date:end_date ~time:end_time `From
              in
              Ok
                (Some
                   (`Dtend (Icalendar.Params.empty, `Datetime (`Utc datetime))))
          | Some tzid ->
              let* timezone = timezone_of_name tzid in
              let* datetime =
                parse_calendar_datetime ~tz:timezone ~now ~date:end_date
                  ~time:end_time `From
              in
              Ok
                (Some
                   (`Dtend
                      ( Icalendar.Params.empty,
                        `Datetime (`With_tzid (datetime, (false, tzid))) )))))

let combine_results (results : ('a, 'b) result list) : ('a list, 'b) result =
  let rec aux acc = function
    | [] -> Ok (List.rev acc)
    | Ok v :: rest -> aux (v :: acc) rest
    | Error e :: _ -> Error e
  in
  aux [] results

let parse_recurrence rrule =
  let rrule = String.trim rrule in
  Calendar_codec.parse_event_rrule rrule
  |> Result.map_error (fun message ->
      `Msg
        ("Invalid recurrence rule" ^ if message = "" then "" else ": " ^ message))

let parse_duration_seconds s =
  let ( let* ) = Result.bind in
  let s = String.lowercase_ascii (String.trim s) in
  let len = String.length s in
  if len = 0 then Error (`Msg "Empty duration specification")
  else
    let rec parse_parts i total_seconds =
      if i >= len then Ok total_seconds
      else
        (* read digits *)
        let j = ref i in
        while !j < len && s.[!j] >= '0' && s.[!j] <= '9' do
          incr j
        done;
        if !j = i then Error (`Msg ("Invalid duration format: " ^ s))
        else
          let* num =
            match int_of_string_opt (String.sub s i (!j - i)) with
            | Some number -> Ok number
            | None -> Error (`Msg ("Duration value is out of range: " ^ s))
          in
          if !j >= len then
            Error (`Msg ("Missing unit suffix in duration: " ^ s))
          else
            let unit_start = !j in
            while !j < len && not (s.[!j] >= '0' && s.[!j] <= '9') do
              incr j
            done;
            let unit_str = String.sub s unit_start (!j - unit_start) in
            let multiplier =
              match unit_str with
              | "s" | "sec" | "second" | "seconds" -> Ok 1
              | "m" | "min" | "minute" | "minutes" -> Ok 60
              | "h" | "hr" | "hour" | "hours" -> Ok 3600
              | "d" | "day" | "days" -> Ok 86400
              | "w" | "week" | "weeks" -> Ok 604800
              | _ -> Error (`Msg ("Unknown alarm time unit: " ^ unit_str))
            in
            let* multiplier = multiplier in
            if num > (max_int - total_seconds) / multiplier then
              Error (`Msg ("Duration value is out of range: " ^ s))
            else parse_parts !j (total_seconds + (num * multiplier))
    in
    let* seconds = parse_parts 0 0 in
    if seconds = 0 then Error (`Msg "Duration must be greater than zero")
    else Ok seconds

let parse_duration value =
  Result.map
    (fun seconds -> Ptime.Span.of_int_s seconds)
    (parse_duration_seconds value)

let parse_alarm value =
  Result.map
    (fun seconds -> Ptime.Span.of_int_s (-seconds))
    (parse_duration_seconds value)

let make_display_alarm span =
  let open Icalendar in
  `Display
    {
      trigger = (Params.empty, `Duration span);
      duration_repeat = None;
      summary = None;
      other = [];
      special = { description = Some (Params.empty, "Reminder") };
    }

let parse_alarms alarm_strings =
  let ( let* ) = Result.bind in
  let rec aux acc = function
    | [] -> Ok (List.rev acc)
    | s :: rest ->
        let* span = parse_alarm s in
        aux (make_display_alarm span :: acc) rest
  in
  aux [] alarm_strings

let alarm_format_manpage_entries =
  [
    `S "ALARM";
    `P
      "Alarm trigger duration before the event/todo start. Can be specified \
       multiple times for multiple alarms.";
    `I ("15m", "15 minutes before");
    `I ("1h", "1 hour before");
    `I ("1d", "1 day before");
    `I ("2h30m", "2 hours and 30 minutes before");
    `I ("1w", "1 week before");
  ]

let recurrence_format_manpage_entries =
  [
    `S "RECURRENCE";
    `P "Recurrence rule in iCalendar RFC5545 format. The FREQ part is required.";
    `I ("FREQ=<frequency>", "DAILY, WEEKLY, MONTHLY, or YEARLY (required)");
    `I
      ( "COUNT=<number>",
        "Limit to this many occurrences (optional, cannot be used with UNTIL)"
      );
    `I
      ( "UNTIL=<date>",
        "Repeat until this date (optional, cannot be used with COUNT)" );
    `I
      ( "INTERVAL=<number>",
        "Interval between occurrences, e.g., 2 for every other (optional)" );
    `I
      ( "BYDAY=<dayspec>",
        "Specific days, e.g., MO,WE,FR or 1MO (first Monday) (optional)" );
    `I
      ( "BYMONTHDAY=<daynum>",
        "Day of month, e.g., 1,15 or -1 (last day) (optional)" );
    `I
      ( "BYMONTH=<monthnum>",
        "Month number, e.g., 1,6,12 for Jan,Jun,Dec (optional)" );
    `P "Examples:";
    `I ("FREQ=DAILY;COUNT=5", "Daily for 5 occurrences");
    `I ("FREQ=WEEKLY;INTERVAL=2", "Every other week indefinitely");
    `I ("FREQ=WEEKLY;BYDAY=MO,WE,FR", "Every Monday, Wednesday, Friday");
    `I ("FREQ=MONTHLY;BYDAY=1MO", "First Monday of every month");
    `I
      ( "FREQ=YEARLY;BYMONTH=1;BYMONTHDAY=1",
        "Every January 1st (New Year's Day)" );
    `I ("FREQ=MONTHLY;BYMONTHDAY=-1", "Last day of every month");
  ]
