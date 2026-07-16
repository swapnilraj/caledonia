type conversion_error =
  [ `Unknown_timezone of string
  | `Nonexistent_local_time of string
  | `Ambiguous_local_time of string
  | `Out_of_range of string ]

let ( let* ) = Result.bind
let rfc3339_utc instant = Ptime.to_rfc3339 ~tz_offset_s:0 instant

let standard_parameter_name : type value.
    value Icalendar.icalparameter -> string option = function
  | Altrep -> Some "ALTREP"
  | Cn -> Some "CN"
  | Cutype -> Some "CUTYPE"
  | Delegated_from -> Some "DELEGATED-FROM"
  | Delegated_to -> Some "DELEGATED-TO"
  | Dir -> Some "DIR"
  | Encoding -> Some "ENCODING"
  | Media_type -> Some "FMTTYPE"
  | Fbtype -> Some "FBTYPE"
  | Language -> Some "LANGUAGE"
  | Member -> Some "MEMBER"
  | Partstat -> Some "PARTSTAT"
  | Range -> Some "RANGE"
  | Related -> Some "RELATED"
  | Reltype -> Some "RELTYPE"
  | Role -> Some "ROLE"
  | Rsvp -> Some "RSVP"
  | Sentby -> Some "SENT-BY"
  | Tzid -> Some "TZID"
  | Valuetype -> Some "VALUE"
  | Iana_param _ | Xparam _ -> None

let standard_parameter_names =
  [
    "ALTREP";
    "CN";
    "CUTYPE";
    "DELEGATED-FROM";
    "DELEGATED-TO";
    "DIR";
    "ENCODING";
    "FMTTYPE";
    "FBTYPE";
    "LANGUAGE";
    "MEMBER";
    "PARTSTAT";
    "RANGE";
    "RELATED";
    "RELTYPE";
    "ROLE";
    "RSVP";
    "SENT-BY";
    "TZID";
    "VALUE";
  ]

let is_standard_iana_parameter name =
  List.mem (String.uppercase_ascii name) standard_parameter_names

let invalid_parameter property message : (unit, [> `Msg of string ]) result =
  Error (`Msg (Printf.sprintf "%s %s" property message))

let validate_integral_seconds ~property span :
    (unit, [> `Msg of string ]) result =
  let _, picoseconds = Ptime.Span.to_d_ps span in
  if
    Int64.rem picoseconds 1_000_000_000_000L = 0L
    && Option.is_some (Ptime.Span.to_int_s span)
  then Ok ()
  else
    invalid_parameter property
      "must use an integral number of seconds representable by the serializer"

let validate_date_or_datetime_params ~property params value :
    (unit, [> `Msg of string ]) result =
  let expected_value_type, embedded_timezone =
    match value with
    | `Date _ -> (`Date, None)
    | `Datetime (`Utc _ | `Local _) -> (`Datetime, None)
    | `Datetime (`With_tzid (_, timezone)) -> (`Datetime, Some timezone)
  in
  let* () =
    match embedded_timezone with
    | Some (_, timezone) when String.trim timezone = "" ->
        invalid_parameter property "has an empty TZID"
    | Some _ | None -> Ok ()
  in
  let* () =
    match (value, Icalendar.Params.find Icalendar.Valuetype params) with
    | `Date _, Some `Date -> Ok ()
    | `Date _, _ ->
        invalid_parameter property "requires VALUE=DATE for a DATE value"
    | `Datetime _, _ -> Ok ()
  in
  let* () =
    match value with
    | `Date _ -> Ok ()
    | `Datetime (`Utc timestamp | `Local timestamp | `With_tzid (timestamp, _))
      ->
        if Ptime.Span.compare (Ptime.frac_s timestamp) Ptime.Span.zero = 0 then
          Ok ()
        else invalid_parameter property "must not contain fractional seconds"
  in
  let rec validate = function
    | [] -> Ok ()
    | Icalendar.Params.B (Icalendar.Valuetype, actual) :: rest ->
        if actual = expected_value_type then validate rest
        else
          invalid_parameter property
            "has a VALUE parameter inconsistent with its typed value"
    | Icalendar.Params.B (Icalendar.Tzid, timezone) :: rest -> (
        match embedded_timezone with
        | Some embedded when timezone = embedded -> validate rest
        | Some _ ->
            invalid_parameter property
              "has a TZID parameter inconsistent with its typed timezone"
        | None ->
            invalid_parameter property
              "has a TZID parameter on a DATE, UTC, or floating value")
    | Icalendar.Params.B (Icalendar.Iana_param name, _) :: rest ->
        if is_standard_iana_parameter name then
          invalid_parameter property
            (Printf.sprintf "does not allow the %s parameter"
               (String.uppercase_ascii name))
        else validate rest
    | Icalendar.Params.B (Icalendar.Xparam _, _) :: rest -> validate rest
    | Icalendar.Params.B (parameter, _) :: _ ->
        let name = Option.get (standard_parameter_name parameter) in
        invalid_parameter property
          (Printf.sprintf "does not allow the %s parameter" name)
  in
  validate (Icalendar.Params.bindings params)

let validate_duration_params ~property params duration :
    (unit, [> `Msg of string ]) result =
  let* () = validate_integral_seconds ~property duration in
  let rec validate = function
    | [] -> Ok ()
    | Icalendar.Params.B (Icalendar.Iana_param name, _) :: rest ->
        if is_standard_iana_parameter name then
          invalid_parameter property
            (Printf.sprintf "does not allow the %s parameter"
               (String.uppercase_ascii name))
        else validate rest
    | Icalendar.Params.B (Icalendar.Xparam _, _) :: rest -> validate rest
    | Icalendar.Params.B (parameter, _) :: _ ->
        let name = Option.get (standard_parameter_name parameter) in
        invalid_parameter property
          (Printf.sprintf "does not allow the %s parameter" name)
  in
  validate (Icalendar.Params.bindings params)

let string_of_conversion_error = function
  | `Unknown_timezone tzid -> Printf.sprintf "unknown timezone %s" tzid
  | `Nonexistent_local_time value ->
      Printf.sprintf "nonexistent local time (%s)" value
  | `Ambiguous_local_time value ->
      Printf.sprintf "ambiguous local time (%s)" value
  | `Out_of_range value -> Printf.sprintf "out-of-range date/time (%s)" value

let local_timezone () =
  Option.value (Timedesc.Time_zone.local ()) ~default:Timedesc.Time_zone.utc

let timedesc_to_ptime_result datetime =
  match Timedesc.to_timestamp datetime with
  | `Single timestamp | `Ambiguous (timestamp, _) -> (
      (* The public helper for selecting the first result was renamed from
         [min_of_local_result] through [min_of_local_dt_result] to
         [min_of_local_date_time_result] across Timedesc releases.  The
         [`Single]/[`Ambiguous] result itself is stable, and its first
         ambiguous value is the earlier instant, so match it directly. *)
      match Timedesc.Utils.ptime_of_timestamp timestamp with
      | Some instant -> Ok instant
      | None -> Error (`Out_of_range "Timedesc value"))

let ptime_to_timedesc_result ~tz instant =
  let timestamp = Timedesc.Utils.timestamp_of_ptime instant in
  match Timedesc.of_timestamp ~tz_of_date_time:tz timestamp with
  | Some datetime -> Ok datetime
  | None -> Error (`Out_of_range (rfc3339_utc instant))

let unwrap_conversion operation = function
  | Ok value -> value
  | Error error ->
      invalid_arg
        (Printf.sprintf "%s: %s" operation (string_of_conversion_error error))

let ptime_to_timedesc ~tz instant =
  unwrap_conversion "ptime_to_timedesc" (ptime_to_timedesc_result ~tz instant)

let wall_clock_parts local =
  let date, ((hour, minute, second), _) = Ptime.to_date_time local in
  (date, hour, minute, second)

let instant_with_offset ~label local offset =
  let offset = Ptime.Span.of_int_s offset in
  match Ptime.sub_span local offset with
  | Some instant -> Ok instant
  | None -> Error (`Out_of_range label)

let resolve_local_date_and_time ~label ~tz date time =
  let* local =
    let date =
      (Timedesc.Date.year date, Timedesc.Date.month date, Timedesc.Date.day date)
    in
    let hms =
      ( Timedesc.Time.hour time,
        Timedesc.Time.minute time,
        Timedesc.Time.second time )
    in
    match Ptime.of_date_time (date, (hms, 0)) with
    | Some local -> (
        let nanoseconds = Timedesc.Time.ns time in
        if nanoseconds = 0 then Ok local
        else
          let picoseconds = Int64.mul (Int64.of_int nanoseconds) 1_000L in
          match Ptime.Span.of_d_ps (0, picoseconds) with
          | Some fraction -> (
              match Ptime.add_span local fraction with
              | Some local -> Ok local
              | None -> Error (`Out_of_range label))
          | None -> Error (`Out_of_range label))
    | None -> Error (`Out_of_range label)
  in
  let days, picoseconds = Ptime.to_span local |> Ptime.Span.to_d_ps in
  let local_seconds =
    Int64.add
      (Int64.mul (Int64.of_int days) 86_400L)
      (Int64.div picoseconds 1_000_000_000_000L)
  in
  let same_wall_clock instant =
    let* resolved = ptime_to_timedesc_result ~tz instant in
    Ok
      (Timedesc.Date.equal (Timedesc.date resolved) date
      && Timedesc.Time.equal (Timedesc.time resolved) time)
  in
  let* candidates =
    List.fold_left
      (fun result offset ->
        let* candidates = result in
        let* instant = instant_with_offset ~label local offset in
        let* matches = same_wall_clock instant in
        Ok (if matches then instant :: candidates else candidates))
      (Ok [])
      (Timedesc.Time_zone.recorded_offsets tz)
  in
  match candidates with
  | first :: rest ->
      (* RFC 5545 selects the first occurrence of a repeated wall time. *)
      Ok
        (List.fold_left
           (fun earliest instant ->
             if Ptime.compare instant earliest < 0 then instant else earliest)
           first rest)
  | [] -> (
      (* RFC 5545 interprets a wall time in a forward-shift gap using the UTC
         offset in force immediately before the gap. *)
      let transitions = Timedesc.Time_zone.Raw.to_transitions tz in
      let rec prior_gap_offset = function
        | ((_, previous_end), (previous : Timedesc.Time_zone.entry))
          :: (((next_start, _), (next : Timedesc.Time_zone.entry)) as
              next_transition)
          :: rest ->
            let transition = max previous_end next_start in
            let local_before =
              Int64.add transition (Int64.of_int previous.offset)
            in
            let local_after = Int64.add transition (Int64.of_int next.offset) in
            if
              next.offset > previous.offset
              && Int64.compare local_seconds local_before >= 0
              && Int64.compare local_seconds local_after < 0
            then Some previous.offset
            else prior_gap_offset (next_transition :: rest)
        | _ -> None
      in
      match prior_gap_offset transitions with
      | Some offset -> instant_with_offset ~label local offset
      | None -> Error (`Nonexistent_local_time label))

let instant_of_local ~label ~tz local =
  let (year, month, day), hour, minute, second = wall_clock_parts local in
  match Timedesc.Date.Ymd.make ~year ~month ~day with
  | Error _ -> Error (`Out_of_range label)
  | Ok date -> (
      match Timedesc.Time.make ~hour ~minute ~second () with
      | Error _ -> Error (`Out_of_range label)
      | Ok time -> resolve_local_date_and_time ~label ~tz date time)

let instant_of_date ~tz (year, month, day) =
  let label = Printf.sprintf "%04d-%02d-%02d" year month day in
  match Ptime.of_date_time ((year, month, day), ((0, 0, 0), 0)) with
  | None -> Error (`Out_of_range label)
  | Some local -> instant_of_local ~label ~tz local

let ptime_of_ical_result ~floating_tz = function
  | `Datetime (`Utc instant) -> Ok instant
  | `Date date -> instant_of_date ~tz:floating_tz date
  | `Datetime (`Local local) ->
      instant_of_local ~label:"floating datetime" ~tz:floating_tz local
  | `Datetime (`With_tzid (local, (_, tzid))) -> (
      match Timedesc.Time_zone.make tzid with
      | None -> Error (`Unknown_timezone tzid)
      | Some tz -> instant_of_local ~label:tzid ~tz local)

let compare_ical_time ~floating_tz left right =
  match (left, right) with
  | `Date left, `Date right -> Ok (Stdlib.compare left right)
  | `Datetime (`Local left), `Datetime (`Local right)
  | `Datetime (`Utc left), `Datetime (`Utc right) ->
      Ok (Ptime.compare left right)
  | ( `Datetime (`With_tzid (left, (_, left_tzid))),
      `Datetime (`With_tzid (right, (_, right_tzid))) )
    when String.equal left_tzid right_tzid ->
      Ok (Ptime.compare left right)
  | _ ->
      let* left = ptime_of_ical_result ~floating_tz left in
      let* right = ptime_of_ical_result ~floating_tz right in
      Ok (Ptime.compare left right)

let instant_of_date_and_time ~tz ~label date time =
  resolve_local_date_and_time ~label ~tz date time

let midnight_of_date ~tz date =
  let label =
    Printf.sprintf "%04d-%02d-%02d" (Timedesc.Date.year date)
      (Timedesc.Date.month date) (Timedesc.Date.day date)
  in
  match Timedesc.Time.make ~hour:0 ~minute:0 ~second:0 () with
  | Error _ -> Error (`Out_of_range label)
  | Ok midnight -> instant_of_date_and_time ~tz ~label date midnight

let start_of_day_result ~tz instant =
  let* local = ptime_to_timedesc_result ~tz instant in
  midnight_of_date ~tz (Timedesc.date local)

let next_midnight_result ~tz instant =
  let* local = ptime_to_timedesc_result ~tz instant in
  let next_date = Timedesc.Date.add ~days:1 (Timedesc.date local) in
  midnight_of_date ~tz next_date

let today_result ~tz ~now = start_of_day_result ~tz now

let date_and_time_of_instant ~tz instant =
  let* local = ptime_to_timedesc_result ~tz instant in
  Ok (Timedesc.date local, Timedesc.time local)

let instant_preserving_time ~tz ~label date time =
  instant_of_date_and_time ~tz ~label date time

let add_days_result ~tz instant days =
  let* date, time = date_and_time_of_instant ~tz instant in
  let date = Timedesc.Date.add ~days date in
  instant_preserving_time ~tz ~label:"adding days" date time

let add_weeks_result ~tz instant weeks =
  if weeks > max_int / 7 || weeks < min_int / 7 then
    Error (`Out_of_range "adding weeks")
  else add_days_result ~tz instant (weeks * 7)

let add_months_result ~tz instant months =
  let* date, time = date_and_time_of_instant ~tz instant in
  let year = Timedesc.Date.year date in
  let month = Timedesc.Date.month date in
  let day = Timedesc.Date.day date in
  let total_month = (year * 12) + month - 1 + months in
  if total_month < 0 then Error (`Out_of_range "adding months")
  else
    let new_year = total_month / 12 in
    let new_month = (total_month mod 12) + 1 in
    let rec find_valid_day candidate =
      match
        Timedesc.Date.Ymd.make ~year:new_year ~month:new_month ~day:candidate
      with
      | Ok date -> instant_preserving_time ~tz ~label:"adding months" date time
      | Error _ when candidate > 1 -> find_valid_day (candidate - 1)
      | Error _ -> Error (`Out_of_range "adding months")
    in
    find_valid_day day

let add_years_result ~tz instant years =
  if years > max_int / 12 || years < min_int / 12 then
    Error (`Out_of_range "adding years")
  else add_months_result ~tz instant (years * 12)

let start_of_week_result ~tz instant =
  let* local = ptime_to_timedesc_result ~tz instant in
  let days_to_subtract =
    match Timedesc.weekday local with
    | `Mon -> 0
    | `Tue -> 1
    | `Wed -> 2
    | `Thu -> 3
    | `Fri -> 4
    | `Sat -> 5
    | `Sun -> 6
  in
  Timedesc.Date.sub ~days:days_to_subtract (Timedesc.date local)
  |> midnight_of_date ~tz

let start_of_month_result ~tz instant =
  let* local = ptime_to_timedesc_result ~tz instant in
  let year = Timedesc.year local in
  let month = Timedesc.month local in
  match Timedesc.Date.Ymd.make ~year ~month ~day:1 with
  | Error _ -> Error (`Out_of_range "start of month")
  | Ok date -> midnight_of_date ~tz date

let start_of_year_result ~tz instant =
  let* local = ptime_to_timedesc_result ~tz instant in
  let year = Timedesc.year local in
  match Timedesc.Date.Ymd.make ~year ~month:1 ~day:1 with
  | Error _ -> Error (`Out_of_range "start of year")
  | Ok date -> midnight_of_date ~tz date

let end_of_week_result ~tz instant =
  let* start = start_of_week_result ~tz instant in
  add_days_result ~tz start 6

let end_of_month_result ~tz instant =
  let* start = start_of_month_result ~tz instant in
  let* next = add_months_result ~tz start 1 in
  add_days_result ~tz next (-1)

let end_of_year_result ~tz instant =
  let* start = start_of_year_result ~tz instant in
  let* next = add_years_result ~tz start 1 in
  add_days_result ~tz next (-1)

let convert_relative_date_formats ~tz ~now ~today:today_flag ~tomorrow ~week
    ~month () =
  let* today_date = today_result ~tz ~now in
  if today_flag then Ok (Some (today_date, today_date))
  else if tomorrow then
    let* tomorrow_date = add_days_result ~tz today_date 1 in
    Ok (Some (tomorrow_date, tomorrow_date))
  else if week then
    let* week_start = start_of_week_result ~tz today_date in
    let* week_end = add_days_result ~tz week_start 6 in
    Ok (Some (week_start, week_end))
  else if month then
    let* month_start = start_of_month_result ~tz today_date in
    let* month_end = end_of_month_result ~tz month_start in
    Ok (Some (month_start, month_end))
  else Ok None

let conversion_message error = Error (`Msg (string_of_conversion_error error))

let boundary_result result =
  Result.map_error (fun error -> `Msg (string_of_conversion_error error)) result

let midnight_of_ymd ~tz ~label year month day =
  match Timedesc.Date.Ymd.make ~year ~month ~day with
  | Error _ -> Error (`Msg (Printf.sprintf "Invalid %s" label))
  | Ok date -> (
      match midnight_of_date ~tz date with
      | Ok instant -> Ok instant
      | Error error -> conversion_message error)

let parse_full_iso_date ~tz expression =
  let regex = Re.Pcre.regexp "^(\\d{4})-(\\d{1,2})-(\\d{1,2})$" in
  if Re.Pcre.pmatch ~rex:regex expression then
    let groups = Re.Pcre.exec ~rex:regex expression in
    let year = int_of_string (Re.Pcre.get_substring groups 1) in
    let month = int_of_string (Re.Pcre.get_substring groups 2) in
    let day = int_of_string (Re.Pcre.get_substring groups 3) in
    Some (midnight_of_ymd ~tz ~label:("date: " ^ expression) year month day)
  else None

let parse_year_only ~tz expression parameter =
  let regex = Re.Pcre.regexp "^(\\d{4})$" in
  if Re.Pcre.pmatch ~rex:regex expression then
    let groups = Re.Pcre.exec ~rex:regex expression in
    let year = int_of_string (Re.Pcre.get_substring groups 1) in
    let month, day = match parameter with `From -> (1, 1) | `To -> (12, 31) in
    Some (midnight_of_ymd ~tz ~label:("year: " ^ expression) year month day)
  else None

let parse_year_month ~tz expression parameter =
  let regex = Re.Pcre.regexp "^(\\d{4})-(\\d{1,2})$" in
  if Re.Pcre.pmatch ~rex:regex expression then
    let groups = Re.Pcre.exec ~rex:regex expression in
    let year = int_of_string (Re.Pcre.get_substring groups 1) in
    let month = int_of_string (Re.Pcre.get_substring groups 2) in
    let result =
      match parameter with
      | `From ->
          midnight_of_ymd ~tz ~label:("year-month: " ^ expression) year month 1
      | `To -> (
          let next_month = if month = 12 then 1 else month + 1 in
          let next_year = if month = 12 then year + 1 else year in
          match
            Timedesc.Date.Ymd.make ~year:next_year ~month:next_month ~day:1
          with
          | Error _ ->
              Error (`Msg (Printf.sprintf "Invalid year-month: %s" expression))
          | Ok next -> (
              match midnight_of_date ~tz (Timedesc.Date.sub ~days:1 next) with
              | Ok instant -> Ok instant
              | Error error -> conversion_message error))
    in
    Some result
  else None

let parse_relative ~tz ~today expression parameter =
  let regex = Re.Pcre.regexp "^([+-])(\\d+)([dwmy])$" in
  if Re.Pcre.pmatch ~rex:regex expression then
    let groups = Re.Pcre.exec ~rex:regex expression in
    let sign = Re.Pcre.get_substring groups 1 in
    let unit_name = Re.Pcre.get_substring groups 3 in
    let magnitude = Re.Pcre.get_substring groups 2 |> int_of_string_opt in
    let safe_limit =
      (* These exceed Ptime's complete supported civil-date range while also
         keeping all intermediate integer arithmetic bounded. *)
      match unit_name with
      | "d" -> 4_000_000
      | "w" -> 600_000
      | "m" -> 150_000
      | "y" -> 12_000
      | _ -> 0
    in
    let arithmetic_result =
      match magnitude with
      | None -> Error (`Out_of_range ("relative date " ^ expression))
      | Some magnitude when magnitude > safe_limit ->
          Error (`Out_of_range ("relative date " ^ expression))
      | Some magnitude -> (
          let amount =
            if String.equal sign "+" then magnitude else -magnitude
          in
          match unit_name with
          | "d" -> add_days_result ~tz today amount
          | "w" -> add_weeks_result ~tz today amount
          | "m" -> add_months_result ~tz today amount
          | "y" -> add_years_result ~tz today amount
          | _ -> Error (`Out_of_range ("date unit " ^ unit_name)))
    in
    let result =
      match arithmetic_result with
      | Error error -> conversion_message error
      | Ok date -> (
          match (unit_name, parameter) with
          | "w", `From -> boundary_result (start_of_week_result ~tz date)
          | "w", `To -> boundary_result (end_of_week_result ~tz date)
          | "m", `From -> boundary_result (start_of_month_result ~tz date)
          | "m", `To -> boundary_result (end_of_month_result ~tz date)
          | "y", `From -> boundary_result (start_of_year_result ~tz date)
          | "y", `To -> boundary_result (end_of_year_result ~tz date)
          | _ -> Ok date)
    in
    Some result
  else None

let parse_date ~tz ~now expression parameter =
  let* today =
    match today_result ~tz ~now with
    | Ok value -> Ok value
    | Error error -> conversion_message error
  in
  match expression with
  | "today" -> Ok today
  | "tomorrow" -> (
      match add_days_result ~tz today 1 with
      | Ok date -> Ok date
      | Error error -> conversion_message error)
  | "yesterday" -> (
      match add_days_result ~tz today (-1) with
      | Ok date -> Ok date
      | Error error -> conversion_message error)
  | "this-week" -> (
      match parameter with
      | `From -> boundary_result (start_of_week_result ~tz today)
      | `To -> boundary_result (end_of_week_result ~tz today))
  | "next-week" -> (
      let* start = boundary_result (start_of_week_result ~tz today) in
      let* next = boundary_result (add_days_result ~tz start 7) in
      match parameter with
      | `From -> Ok next
      | `To -> boundary_result (end_of_week_result ~tz next))
  | "this-month" -> (
      match parameter with
      | `From -> boundary_result (start_of_month_result ~tz today)
      | `To -> boundary_result (end_of_month_result ~tz today))
  | "next-month" -> (
      let* start = boundary_result (start_of_month_result ~tz today) in
      let* next = boundary_result (add_months_result ~tz start 1) in
      match parameter with
      | `From -> Ok next
      | `To -> boundary_result (end_of_month_result ~tz next))
  | _ -> (
      let ( |>? ) option fallback =
        match option with None -> fallback () | Some value -> Some value
      in
      ( ( ( parse_full_iso_date ~tz expression |>? fun () ->
            parse_year_only ~tz expression parameter )
        |>? fun () -> parse_year_month ~tz expression parameter )
      |>? fun () -> parse_relative ~tz ~today expression parameter )
      |> function
      | Some result -> result
      | None ->
          Error (`Msg (Printf.sprintf "Invalid date format: %s" expression)))

let parse_time value =
  try
    let regex =
      Re.Perl.compile_pat "^([0-9]{1,2}):([0-9]{1,2})(?::([0-9]{1,2}))?$"
    in
    match Re.exec_opt regex value with
    | None -> Error (`Msg "Invalid time format. Expected HH:MM or HH:MM:SS")
    | Some groups ->
        let hour = int_of_string (Re.Group.get groups 1) in
        let minute = int_of_string (Re.Group.get groups 2) in
        let second =
          try int_of_string (Re.Group.get groups 3) with Not_found -> 0
        in
        if hour > 23 then Error (`Msg (Printf.sprintf "Invalid hour: %d" hour))
        else if minute > 59 then
          Error (`Msg (Printf.sprintf "Invalid minute: %d" minute))
        else if second > 59 then
          Error (`Msg (Printf.sprintf "Invalid second: %d" second))
        else Ok (hour, minute, second)
  with exception_value ->
    Error
      (`Msg
         (Printf.sprintf "Error parsing time: %s"
            (Printexc.to_string exception_value)))

let parse_date_time ~tz ~now ~date ~time:time_string parameter =
  let* date_instant = parse_date ~tz ~now date parameter in
  let* hour, minute, second = parse_time time_string in
  let* local =
    match ptime_to_timedesc_result ~tz date_instant with
    | Ok value -> Ok value
    | Error error -> conversion_message error
  in
  match Timedesc.Time.make ~hour ~minute ~second () with
  | Error _ -> Error (`Msg "Invalid time for date-time combination")
  | Ok time -> (
      match
        instant_of_date_and_time ~tz
          ~label:(date ^ " " ^ time_string)
          (Timedesc.date local) time
      with
      | Ok instant -> Ok instant
      | Error error -> conversion_message error)
