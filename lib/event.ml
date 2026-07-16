open Icalendar

type t = {
  event : event;
  overrides : event list;
  date_until : Ptime.date option;
}

type recurrence_error = [ `Msg of string ]
type occurrence_origin = Generated | Persisted_override

type occurrence_reference = {
  series_uid : string;
  recurrence_id_property : Icalendar.params * Icalendar.date_or_datetime;
  occurrence_start : Ptime.t;
  query_timezone : Timedesc.Time_zone.t;
}

type occurrence = {
  effective_event : event;
  reference : occurrence_reference;
  origin : occurrence_origin;
}

let ( let* ) = Result.bind

let is_cleared_override_marker = function
  | `Xprop ((vendor, name), _, _) ->
      let vendor = String.uppercase_ascii vendor in
      let name = String.uppercase_ascii name in
      (String.equal vendor "CALEDONIA" && String.equal name "CLEARED")
      || (String.equal vendor "" && String.equal name "CALEDONIA-CLEARED")
  | _ -> false

let cleared_override_families props =
  List.filter_map
    (function
      | `Xprop (_, _, value) as property
        when is_cleared_override_marker property ->
          Some
            (String.split_on_char ',' value
            |> List.map (fun family ->
                String.trim family |> String.uppercase_ascii)
            |> List.filter (fun family -> family <> ""))
      | _ -> None)
    props
  |> List.flatten
  |> List.sort_uniq String.compare

let with_cleared_override_families families props =
  let props = List.filter (Fun.negate is_cleared_override_marker) props in
  match List.sort_uniq String.compare families with
  | [] -> props
  | families ->
      `Xprop (("CALEDONIA", "CLEARED"), Params.empty, String.concat "," families)
      :: props

let update_cleared_family family patch families =
  match patch with
  | Patch.Keep -> families
  | Patch.Clear ->
      if List.mem family families then families else family :: families
  | Patch.Set _ -> List.filter (Fun.negate (String.equal family)) families

let validate_start_end start end_ =
  let* () =
    Date.validate_date_or_datetime_params ~property:"VEVENT DTSTART" (fst start)
      (snd start)
  in
  let* () =
    match end_ with
    | None -> Ok ()
    | Some (`Duration (params, duration)) ->
        Date.validate_duration_params ~property:"VEVENT DURATION" params
          duration
    | Some (`Dtend (params, value)) ->
        Date.validate_date_or_datetime_params ~property:"VEVENT DTEND" params
          value
  in
  match end_ with
  | None -> Ok ()
  | Some (`Duration (_, duration)) ->
      if Ptime.Span.compare duration Ptime.Span.zero <= 0 then
        Error (`Msg "VEVENT DURATION must be greater than zero")
      else Ok ()
  | Some (`Dtend (_, end_value)) -> (
      let start_value = snd start in
      let same_value_type =
        match (start_value, end_value) with
        | `Date _, `Date _ | `Datetime _, `Datetime _ -> true
        | _ -> false
      in
      if not same_value_type then
        Error (`Msg "VEVENT DTSTART and DTEND must use the same value type")
      else
        match
          Date.compare_ical_time ~floating_tz:Timedesc.Time_zone.utc start_value
            end_value
        with
        | Ok comparison when comparison < 0 -> Ok ()
        | Ok _ -> Error (`Msg "VEVENT DTEND must be later than DTSTART")
        | Error error -> Error (`Msg (Date.string_of_conversion_error error)))

let standard_parameter_name : type value. value icalparameter -> string option =
  function
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

let is_private_date_until_parameter : type value. value icalparameter -> bool =
  function
  | Iana_param name ->
      String.equal (String.uppercase_ascii name) "X-CALEDONIA-DATE-UNTIL"
  | Xparam (vendor, name) ->
      String.equal (String.uppercase_ascii vendor) "CALEDONIA"
      && String.equal (String.uppercase_ascii name) "DATE-UNTIL"
  | _ -> false

let validate_extension_parameters ~property ~allowed params =
  let invalid name =
    Error
      (`Msg
         (Printf.sprintf "VEVENT %s does not allow the %s parameter" property
            name))
  in
  let rec validate = function
    | [] -> Ok ()
    | Params.B (Iana_param name, _) :: rest ->
        if is_standard_iana_parameter name then
          invalid (String.uppercase_ascii name)
        else validate rest
    | Params.B (Xparam _, _) :: rest -> validate rest
    | Params.B (parameter, _) :: rest ->
        let name = Option.get (standard_parameter_name parameter) in
        if List.mem name allowed then validate rest else invalid name
  in
  validate (Params.bindings params)

let timestamp_is_integral = function
  | `Utc timestamp | `Local timestamp ->
      Ptime.Span.compare (Ptime.frac_s timestamp) Ptime.Span.zero = 0

let validate_rrule_params params =
  let* () =
    if
      Params.bindings params
      |> List.exists (fun (Params.B (parameter, _)) ->
          is_private_date_until_parameter parameter)
    then
      Error
        (`Msg
           "VEVENT RRULE cannot retain the codec's private DATE UNTIL parameter")
    else Ok ()
  in
  validate_extension_parameters ~property:"RRULE" ~allowed:[] params

let validate_recur_parts dtstart frequency parts =
  let invalid message = Error (`Msg ("VEVENT RRULE " ^ message)) in
  let rule_part_name = function
    | `Bysecond _ -> "BYSECOND"
    | `Byminute _ -> "BYMINUTE"
    | `Byhour _ -> "BYHOUR"
    | `Byday _ -> "BYDAY"
    | `Bymonthday _ -> "BYMONTHDAY"
    | `Byyearday _ -> "BYYEARDAY"
    | `Byweek _ -> "BYWEEKNO"
    | `Bymonth _ -> "BYMONTH"
    | `Bysetposday _ -> "BYSETPOS"
    | `Weekday _ -> "WKST"
  in
  let validate_values name ~min ~max ~allow_zero values =
    if values = [] then invalid (name ^ " must not be empty")
    else if
      List.for_all
        (fun value ->
          value >= min && value <= max && (allow_zero || value <> 0))
        values
    then Ok ()
    else invalid (Printf.sprintf "%s contains an out-of-range value" name)
  in
  let validate_part = function
    | `Bysecond values ->
        validate_values "BYSECOND" ~min:0 ~max:60 ~allow_zero:true values
    | `Byminute values ->
        validate_values "BYMINUTE" ~min:0 ~max:59 ~allow_zero:true values
    | `Byhour values ->
        validate_values "BYHOUR" ~min:0 ~max:23 ~allow_zero:true values
    | `Byday values ->
        if values = [] then invalid "BYDAY must not be empty"
        else if
          List.for_all
            (fun (ordinal, _) -> ordinal >= -53 && ordinal <= 53)
            values
        then Ok ()
        else invalid "BYDAY contains an out-of-range ordinal"
    | `Bymonthday values ->
        validate_values "BYMONTHDAY" ~min:(-31) ~max:31 ~allow_zero:false values
    | `Byyearday values ->
        validate_values "BYYEARDAY" ~min:(-366) ~max:366 ~allow_zero:false
          values
    | `Byweek values ->
        validate_values "BYWEEKNO" ~min:(-53) ~max:53 ~allow_zero:false values
    | `Bymonth values ->
        validate_values "BYMONTH" ~min:1 ~max:12 ~allow_zero:false values
    | `Bysetposday values ->
        validate_values "BYSETPOS" ~min:(-366) ~max:366 ~allow_zero:false values
    | `Weekday _ -> Ok ()
  in
  let rec validate_unique seen = function
    | [] -> Ok ()
    | part :: rest ->
        let name = rule_part_name part in
        if List.mem name seen then invalid ("contains duplicate " ^ name)
        else
          let* () = validate_part part in
          validate_unique (name :: seen) rest
  in
  let has predicate = List.exists predicate parts in
  let* () = validate_unique [] parts in
  let* () =
    match dtstart with
    | `Date _
      when has (function
             | `Bysecond _ | `Byminute _ | `Byhour _ -> true
             | _ -> false) ->
        invalid "cannot use BYSECOND, BYMINUTE, or BYHOUR with a DATE DTSTART"
    | _ -> Ok ()
  in
  let* () =
    if has (function `Byweek _ -> true | _ -> false) && frequency <> `Yearly
    then invalid "BYWEEKNO is only valid with FREQ=YEARLY"
    else Ok ()
  in
  let* () =
    if
      has (function `Byyearday _ -> true | _ -> false)
      && List.mem frequency [ `Daily; `Weekly; `Monthly ]
    then invalid "BYYEARDAY is not valid with FREQ=DAILY, WEEKLY, or MONTHLY"
    else Ok ()
  in
  let* () =
    if
      has (function `Bymonthday _ -> true | _ -> false) && frequency = `Weekly
    then invalid "BYMONTHDAY is not valid with FREQ=WEEKLY"
    else Ok ()
  in
  let ordinal_byday =
    List.exists
      (function
        | `Byday values -> List.exists (fun (ordinal, _) -> ordinal <> 0) values
        | _ -> false)
      parts
  in
  let* () =
    if ordinal_byday && not (List.mem frequency [ `Monthly; `Yearly ]) then
      invalid "numeric BYDAY is only valid with FREQ=MONTHLY or YEARLY"
    else if
      ordinal_byday && frequency = `Yearly
      && has (function `Byweek _ -> true | _ -> false)
    then invalid "numeric BYDAY is not valid with FREQ=YEARLY and BYWEEKNO"
    else Ok ()
  in
  if
    has (function `Bysetposday _ -> true | _ -> false)
    && not
         (has (function
           | `Bysecond _ | `Byminute _ | `Byhour _ | `Byday _ | `Bymonthday _
           | `Byyearday _ | `Byweek _ | `Bymonth _ ->
               true
           | `Bysetposday _ | `Weekday _ -> false))
  then invalid "BYSETPOS requires another BY rule part"
  else Ok ()

let validate_recurrence_rule ~date_until dtstart rrule =
  match rrule with
  | None ->
      if Option.is_none date_until then Ok ()
      else Error (`Msg "VEVENT has DATE UNTIL metadata without an RRULE")
  | Some (params, (frequency, limit, interval, parts)) -> (
      let invalid message = Error (`Msg ("VEVENT RRULE " ^ message)) in
      let* () = validate_rrule_params params in
      let* () =
        match limit with
        | Some (`Count count) when count <= 0 ->
            invalid "COUNT must be greater than zero"
        | Some (`Until until) when not (timestamp_is_integral until) ->
            invalid "UNTIL must not contain fractional seconds"
        | None | Some (`Count _ | `Until _) -> Ok ()
      in
      let* () =
        match interval with
        | Some interval when interval <= 0 ->
            invalid "INTERVAL must be greater than zero"
        | None | Some _ -> Ok ()
      in
      let* () = validate_recur_parts dtstart frequency parts in
      match (dtstart, limit, date_until) with
      | _, None, None | _, Some (`Count _), None -> Ok ()
      | _, (None | Some (`Count _)), Some _ ->
          invalid "has DATE UNTIL metadata without an UNTIL limit"
      | `Date _, Some (`Until (`Utc until)), Some original_date -> (
          match Ptime.of_date original_date with
          | Some midnight when Ptime.equal midnight until -> Ok ()
          | Some _ | None ->
              invalid "DATE UNTIL metadata does not match its UNTIL value")
      | `Date _, Some (`Until _), Some _ ->
          invalid "has DATE UNTIL metadata for a non-UTC compatibility value"
      | `Date _, Some (`Until _), None ->
          invalid "UNTIL must retain its typed DATE metadata for a DATE DTSTART"
      | `Datetime (`Local _), Some (`Until (`Local _)), None -> Ok ()
      | `Datetime (`Local _), Some (`Until _), _ ->
          invalid "UNTIL must be floating when DTSTART is floating"
      | `Datetime (`Utc _ | `With_tzid _), Some (`Until (`Utc _)), None -> Ok ()
      | `Datetime (`Utc _ | `With_tzid _), Some (`Until _), _ ->
          invalid "UNTIL must be UTC when DTSTART is UTC or has a TZID")

let create ~now ~summary ~start ?end_ ?location ?description ?categories
    ?recurrence ?recurrence_params ?recurrence_date_until ?(alarms = []) () =
  let uuid = Fresh_id.generate () in
  let uid = (Params.empty, uuid) in
  let dtstart = start in
  let dtend_or_duration = end_ in
  let* () = validate_start_end dtstart dtend_or_duration in
  let* () = Alarm.validate_all alarms in
  let* () =
    Alarm.validate_references ~has_start:true
      ~has_end:(Option.is_some dtend_or_duration)
      alarms
  in
  let rrule =
    Option.map
      (fun r -> (Option.value recurrence_params ~default:Params.empty, r))
      recurrence
  in
  let* () =
    validate_recurrence_rule ~date_until:recurrence_date_until (snd dtstart)
      rrule
  in
  let props = [ `Summary (Params.empty, summary) ] in
  let props =
    match location with
    | Some loc -> `Location (Params.empty, loc) :: props
    | None -> props
  in
  let props =
    match description with
    | Some desc -> `Description (Params.empty, desc) :: props
    | None -> props
  in
  let props =
    match categories with
    | Some cats when cats <> [] -> `Categories (Params.empty, cats) :: props
    | _ -> props
  in
  let event =
    {
      dtstamp = (Params.empty, now);
      uid;
      dtstart;
      dtend_or_duration;
      rrule;
      props;
      alarms;
    }
  in
  Ok { event; overrides = []; date_until = recurrence_date_until }

let recurrence_id_property_of_event event =
  List.find_map
    (function
      | `Recur_id (params, recurrence_id) -> Some (params, recurrence_id)
      | _ -> None)
    event.props

let recurrence_id_of_event event =
  recurrence_id_property_of_event event |> Option.map snd

let event_has_recurrence_set event =
  Option.is_some event.rrule
  || List.exists
       (function
         | `Rdate (_, `Dates (_ :: _))
         | `Rdate (_, `Datetimes (_ :: _))
         | `Rdate (_, `Periods (_ :: _)) ->
             true
         | _ -> false)
       event.props

let same_temporal_kind left right =
  match (left, right) with
  | `Date _, `Date _ -> true
  | `Datetime (`Utc _), `Datetime (`Utc _) -> true
  | `Datetime (`Local _), `Datetime (`Local _) -> true
  | ( `Datetime (`With_tzid (_, (_, left_tzid))),
      `Datetime (`With_tzid (_, (_, right_tzid))) ) ->
      String.equal left_tzid right_tzid
  | _ -> false

let validate_recurrence_temporal_params ~property ~allow_range params value =
  let allowed =
    if allow_range then [ "VALUE"; "TZID"; "RANGE" ] else [ "VALUE"; "TZID" ]
  in
  let invalid message = Error (`Msg ("VEVENT " ^ property ^ " " ^ message)) in
  let* () = validate_extension_parameters ~property ~allowed params in
  let expected_value_type =
    match value with `Date _ -> `Date | `Datetime _ -> `Datetime
  in
  let* () =
    match (expected_value_type, Params.find Valuetype params) with
    | `Date, Some `Date | `Datetime, (None | Some `Datetime) -> Ok ()
    | `Date, _ -> invalid "requires VALUE=DATE for a DATE value"
    | `Datetime, Some _ ->
        invalid "has a VALUE parameter inconsistent with its typed value"
  in
  let* () =
    match (value, Params.find Tzid params) with
    | `Datetime (`With_tzid (_, embedded)), Some actual when embedded = actual
      ->
        Ok ()
    | `Datetime (`With_tzid _), None -> Ok ()
    | (`Date _ | `Datetime (`Utc _ | `Local _)), None -> Ok ()
    | `Datetime (`With_tzid _), Some _ ->
        invalid "has a TZID parameter inconsistent with its typed timezone"
    | (`Date _ | `Datetime (`Utc _ | `Local _)), Some _ ->
        invalid "has a TZID parameter on a DATE, UTC, or floating value"
  in
  match value with
  | `Date _ -> Ok ()
  | `Datetime (`Utc timestamp | `Local timestamp | `With_tzid (timestamp, _)) ->
      if Ptime.Span.compare (Ptime.frac_s timestamp) Ptime.Span.zero = 0 then
        Ok ()
      else invalid "must not contain fractional seconds"

let validate_recurrence_id_property props =
  let rec validate = function
    | [] -> Ok ()
    | `Recur_id (params, value) :: rest ->
        let* () =
          validate_recurrence_temporal_params ~property:"RECURRENCE-ID"
            ~allow_range:true params value
        in
        validate rest
    | _ :: rest -> validate rest
  in
  validate props

let recurrence_values_match_start (_, dtstart) props =
  let invalid property detail =
    Error
      (`Msg
         (Printf.sprintf "VEVENT %s %s DTSTART value kind and timezone" property
            detail))
  in
  let timestamps_match values =
    values <> []
    && List.for_all
         (fun timestamp -> same_temporal_kind dtstart (`Datetime timestamp))
         values
  in
  let rec validate_timestamps property params = function
    | [] -> Ok ()
    | timestamp :: rest ->
        let* () =
          validate_recurrence_temporal_params ~property ~allow_range:false
            params (`Datetime timestamp)
        in
        validate_timestamps property params rest
  in
  let rec validate = function
    | [] -> Ok ()
    | `Exdate (params, `Dates dates) :: rest -> (
        match dtstart with
        | `Date _ when dates <> [] ->
            let* () =
              validate_recurrence_temporal_params ~property:"EXDATE"
                ~allow_range:false params
                (`Date (List.hd dates))
            in
            validate rest
        | _ -> invalid "EXDATE" "must use the same")
    | `Exdate (params, `Datetimes timestamps) :: rest ->
        if timestamps_match timestamps then
          let* () = validate_timestamps "EXDATE" params timestamps in
          validate rest
        else invalid "EXDATE" "must use the same"
    | `Rdate (params, `Dates dates) :: rest -> (
        match dtstart with
        | `Date _ when dates <> [] ->
            let* () =
              validate_recurrence_temporal_params ~property:"RDATE"
                ~allow_range:false params
                (`Date (List.hd dates))
            in
            validate rest
        | _ -> invalid "RDATE" "must use the same")
    | `Rdate (params, `Datetimes timestamps) :: rest ->
        if timestamps_match timestamps then
          let* () = validate_timestamps "RDATE" params timestamps in
          validate rest
        else invalid "RDATE" "must use the same"
    | `Rdate (params, `Periods periods) :: rest -> (
        let valid_period (timestamp, duration, _) =
          same_temporal_kind dtstart (`Datetime timestamp)
          && timestamp_is_integral
               (match timestamp with
               | `Utc timestamp -> `Utc timestamp
               | `Local timestamp | `With_tzid (timestamp, _) ->
                   `Local timestamp)
          && Ptime.Span.compare duration Ptime.Span.zero > 0
          && Result.is_ok
               (Date.validate_integral_seconds ~property:"VEVENT RDATE PERIOD"
                  duration)
        in
        match dtstart with
        | `Datetime _
          when periods <> []
               && List.for_all valid_period periods
               && Params.find Valuetype params = Some `Period ->
            let* () =
              validate_extension_parameters ~property:"RDATE PERIOD"
                ~allowed:[ "VALUE"; "TZID" ] params
            in
            let rec validate_period_timezones = function
              | [] -> Ok ()
              | (timestamp, _, _) :: remaining ->
                  let* () =
                    match (timestamp, Params.find Tzid params) with
                    | `With_tzid (_, embedded), Some actual
                      when embedded = actual ->
                        Ok ()
                    | `With_tzid _, None -> Ok ()
                    | (`Utc _ | `Local _), None -> Ok ()
                    | `With_tzid _, Some _ ->
                        Error
                          (`Msg "VEVENT RDATE PERIOD has an inconsistent TZID")
                    | (`Utc _ | `Local _), Some _ ->
                        Error
                          (`Msg
                             "VEVENT RDATE PERIOD has a TZID on a UTC or \
                              floating value")
                  in
                  validate_period_timezones remaining
            in
            let* () = validate_period_timezones periods in
            validate rest
        | _ ->
            invalid "RDATE PERIOD"
              "must start with the same and have a positive duration")
    | _ :: rest -> validate rest
  in
  validate props

let recurrence_position = function
  | `Date date -> Ptime.of_date date
  | `Datetime (`Utc value | `Local value | `With_tzid (value, _)) -> Some value

let recurrence_id_is_member master recurrence_id =
  let target = Option.get (recurrence_position recurrence_id) in
  let generator = Icalendar.recur_events master in
  let rec search remaining =
    if remaining = 0 then
      Error
        (`Msg
           "VEVENT recurrence membership validation exceeded the \
            100000-instance safety limit")
    else
      match generator () with
      | None -> Ok false
      | Some generated ->
          let generated_start = snd generated.dtstart in
          if generated_start = recurrence_id then Ok true
          else
            let position = Option.get (recurrence_position generated_start) in
            if Ptime.compare position target > 0 then Ok false
            else search (remaining - 1)
  in
  search 100_000

let validate_override_structure uid master same_uid =
  let overrides =
    List.filter_map
      (fun candidate ->
        Option.map
          (fun recurrence_id -> (candidate, recurrence_id))
          (recurrence_id_of_event candidate))
      same_uid
  in
  let* () =
    if overrides <> [] && not (event_has_recurrence_set master) then
      Error
        (`Msg
           (Printf.sprintf
              "VEVENT UID %s has RECURRENCE-ID overrides but its master has no \
               RRULE or RDATE recurrence set"
              uid))
    else Ok ()
  in
  let rec loop seen = function
    | [] -> Ok ()
    | (candidate, recurrence_id) :: rest ->
        let has_this_and_future =
          List.exists
            (function
              | `Recur_id (params, _) ->
                  Option.is_some (Params.find Range params)
              | _ -> false)
            candidate.props
        in
        let has_override_recurrence_set =
          Option.is_some candidate.rrule
          || List.exists
               (function `Rdate _ | `Exdate _ -> true | _ -> false)
               candidate.props
        in
        if has_override_recurrence_set then
          Error
            (`Msg
               (Printf.sprintf
                  "VEVENT UID %s has an exact-instance override containing \
                   RRULE, RDATE, or EXDATE"
                  uid))
        else if has_this_and_future then
          Error
            (`Msg
               (Printf.sprintf
                  "VEVENT UID %s uses unsupported RECURRENCE-ID \
                   RANGE=THISANDFUTURE"
                  uid))
        else if not (same_temporal_kind (snd master.dtstart) recurrence_id) then
          Error
            (`Msg
               (Printf.sprintf
                  "VEVENT UID %s has a RECURRENCE-ID whose value kind does not \
                   match DTSTART"
                  uid))
        else if List.mem recurrence_id seen then
          Error
            (`Msg
               (Printf.sprintf
                  "VEVENT UID %s has duplicate RECURRENCE-ID overrides" uid))
        else
          let* is_member = recurrence_id_is_member master recurrence_id in
          if not is_member then
            Error
              (`Msg
                 (Printf.sprintf
                    "VEVENT UID %s has a RECURRENCE-ID that is not an active \
                     member of its master recurrence set"
                    uid))
          else loop (recurrence_id :: seen) rest
  in
  loop [] overrides

let validate_loaded_event ?(rrule_date_until = None) event =
  let singleton_name = function
    | `Class _ -> Some "CLASS"
    | `Created _ -> Some "CREATED"
    | `Description _ -> Some "DESCRIPTION"
    | `Geo _ -> Some "GEO"
    | `Lastmod _ -> Some "LAST-MODIFIED"
    | `Location _ -> Some "LOCATION"
    | `Organizer _ -> Some "ORGANIZER"
    | `Seq _ -> Some "SEQUENCE"
    | `Summary _ -> Some "SUMMARY"
    | `Url _ -> Some "URL"
    | `Recur_id _ -> Some "RECURRENCE-ID"
    | `Transparency _ -> Some "TRANSP"
    | _ -> None
  in
  let rec validate_singletons seen = function
    | [] -> Ok ()
    | property :: rest -> (
        match singleton_name property with
        | None -> validate_singletons seen rest
        | Some name when List.mem name seen ->
            Error (`Msg (Printf.sprintf "VEVENT contains duplicate %s" name))
        | Some name -> validate_singletons (name :: seen) rest)
  in
  let statuses =
    List.filter_map
      (function `Status (_, status) -> Some status | _ -> None)
      event.props
  in
  let priorities =
    List.filter_map
      (function `Priority (_, priority) -> Some priority | _ -> None)
      event.props
  in
  let* () = Property_validation.validate_event_properties event.props in
  let* () =
    if String.trim (snd event.uid) = "" then
      Error (`Msg "VEVENT UID must not be empty")
    else Ok ()
  in
  let* () =
    match statuses with
    | [] | [ (`Tentative | `Confirmed | `Cancelled) ] -> Ok ()
    | [ _ ] ->
        Error (`Msg "VEVENT STATUS must be TENTATIVE, CONFIRMED, or CANCELLED")
    | _ -> Error (`Msg "VEVENT contains duplicate STATUS properties")
  in
  let* () =
    match priorities with
    | [] -> Ok ()
    | [ priority ] when priority >= 0 && priority <= 9 -> Ok ()
    | [ _ ] -> Error (`Msg "VEVENT PRIORITY must be between 0 and 9")
    | _ -> Error (`Msg "VEVENT contains duplicate PRIORITY properties")
  in
  let* () = validate_singletons [] event.props in
  let* () = validate_start_end event.dtstart event.dtend_or_duration in
  let* () =
    validate_recurrence_rule ~date_until:rrule_date_until (snd event.dtstart)
      event.rrule
  in
  let* () = recurrence_values_match_start event.dtstart event.props in
  let* () = validate_recurrence_id_property event.props in
  let* () = Alarm.validate_all event.alarms in
  Alarm.validate_references ~has_start:true
    ~has_end:(Option.is_some event.dtend_or_duration)
    event.alarms

let validate_series_events (master : event) (overrides : event list) date_until
    =
  let uid = snd master.uid in
  let* () =
    match recurrence_id_of_event master with
    | None -> Ok ()
    | Some _ -> Error (`Msg (Printf.sprintf "VEVENT UID %s has no master" uid))
  in
  let* () =
    if
      List.for_all
        (fun (override : event) ->
          String.equal uid (snd override.uid)
          && Option.is_some (recurrence_id_of_event override))
        overrides
    then Ok ()
    else
      Error
        (`Msg
           (Printf.sprintf
              "VEVENT UID %s has an override with a mismatched UID or missing \
               RECURRENCE-ID"
              uid))
  in
  let rec validate_overrides = function
    | [] -> Ok ()
    | candidate :: rest ->
        let* () = validate_loaded_event candidate in
        validate_overrides rest
  in
  let* () = validate_loaded_event ~rrule_date_until:date_until master in
  let* () = validate_overrides overrides in
  validate_override_structure uid master (master :: overrides)

let make_series ~date_until ~master ~overrides =
  let* () = validate_series_events master overrides date_until in
  Ok { event = master; overrides; date_until }

let edit_patch ~now ?(summary = Patch.Keep) ?(start = Patch.Keep)
    ?(end_ = Patch.Keep) ?(location = Patch.Keep) ?(description = Patch.Keep)
    ?(categories = Patch.Keep) ?(recurrence = Patch.Keep) ?recurrence_params
    ?recurrence_date_until ?(alarms = Patch.Keep) t =
  if
    summary = Patch.Keep && start = Patch.Keep && end_ = Patch.Keep
    && location = Patch.Keep && description = Patch.Keep
    && categories = Patch.Keep && recurrence = Patch.Keep && alarms = Patch.Keep
  then Ok t
  else
    let uid = t.event.uid in
    let* dtstart =
      match start with
      | Patch.Keep -> Ok t.event.dtstart
      | Patch.Set value -> Ok value
      | Patch.Clear -> Error (`Msg "DTSTART is required and cannot be cleared")
    in
    let dtend_or_duration =
      match end_ with
      | Patch.Keep -> t.event.dtend_or_duration
      | Patch.Clear -> None
      | Patch.Set value -> Some value
    in
    let* () = validate_start_end dtstart dtend_or_duration in
    let final_alarms =
      match alarms with
      | Patch.Keep -> t.event.alarms
      | Patch.Clear -> []
      | Patch.Set value -> value
    in
    let* () = Alarm.validate_all final_alarms in
    let* () =
      Alarm.validate_references ~has_start:true
        ~has_end:(Option.is_some dtend_or_duration)
        final_alarms
    in
    let rrule =
      match recurrence with
      | Patch.Keep -> t.event.rrule
      | Patch.Clear -> None
      | Patch.Set value ->
          Some (Option.value recurrence_params ~default:Params.empty, value)
    in
    let rrule_date_until =
      match recurrence with
      | Patch.Keep -> t.date_until
      | Patch.Clear -> None
      | Patch.Set _ -> recurrence_date_until
    in
    let props =
      Patch.replace_in_list
        (function `Summary _ -> true | _ -> false)
        (fun value -> `Summary (Params.empty, value))
        summary t.event.props
    in
    let props =
      Patch.replace_in_list
        (function `Location _ -> true | _ -> false)
        (fun value -> `Location (Params.empty, value))
        location props
    in
    let props =
      Patch.replace_in_list
        (function `Description _ -> true | _ -> false)
        (fun value -> `Description (Params.empty, value))
        description props
    in
    let props =
      match categories with
      | Patch.Set [] ->
          List.filter (function `Categories _ -> false | _ -> true) props
      | patch ->
          Patch.replace_in_list
            (function `Categories _ -> true | _ -> false)
            (fun value -> `Categories (Params.empty, value))
            patch props
    in
    let props =
      match recurrence with
      | Patch.Keep -> props
      | Patch.Clear | Patch.Set _ ->
          List.filter
            (function `Exdate _ | `Rdate _ -> false | _ -> true)
            props
    in
    let event =
      {
        dtstamp = (Params.empty, now);
        uid;
        dtstart;
        dtend_or_duration;
        rrule;
        props;
        alarms = final_alarms;
      }
    in
    let* () = validate_loaded_event ~rrule_date_until event in
    let overrides =
      match recurrence with
      | Patch.Keep -> t.overrides
      | Patch.Clear | Patch.Set _ -> []
    in
    let* () = validate_series_events event overrides rrule_date_until in
    Ok { event; overrides; date_until = rrule_date_until }

let series_of_authored_events_result authored =
  let events = List.map fst authored in
  let uids =
    List.sort_uniq String.compare (List.map (fun event -> snd event.uid) events)
  in
  let rec collect acc = function
    | [] -> Ok (List.rev acc)
    | uid :: rest -> (
        let same_uid =
          List.filter
            (fun (event, _) -> String.equal uid (snd event.uid))
            authored
        in
        let masters =
          List.filter
            (fun (event, _) -> recurrence_id_of_event event = None)
            same_uid
        in
        match masters with
        | [ (master, master_date_until) ] ->
            let overrides =
              List.filter
                (fun (candidate, _) ->
                  Option.is_some (recurrence_id_of_event candidate))
                same_uid
            in
            let* () =
              if
                List.for_all
                  (fun (_, date_until) -> date_until = None)
                  overrides
              then Ok ()
              else
                Error
                  (`Msg
                     "VEVENT RECURRENCE-ID override cannot retain DATE UNTIL \
                      metadata")
            in
            let override_events = List.map fst overrides in
            let* series =
              make_series ~date_until:master_date_until ~master
                ~overrides:override_events
            in
            collect (series :: acc) rest
        | [] ->
            Error
              (`Msg
                 (Printf.sprintf
                    "VEVENT UID %s has overrides but no recurrence master" uid))
        | _ ->
            Error
              (`Msg
                 (Printf.sprintf "VEVENT UID %s has multiple recurrence masters"
                    uid)))
  in
  collect [] uids

let series_of_events_result events =
  events
  |> List.map (fun event -> (event, None))
  |> series_of_authored_events_result

let get_id t = snd t.event.uid

let get_ical_summary event =
  match
    List.filter_map
      (function `Summary (_, s) when s <> "" -> Some s | _ -> None)
      event.props
  with
  | s :: _ -> Some s
  | _ -> None

let get_summary t = get_ical_summary t.event

let get_ical_start_result ~floating_tz event =
  Date.ptime_of_ical_result ~floating_tz (snd event.dtstart)

let get_start_result ~floating_tz t = get_ical_start_result ~floating_tz t.event

let get_ical_end_result ~floating_tz event =
  let open Result in
  match event.dtend_or_duration with
  | None -> Ok None
  | Some (`Dtend (_, value)) ->
      Date.ptime_of_ical_result ~floating_tz value |> map Option.some
  | Some (`Duration (_, span)) ->
      bind (get_ical_start_result ~floating_tz event) (fun start ->
          match Ptime.add_span start span with
          | Some end_ -> Ok (Some end_)
          | None -> Error (`Out_of_range "event duration"))

let get_end_result ~floating_tz t = get_ical_end_result ~floating_tz t.event

let get_ical_start_timezone event =
  match event.dtstart with
  | _, `Datetime (`With_tzid (_, (_, tzid))) -> Some tzid
  | _, `Datetime (`Utc _) -> Some "UTC"
  | _ -> None

let get_start_timezone t = get_ical_start_timezone t.event

let get_ical_end_timezone event =
  match event.dtend_or_duration with
  | Some (`Dtend (_, `Datetime (`With_tzid (_, (_, tzid))))) -> Some tzid
  | Some (`Dtend (_, `Datetime (`Utc _))) -> Some "UTC"
  | _ -> None

let get_end_timezone t = get_ical_end_timezone t.event

let ical_event_is_date event =
  match (event.dtstart, event.dtend_or_duration) with
  | (_, `Date _), _ -> true
  | _, Some (`Dtend (_, `Date _)) -> true
  | _ -> false

let is_date t = ical_event_is_date t.event

let get_ical_location event =
  match
    List.filter_map
      (function `Location (_, s) when s <> "" -> Some s | _ -> None)
      event.props
  with
  | s :: _ -> Some s
  | _ -> None

let get_location t = get_ical_location t.event

let get_ical_description event =
  match
    List.filter_map
      (function `Description (_, s) when s <> "" -> Some s | _ -> None)
      event.props
  with
  | s :: _ -> Some s
  | _ -> None

let get_description t = get_ical_description t.event

let get_ical_categories event =
  List.filter_map
    (function `Categories (_, cats) -> Some cats | _ -> None)
    event.props
  |> List.flatten

let get_categories t = get_ical_categories t.event
let get_recurrence t = Option.map (fun r -> snd r) t.event.rrule
let has_recurrence_set t = event_has_recurrence_set t.event
let get_alarms t = t.event.alarms
let validated_overrides event = Ok event.overrides

let recurrence_conversion_error error =
  `Msg (Date.string_of_conversion_error error)

let ical_event_overlaps_result ~floating_tz ~from ~to_ event =
  let* start =
    get_ical_start_result ~floating_tz event
    |> Result.map_error recurrence_conversion_error
  in
  let* end_opt =
    get_ical_end_result ~floating_tz event
    |> Result.map_error recurrence_conversion_error
  in
  let end_ = Option.value end_opt ~default:start in
  let starts_before_end = Ptime.compare start to_ < 0 in
  let after_start =
    match from with
    | None -> true
    | Some lower when Ptime.equal start end_ -> Ptime.compare start lower >= 0
    | Some lower -> Ptime.compare end_ lower > 0
  in
  Ok (starts_before_end && after_start)

let materialize_generated_occurrence event =
  match recurrence_id_of_event event with
  | Some _ -> event
  | None ->
      let recurrence_params = fst event.dtstart in
      let recurrence_id = snd event.dtstart in
      let props =
        List.filter
          (function `Recur_id _ | `Exdate _ | `Rdate _ -> false | _ -> true)
          event.props
      in
      {
        event with
        rrule = None;
        props = `Recur_id (recurrence_params, recurrence_id) :: props;
      }

let event_property_family = function
  | `Dtstamp _ -> "DTSTAMP"
  | `Uid _ -> "UID"
  | `Dtstart _ -> "DTSTART"
  | `Class _ -> "CLASS"
  | `Created _ -> "CREATED"
  | `Description _ -> "DESCRIPTION"
  | `Geo _ -> "GEO"
  | `Lastmod _ -> "LAST-MODIFIED"
  | `Location _ -> "LOCATION"
  | `Organizer _ -> "ORGANIZER"
  | `Priority _ -> "PRIORITY"
  | `Seq _ -> "SEQUENCE"
  | `Status _ -> "STATUS"
  | `Summary _ -> "SUMMARY"
  | `Url _ -> "URL"
  | `Recur_id _ -> "RECURRENCE-ID"
  | `Rrule _ -> "RRULE"
  | `Duration _ -> "DURATION"
  | `Attach _ -> "ATTACH"
  | `Attendee _ -> "ATTENDEE"
  | `Categories _ -> "CATEGORIES"
  | `Comment _ -> "COMMENT"
  | `Contact _ -> "CONTACT"
  | `Exdate _ -> "EXDATE"
  | `Rstatus _ -> "REQUEST-STATUS"
  | `Related _ -> "RELATED-TO"
  | `Resource _ -> "RESOURCES"
  | `Rdate _ -> "RDATE"
  | `Transparency _ -> "TRANSP"
  | `Dtend _ -> "DTEND"
  | `Iana_prop (name, _, _) -> "IANA:" ^ String.uppercase_ascii name
  | `Xprop ((namespace, name), _, _) ->
      "X:"
      ^ String.uppercase_ascii namespace
      ^ ":"
      ^ String.uppercase_ascii name

let inherited_override_props event =
  List.filter
    (function
      | `Recur_id _ | `Exdate _ | `Rdate _ -> false
      | property when is_cleared_override_marker property -> false
      | _ -> true)
    event.props

let inherited_props_with_override_fallback master override =
  let master_props = inherited_override_props master in
  let override_props = inherited_override_props override in
  let cleared_families = cleared_override_families override.props in
  let overridden_families =
    List.map event_property_family override_props
    |> List.sort_uniq String.compare
  in
  let inherited_master_props =
    List.filter
      (fun property ->
        let family = event_property_family property in
        not
          (List.mem family overridden_families
          || List.mem family cleared_families))
      master_props
  in
  override_props @ inherited_master_props

let utc_to_local_ptime_result tz occurrence =
  let* local = Date.ptime_to_timedesc_result ~tz occurrence in
  match
    Ptime.of_date_time
      ( (Timedesc.year local, Timedesc.month local, Timedesc.day local),
        ((Timedesc.hour local, Timedesc.minute local, Timedesc.second local), 0)
      )
  with
  | Some value -> Ok value
  | None -> Error (`Out_of_range "converting occurrence to local wall time")

let make_timestamp_matching_dtstart_result ~date_tz dtstart occurrence =
  match dtstart with
  | `Datetime (`Utc _) -> Ok (`Utc occurrence)
  | `Datetime (`Local _) ->
      let* local =
        utc_to_local_ptime_result date_tz occurrence
        |> Result.map_error recurrence_conversion_error
      in
      Ok (`Local local)
  | `Datetime (`With_tzid (_, (params, tzid))) -> (
      match Timedesc.Time_zone.make tzid with
      | Some tz ->
          let* local =
            utc_to_local_ptime_result tz occurrence
            |> Result.map_error recurrence_conversion_error
          in
          Ok (`With_tzid (local, (params, tzid)))
      | None -> Error (`Msg (Printf.sprintf "unknown timezone %s" tzid)))
  | `Date _ -> Error (`Msg "a DATE occurrence is not a datetime")

let local_date_in_timezone_result ~tz instant =
  Date.ptime_to_timedesc_result ~tz instant
  |> Result.map (fun local ->
      (Timedesc.year local, Timedesc.month local, Timedesc.day local))
  |> Result.map_error recurrence_conversion_error

let date_params params = Params.add Valuetype `Date params

let inherited_end_for_start_result ~floating_tz master target_start =
  match master.dtend_or_duration with
  | None -> Ok None
  | Some (`Duration _ as duration) -> Ok (Some duration)
  | Some (`Dtend (params, `Date original_end)) -> (
      match (snd master.dtstart, snd target_start) with
      | `Date original_start, `Date target_date ->
          let midnight date =
            Option.get (Ptime.of_date_time (date, ((0, 0, 0), 0)))
          in
          let days, _ =
            Ptime.diff (midnight original_end) (midnight original_start)
            |> Ptime.Span.to_d_ps
          in
          let year, month, day = target_date in
          let shifted =
            Timedesc.Date.Ymd.make_exn ~year ~month ~day
            |> Timedesc.Date.add ~days
          in
          Ok
            (Some
               (`Dtend
                  ( date_params params,
                    `Date
                      ( Timedesc.Date.year shifted,
                        Timedesc.Date.month shifted,
                        Timedesc.Date.day shifted ) )))
      | _ -> Error (`Msg "DTSTART and DTEND use different temporal value kinds")
      )
  | Some (`Dtend (params, (`Datetime _ as original_end_value))) -> (
      match snd target_start with
      | `Date _ ->
          Error (`Msg "DTSTART and DTEND use different temporal value kinds")
      | `Datetime _ ->
          let* original_start =
            get_ical_start_result ~floating_tz master
            |> Result.map_error recurrence_conversion_error
          in
          let* original_end =
            Date.ptime_of_ical_result ~floating_tz original_end_value
            |> Result.map_error recurrence_conversion_error
          in
          let* target_instant =
            Date.ptime_of_ical_result ~floating_tz (snd target_start)
            |> Result.map_error recurrence_conversion_error
          in
          let duration = Ptime.diff original_end original_start in
          let* shifted =
            match Ptime.add_span target_instant duration with
            | Some shifted -> Ok shifted
            | None -> Error (`Msg "occurrence end is out of range")
          in
          let* timestamp =
            make_timestamp_matching_dtstart_result ~date_tz:floating_tz
              original_end_value shifted
          in
          Ok (Some (`Dtend (params, `Datetime timestamp))))

let effective_override_result ~floating_tz master override =
  let cleared_families = cleared_override_families override.props in
  let* recurrence_property =
    match
      List.find_opt (function `Recur_id _ -> true | _ -> false) override.props
    with
    | Some property -> Ok property
    | None -> Error (`Msg "persisted override is missing RECURRENCE-ID")
  in
  let props =
    recurrence_property
    :: (inherited_props_with_override_fallback master override
       |> with_cleared_override_families cleared_families)
  in
  let* dtend_or_duration =
    match override.dtend_or_duration with
    | Some _ as explicit -> Ok explicit
    | None when List.mem "END" cleared_families -> Ok None
    | None ->
        inherited_end_for_start_result ~floating_tz master override.dtstart
  in
  let alarms =
    match override.alarms with
    | _ :: _ as alarms -> alarms
    | [] when List.mem "VALARM" cleared_families -> []
    | [] -> master.alarms
  in
  let effective =
    { override with props; dtend_or_duration; rrule = None; alarms }
  in
  let* () = validate_loaded_event effective in
  Ok effective

let effective_generated_occurrence_result ~floating_tz master generated =
  let* recurrence_property =
    match
      List.find_opt
        (function `Recur_id _ -> true | _ -> false)
        generated.props
    with
    | Some property -> Ok property
    | None -> Error (`Msg "generated recurrence is missing RECURRENCE-ID")
  in
  let* dtend_or_duration =
    inherited_end_for_start_result ~floating_tz master generated.dtstart
  in
  let effective =
    {
      master with
      dtstart = generated.dtstart;
      dtend_or_duration;
      rrule = None;
      props = recurrence_property :: inherited_override_props master;
    }
  in
  let* () = validate_loaded_event effective in
  Ok effective

let make_occurrence ~series_uid ~effective_event ~recurrence_id_property
    ~occurrence_start ~query_timezone ~origin =
  {
    effective_event;
    reference =
      { series_uid; recurrence_id_property; occurrence_start; query_timezone };
    origin;
  }

let expand_occurrences_result ?(max_instances = 100_000) ~floating_tz ~from ~to_
    event =
  if max_instances <= 0 then
    Error (`Msg "recurrence expansion limit must be positive")
  else if
    match from with Some lower -> Ptime.compare lower to_ >= 0 | None -> false
  then Ok []
  else
    let rule = if has_recurrence_set event then Some () else None in
    match rule with
    | None -> Ok []
    | Some _ ->
        let* overrides = validated_overrides event in
        let work_remaining = ref max_instances in
        let next generator =
          if !work_remaining = 0 then
            Error
              (`Msg
                 (Printf.sprintf
                    "recurrence expansion exceeded the %d-instance safety limit"
                    max_instances))
          else (
            decr work_remaining;
            Ok (generator ()))
        in
        let cancelled override =
          List.exists
            (function `Status (_, `Cancelled) -> true | _ -> false)
            override.props
        in
        let override_for recurrence_id =
          List.find_opt
            (fun override ->
              recurrence_id_of_event override = Some recurrence_id)
            overrides
        in
        let master_generator () = recur_events event.event in
        let rec collect generator acc =
          let* generated = next generator in
          match generated with
          | None -> Ok (List.rev acc)
          | Some recur -> (
              let recur = materialize_generated_occurrence recur in
              let* recurrence_id_property, recurrence_identity =
                match recurrence_id_property_of_event recur with
                | None ->
                    Error (`Msg "generated recurrence is missing its identity")
                | Some ((_, recurrence_id) as property) ->
                    let* identity =
                      Date.ptime_of_ical_result ~floating_tz recurrence_id
                      |> Result.map_error recurrence_conversion_error
                    in
                    Ok (property, identity)
              in
              if Ptime.compare recurrence_identity to_ >= 0 then
                Ok (List.rev acc)
              else
                let* recur =
                  match override_for (snd recurrence_id_property) with
                  | Some override when cancelled override -> Ok None
                  | Some override ->
                      effective_override_result ~floating_tz event.event
                        override
                      |> Result.map (fun effective ->
                          let reference_property =
                            recurrence_id_property_of_event override
                            |> Option.get
                          in
                          Some
                            (effective, Persisted_override, reference_property))
                  | None ->
                      effective_generated_occurrence_result ~floating_tz
                        event.event recur
                      |> Result.map (fun effective ->
                          Some (effective, Generated, recurrence_id_property))
                in
                match recur with
                | None -> collect generator acc
                | Some (recur, origin, reference_property) ->
                    let occurrence =
                      make_occurrence ~series_uid:(get_id event)
                        ~effective_event:recur
                        ~recurrence_id_property:reference_property
                        ~occurrence_start:recurrence_identity
                        ~query_timezone:floating_tz ~origin
                    in
                    let* overlaps =
                      ical_event_overlaps_result ~floating_tz ~from ~to_ recur
                    in
                    if overlaps then collect generator (occurrence :: acc)
                    else collect generator acc)
        in
        let* ordinary = collect (master_generator ()) [] in
        let rec identity_is_active target generator =
          let* generated = next generator in
          match generated with
          | None -> Ok false
          | Some generated ->
              let* identity =
                Date.ptime_of_ical_result ~floating_tz (snd generated.dtstart)
                |> Result.map_error recurrence_conversion_error
              in
              let comparison = Ptime.compare identity target in
              if comparison = 0 then Ok true
              else if comparison > 0 then Ok false
              else identity_is_active target generator
        in
        let rec collect_future_moved acc = function
          | [] -> Ok (List.rev acc)
          | override :: rest ->
              let recurrence_id_property =
                match recurrence_id_property_of_event override with
                | Some property -> property
                | None -> assert false
              in
              let* identity =
                Date.ptime_of_ical_result ~floating_tz
                  (snd recurrence_id_property)
                |> Result.map_error recurrence_conversion_error
              in
              if Ptime.compare identity to_ < 0 || cancelled override then
                collect_future_moved acc rest
              else
                let* effective =
                  effective_override_result ~floating_tz event.event override
                in
                let occurrence =
                  make_occurrence ~series_uid:(get_id event)
                    ~effective_event:effective ~recurrence_id_property
                    ~occurrence_start:identity ~query_timezone:floating_tz
                    ~origin:Persisted_override
                in
                let* overlaps =
                  ical_event_overlaps_result ~floating_tz ~from ~to_ effective
                in
                if not overlaps then collect_future_moved acc rest
                else
                  let* active =
                    identity_is_active identity (master_generator ())
                  in
                  collect_future_moved
                    (if active then occurrence :: acc else acc)
                    rest
        in
        let* moved = collect_future_moved [] overrides in
        let* decorated =
          List.fold_left
            (fun result occurrence ->
              let* accumulated = result in
              let* start =
                get_ical_start_result ~floating_tz occurrence.effective_event
                |> Result.map_error recurrence_conversion_error
              in
              Ok ((start, occurrence) :: accumulated))
            (Ok []) (ordinary @ moved)
        in
        Ok
          (List.stable_sort
             (fun (left, _) (right, _) -> Ptime.compare left right)
             decorated
          |> List.map snd)

let make_exdate_value_result ~date_tz
    (dtstart : Icalendar.Params.t * Icalendar.date_or_datetime)
    (occurrence : Ptime.t) =
  match snd dtstart with
  | `Date _ ->
      let* date = local_date_in_timezone_result ~tz:date_tz occurrence in
      Ok (`Exdate (date_params Icalendar.Params.empty, `Dates [ date ]))
  | `Datetime _ ->
      let* timestamp =
        make_timestamp_matching_dtstart_result ~date_tz (snd dtstart) occurrence
      in
      Ok (`Exdate (Icalendar.Params.empty, `Datetimes [ timestamp ]))

let occurrence_overrides ~floating_tz t occurrence =
  let matches candidate =
    match recurrence_id_of_event candidate with
    | None -> false
    | Some recurrence_id -> (
        match Date.ptime_of_ical_result ~floating_tz recurrence_id with
        | Ok instant -> Ptime.equal instant occurrence
        | Error _ -> false)
  in
  List.filter matches t.overrides

let event_is_cancelled (event : Icalendar.event) =
  List.exists
    (function `Status (_, `Cancelled) -> true | _ -> false)
    event.props

let recurrence_id_value_result ~floating_tz
    (dtstart : Icalendar.Params.t * Icalendar.date_or_datetime)
    (occurrence : Ptime.t) =
  match snd dtstart with
  | `Date _ ->
      let* date = local_date_in_timezone_result ~tz:floating_tz occurrence in
      Ok (`Date date)
  | `Datetime _ ->
      let* timestamp =
        make_timestamp_matching_dtstart_result ~date_tz:floating_tz
          (snd dtstart) occurrence
      in
      Ok (`Datetime timestamp)

let validate_active_occurrence ~floating_tz t occurrence =
  if not (has_recurrence_set t) then Error (`Msg "event is not recurring")
  else
    match occurrence_overrides ~floating_tz t occurrence with
    | _ :: _ :: _ -> Error (`Msg "occurrence has ambiguous duplicate overrides")
    | [ override ] when event_is_cancelled override ->
        Error (`Msg "requested recurrence is cancelled and no longer active")
    | [ _ ] | [] -> (
        let master_only = { t with overrides = [] } in
        let second = Ptime.Span.of_int_s 1 in
        let from = Ptime.sub_span occurrence second in
        let to_ = Ptime.add_span occurrence second in
        match (from, to_) with
        | Some from, Some to_ ->
            let* occurrences =
              expand_occurrences_result ~floating_tz ~from:(Some from) ~to_
                master_only
            in
            if
              List.exists
                (fun candidate ->
                  match
                    get_ical_start_result ~floating_tz candidate.effective_event
                  with
                  | Ok start -> Ptime.equal start occurrence
                  | Error _ -> false)
                occurrences
            then Ok ()
            else
              Error
                (`Msg
                   "requested recurrence is not an active member of this series")
        | _ -> Error (`Msg "occurrence timestamp is out of range"))

let resolve_occurrence_reference ~floating_tz t occurrence_start =
  let* () = validate_active_occurrence ~floating_tz t occurrence_start in
  let* recurrence_id =
    recurrence_id_value_result ~floating_tz t.event.dtstart occurrence_start
  in
  Ok
    {
      series_uid = get_id t;
      recurrence_id_property = (fst t.event.dtstart, recurrence_id);
      occurrence_start;
      query_timezone = floating_tz;
    }

let validate_occurrence_reference t reference =
  let* () =
    if String.equal reference.series_uid (get_id t) then Ok ()
    else Error (`Msg "occurrence reference belongs to a different event series")
  in
  let* expected_recurrence_id =
    recurrence_id_value_result ~floating_tz:reference.query_timezone
      t.event.dtstart reference.occurrence_start
  in
  let* () =
    if expected_recurrence_id = snd reference.recurrence_id_property then Ok ()
    else
      Error
        (`Msg
           "occurrence reference identity does not match the target series or \
            query timezone")
  in
  validate_active_occurrence ~floating_tz:reference.query_timezone t
    reference.occurrence_start

let validate_occurrence_override t reference candidate =
  let* () = validate_occurrence_reference t reference in
  let* () =
    if String.equal (snd candidate.uid) (get_id t) then Ok ()
    else Error (`Msg "occurrence override UID does not match its master")
  in
  let* recurrence_id =
    match recurrence_id_of_event candidate with
    | Some recurrence_id -> Ok recurrence_id
    | None -> Error (`Msg "occurrence override requires RECURRENCE-ID")
  in
  let* () =
    if recurrence_id = snd reference.recurrence_id_property then Ok ()
    else
      Error
        (`Msg
           "occurrence override RECURRENCE-ID does not match its selected \
            occurrence reference")
  in
  let* () =
    if same_temporal_kind (snd t.event.dtstart) recurrence_id then Ok ()
    else
      Error
        (`Msg
           "occurrence override RECURRENCE-ID value kind does not match DTSTART")
  in
  let* () =
    if
      Option.is_some candidate.rrule
      || List.exists
           (function `Rdate _ | `Exdate _ -> true | _ -> false)
           candidate.props
    then
      Error (`Msg "occurrence override cannot contain RRULE, RDATE, or EXDATE")
    else Ok ()
  in
  let* () =
    if
      List.exists
        (function
          | `Recur_id (params, _) -> Option.is_some (Params.find Range params)
          | _ -> false)
        candidate.props
    then
      Error
        (`Msg "occurrence override cannot use RECURRENCE-ID RANGE=THISANDFUTURE")
    else Ok ()
  in
  let* () = validate_loaded_event candidate in
  Ok ()

let delete_occurrence_checked t reference =
  let* () = validate_occurrence_reference t reference in
  let date_tz = reference.query_timezone in
  let occurrence = reference.occurrence_start in
  let* new_exdate =
    make_exdate_value_result ~date_tz t.event.dtstart occurrence
  in
  (* Merge into existing EXDATE if present, otherwise create new one *)
  let found = ref false in
  let props =
    List.map
      (function
        | `Exdate (params, `Datetimes existing) when not !found -> (
            match new_exdate with
            | `Exdate (_, `Datetimes [ timestamp ]) ->
                found := true;
                let values =
                  if List.mem timestamp existing then existing
                  else existing @ [ timestamp ]
                in
                `Exdate (params, `Datetimes values)
            | _ -> `Exdate (params, `Datetimes existing))
        | `Exdate (params, `Dates existing) when not !found -> (
            match new_exdate with
            | `Exdate (_, `Dates [ date ]) ->
                found := true;
                let values =
                  if List.mem date existing then existing
                  else existing @ [ date ]
                in
                `Exdate (date_params params, `Dates values)
            | _ -> `Exdate (params, `Dates existing))
        | other -> other)
      t.event.props
  in
  let props = if !found then props else new_exdate :: props in
  let event = { t.event with props } in
  let removed_overrides =
    occurrence_overrides ~floating_tz:date_tz t occurrence
  in
  let overrides =
    List.filter
      (fun candidate -> not (List.mem candidate removed_overrides))
      t.overrides
  in
  let* () = validate_series_events event overrides t.date_until in
  Ok { event; overrides; date_until = t.date_until }

let occurrence_start_value ~date_tz dtstart occurrence =
  match snd dtstart with
  | `Date _ ->
      let* date = local_date_in_timezone_result ~tz:date_tz occurrence in
      Ok (date_params (fst dtstart), `Date date)
  | `Datetime _ ->
      let* timestamp =
        make_timestamp_matching_dtstart_result ~date_tz (snd dtstart) occurrence
      in
      Ok (fst dtstart, `Datetime timestamp)

let create_occurrence_override_checked ~now t reference ?(summary = Patch.Keep)
    ?(start = Patch.Keep) ?(end_ = Patch.Keep) ?(location = Patch.Keep)
    ?(description = Patch.Keep) ?(categories = Patch.Keep)
    ?(alarms = Patch.Keep) () =
  let* () = validate_occurrence_reference t reference in
  let date_tz = reference.query_timezone in
  let occurrence = reference.occurrence_start in
  let existing = occurrence_overrides ~floating_tz:date_tz t occurrence in
  let existing_cleared_families =
    match existing with
    | [ override ] -> cleared_override_families override.props
    | [] -> []
    | _ -> assert false
  in
  let base_event =
    match existing with
    | [ override ] -> override
    | [] -> t.event
    | _ -> assert false
  in
  let recurrence_id = `Recur_id reference.recurrence_id_property in
  let* occurrence_start =
    occurrence_start_value ~date_tz t.event.dtstart occurrence
  in
  let default_start =
    match existing with
    | [ _ ] -> base_event.dtstart
    | [] -> occurrence_start
    | _ -> assert false
  in
  let* dtstart =
    match start with
    | Patch.Keep -> Ok default_start
    | Patch.Set value -> Ok value
    | Patch.Clear -> Error (`Msg "DTSTART is required and cannot be cleared")
  in
  let* dtend_or_duration =
    match end_ with
    | Patch.Set value -> Ok (Some value)
    | Patch.Clear -> Ok None
    | Patch.Keep -> (
        match existing with
        | [ _ ] -> (
            match base_event.dtend_or_duration with
            | Some _ as value -> Ok value
            | None when List.mem "END" existing_cleared_families -> Ok None
            | None ->
                inherited_end_for_start_result ~floating_tz:date_tz t.event
                  dtstart)
        | [] ->
            inherited_end_for_start_result ~floating_tz:date_tz t.event dtstart
        | _ -> assert false)
  in
  let props =
    recurrence_id
    ::
    (match existing with
    | [ override ] -> inherited_props_with_override_fallback t.event override
    | [] -> inherited_override_props t.event
    | _ -> assert false)
  in
  let props =
    Patch.replace_in_list
      (function `Summary _ -> true | _ -> false)
      (fun value -> `Summary (Params.empty, value))
      summary props
  in
  let props =
    Patch.replace_in_list
      (function `Location _ -> true | _ -> false)
      (fun value -> `Location (Params.empty, value))
      location props
  in
  let props =
    Patch.replace_in_list
      (function `Description _ -> true | _ -> false)
      (fun value -> `Description (Params.empty, value))
      description props
  in
  let props =
    match categories with
    | Patch.Set [] ->
        List.filter (function `Categories _ -> false | _ -> true) props
    | patch ->
        Patch.replace_in_list
          (function `Categories _ -> true | _ -> false)
          (fun value -> `Categories (Params.empty, value))
          patch props
  in
  let alarm_patch =
    match alarms with Patch.Set [] -> Patch.Clear | patch -> patch
  in
  let alarms =
    match alarm_patch with
    | Patch.Keep ->
        if
          base_event.alarms = [] && existing <> []
          && not (List.mem "VALARM" existing_cleared_families)
        then t.event.alarms
        else base_event.alarms
    | Patch.Clear -> []
    | Patch.Set value -> value
  in
  let cleared_families =
    existing_cleared_families
    |> update_cleared_family "SUMMARY" summary
    |> update_cleared_family "LOCATION" location
    |> update_cleared_family "DESCRIPTION" description
    |> update_cleared_family "CATEGORIES"
         (match categories with Patch.Set [] -> Patch.Clear | patch -> patch)
    |> update_cleared_family "END" end_
    |> update_cleared_family "VALARM" alarm_patch
  in
  let props = with_cleared_override_families cleared_families props in
  let* () = validate_start_end dtstart dtend_or_duration in
  let* () = Alarm.validate_all alarms in
  let* () =
    Alarm.validate_references ~has_start:true
      ~has_end:(Option.is_some dtend_or_duration)
      alarms
  in
  let completed_override =
    {
      dtstamp = (Params.empty, now);
      uid = t.event.uid;
      dtstart;
      dtend_or_duration;
      rrule = None;
      props;
      alarms;
    }
  in
  let* () = validate_loaded_event completed_override in
  Ok completed_override

type alarm_owner = Series of t | Occurrence of occurrence

let event_of_alarm_owner = function
  | Series series -> series.event
  | Occurrence occurrence -> occurrence.effective_event

let compute_alarm_fire_time_result ~floating_tz event alarm =
  match Alarm.trigger alarm with
  | params, `Duration span ->
      let* start =
        get_ical_start_result ~floating_tz event
        |> Result.map_error recurrence_conversion_error
      in
      let* end_ =
        get_ical_end_result ~floating_tz event
        |> Result.map_error recurrence_conversion_error
      in
      let base =
        match Params.find Related params with
        | Some `End -> Option.value end_ ~default:start
        | Some `Start | None -> start
      in
      Ok (Ptime.add_span base span)
  | _, `Datetime dt -> Ok (Some dt)

let fire_is_in_range ~from ~to_ fire_time =
  (match from with
    | None -> true
    | Some lower -> Ptime.compare fire_time lower >= 0)
  && Ptime.compare fire_time to_ < 0

let add_alarm_fires ~max_fires ~from ~to_ ~owner ~alarm ~alarm_index
    (fires, count) fire_times =
  List.fold_left
    (fun result fire_time ->
      let* fires, count = result in
      if not (fire_is_in_range ~from ~to_ fire_time) then Ok (fires, count)
      else if count >= max_fires then
        Error
          (`Msg
             (Printf.sprintf "alarm expansion exceeded the %d-fire safety limit"
                max_fires))
      else
        Ok ({ Alarm.fire_time; owner; alarm; alarm_index } :: fires, count + 1))
    (Ok (fires, count))
    fire_times

let event_duration_result ~floating_tz event =
  match event.dtend_or_duration with
  | None -> Ok Ptime.Span.zero
  | Some (`Duration (_, duration)) -> Ok duration
  | Some (`Dtend _) ->
      let* start =
        get_ical_start_result ~floating_tz event
        |> Result.map_error recurrence_conversion_error
      in
      let* end_ =
        get_ical_end_result ~floating_tz event
        |> Result.map_error recurrence_conversion_error
      in
      Ok
        (match end_ with
        | Some end_ -> Ptime.diff end_ start
        | None -> Ptime.Span.zero)

let relative_alarm_offset_result ~floating_tz event params trigger =
  match Params.find Related params with
  | Some `End ->
      let* duration = event_duration_result ~floating_tz event in
      Ok (Ptime.Span.add trigger duration)
  | Some `Start | None -> Ok trigger

let is_absolute_alarm alarm =
  match Alarm.trigger alarm with
  | _, `Datetime _ -> true
  | _, `Duration _ -> false

let relative_alarm_offsets_result ~max_repetitions ~floating_tz event =
  let rec collect acc = function
    | [] -> Ok (List.rev acc)
    | alarm :: rest -> (
        match Alarm.trigger alarm with
        | params, `Duration trigger ->
            let* offset =
              relative_alarm_offset_result ~floating_tz event params trigger
            in
            let* offsets = Alarm.repeated_spans ~max_repetitions offset alarm in
            collect (List.rev_append offsets acc) rest
        | _, `Datetime _ -> collect acc rest)
  in
  collect [] event.alarms

let min_ptime left right = if Ptime.compare left right <= 0 then left else right
let max_ptime left right = if Ptime.compare left right >= 0 then left else right

let compute_alarm_fires_result ?(max_instances = 100_000) ~floating_tz ~from
    ~to_ event =
  if
    match from with Some lower -> Ptime.compare lower to_ >= 0 | None -> false
  then Ok []
  else
    let* overrides =
      if has_recurrence_set event then validated_overrides event else Ok []
    in
    let cancelled (candidate : Icalendar.event) =
      List.exists
        (function `Status (_, `Cancelled) -> true | _ -> false)
        candidate.props
    in
    let* override_occurrences =
      List.fold_left
        (fun result override ->
          let* effective = result in
          if cancelled override then Ok effective
          else
            let* recurrence_id_property =
              match recurrence_id_property_of_event override with
              | Some property -> Ok property
              | None ->
                  Error (`Msg "persisted override is missing RECURRENCE-ID")
            in
            let* occurrence_start =
              Date.ptime_of_ical_result ~floating_tz
                (snd recurrence_id_property)
              |> Result.map_error recurrence_conversion_error
            in
            let* effective_event =
              effective_override_result ~floating_tz event.event override
            in
            let occurrence =
              make_occurrence ~series_uid:(get_id event) ~effective_event
                ~recurrence_id_property ~occurrence_start
                ~query_timezone:floating_tz ~origin:Persisted_override
            in
            Ok (occurrence :: effective))
        (Ok []) overrides
      |> Result.map List.rev
    in
    let master_absolute =
      get_alarms event
      |> List.mapi (fun index alarm -> (index, alarm))
      |> List.filter (fun (_, alarm) -> is_absolute_alarm alarm)
    in
    let override_absolute =
      List.concat_map
        (fun occurrence ->
          occurrence.effective_event.alarms
          |> List.mapi (fun index alarm ->
              (Occurrence occurrence, index, alarm))
          |> List.filter (fun (_, _, alarm) ->
              is_absolute_alarm alarm
              && not
                   (List.exists
                      (fun (_, master) -> master = alarm)
                      master_absolute)))
        override_occurrences
    in
    let* fires, fire_count =
      List.fold_left
        (fun result (owner, alarm_index, alarm) ->
          let* fires, fire_count = result in
          match Alarm.trigger alarm with
          | _, `Datetime fire_time ->
              let* fire_times =
                Alarm.repeated_instants ~max_repetitions:max_instances fire_time
                  alarm
              in
              add_alarm_fires ~max_fires:max_instances ~from ~to_ ~owner ~alarm
                ~alarm_index (fires, fire_count) fire_times
          | _, `Duration _ -> Ok (fires, fire_count))
        (Ok ([], 0))
        (List.map
           (fun (alarm_index, alarm) -> (Series event, alarm_index, alarm))
           master_absolute
        @ override_absolute)
    in
    let horizon_events =
      event.event
      :: List.map
           (fun occurrence -> occurrence.effective_event)
           override_occurrences
    in
    let* offsets =
      List.fold_left
        (fun result horizon_event ->
          let* acc = result in
          let* offsets =
            relative_alarm_offsets_result ~max_repetitions:max_instances
              ~floating_tz horizon_event
          in
          Ok (List.rev_append offsets acc))
        (Ok []) horizon_events
    in
    let* fires, _fire_count =
      match offsets with
      | [] -> Ok (fires, fire_count)
      | first_offset :: rest_offsets ->
          let shifted_to offset = Ptime.sub_span to_ offset in
          let* candidate_to =
            match shifted_to first_offset with
            | None -> Error (`Msg "alarm recurrence horizon is out of range")
            | Some first ->
                List.fold_left
                  (fun result offset ->
                    let* current = result in
                    match shifted_to offset with
                    | Some shifted -> Ok (max_ptime current shifted)
                    | None ->
                        Error (`Msg "alarm recurrence horizon is out of range"))
                  (Ok first) rest_offsets
          in
          let candidate_from =
            Option.bind from (fun lower ->
                let shifted =
                  List.filter_map
                    (fun offset -> Ptime.sub_span lower offset)
                    offsets
                in
                match shifted with
                | [] -> None
                | first :: rest -> Some (List.fold_left min_ptime first rest))
          in
          let* candidates =
            if not (has_recurrence_set event) then Ok [ Series event ]
            else
              expand_occurrences_result ~max_instances ~from:candidate_from
                ~floating_tz ~to_:candidate_to event
              |> Result.map (List.map (fun occurrence -> Occurrence occurrence))
          in
          List.fold_left
            (fun result owner ->
              let* fires, fire_count = result in
              let occurrence = event_of_alarm_owner owner in
              List.fold_left
                (fun result (alarm_index, alarm) ->
                  let* fires, fire_count = result in
                  match Alarm.trigger alarm with
                  | _, `Duration _ -> (
                      let* fire_time =
                        compute_alarm_fire_time_result ~floating_tz occurrence
                          alarm
                      in
                      match fire_time with
                      | None -> Ok (fires, fire_count)
                      | Some fire_time ->
                          let* fire_times =
                            Alarm.repeated_instants
                              ~max_repetitions:max_instances fire_time alarm
                          in
                          add_alarm_fires ~max_fires:max_instances ~from ~to_
                            ~owner ~alarm ~alarm_index (fires, fire_count)
                            fire_times)
                  | _, `Datetime _ -> Ok (fires, fire_count))
                (Ok (fires, fire_count))
                (List.mapi
                   (fun alarm_index alarm -> (alarm_index, alarm))
                   occurrence.alarms))
            (Ok (fires, fire_count))
            candidates
    in
    Ok
      (List.stable_sort
         (fun left right ->
           Ptime.compare left.Alarm.fire_time right.Alarm.fire_time)
         fires)

let make = make_series
let of_events_result = series_of_events_result
let of_authored_events_result = series_of_authored_events_result

let validate series =
  validate_series_events series.event series.overrides series.date_until

let master series = series.event
let overrides series = series.overrides
let authored_events series = series.event :: series.overrides
let date_until series = series.date_until

module Occurrence = struct
  type origin = occurrence_origin = Generated | Persisted_override

  module Reference = struct
    type t = occurrence_reference

    let uid reference = reference.series_uid
    let recurrence_id reference = snd reference.recurrence_id_property
    let recurrence_id_property reference = reference.recurrence_id_property
    let occurrence_start reference = reference.occurrence_start
    let query_timezone reference = reference.query_timezone
  end

  type t = occurrence

  let reference occurrence = occurrence.reference
  let origin occurrence = occurrence.origin
  let effective_ical_event occurrence = occurrence.effective_event
  let get_summary occurrence = get_ical_summary occurrence.effective_event

  let get_start_result occurrence =
    get_ical_start_result ~floating_tz:occurrence.reference.query_timezone
      occurrence.effective_event

  let get_end_result occurrence =
    get_ical_end_result ~floating_tz:occurrence.reference.query_timezone
      occurrence.effective_event

  let get_location occurrence = get_ical_location occurrence.effective_event

  let get_description occurrence =
    get_ical_description occurrence.effective_event

  let get_categories occurrence = get_ical_categories occurrence.effective_event
  let get_alarms occurrence = occurrence.effective_event.alarms
  let is_date occurrence = ical_event_is_date occurrence.effective_event

  let get_start_timezone occurrence =
    get_ical_start_timezone occurrence.effective_event

  let get_end_timezone occurrence =
    get_ical_end_timezone occurrence.effective_event
end

module Recurrence = struct
  let expand = expand_occurrences_result
  let resolve_reference = resolve_occurrence_reference
  let validate_override = validate_occurrence_override
  let delete_occurrence = delete_occurrence_checked
  let create_override = create_occurrence_override_checked
end
