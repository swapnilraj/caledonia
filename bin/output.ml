open Caledonia_lib

type color_policy = [ `Auto | `Always | `Never ]

let color_enabled policy =
  match policy with
  | `Always -> true
  | `Never -> false
  | `Auto -> Unix.isatty Unix.stdout

let sanitize_terminal = Format_utils.sanitize_terminal
let sanitize_terminal_line = Format_utils.sanitize_terminal_line

let print_terminal_stdout_line message =
  Printf.printf "%s\n%!" (sanitize_terminal_line message)

let print_terminal_stderr_line message =
  Printf.eprintf "%s\n%!" (sanitize_terminal_line message)

let print_error label message =
  print_terminal_stderr_line (Printf.sprintf "%s: %s" label message)

let validate_human_items ~tz items =
  let ( let* ) = Result.bind in
  let validate item =
    let name =
      Component_query.component_type item |> Component_kind.to_string
    in
    let* _start =
      Component_query.get_start_result ~floating_tz:tz item
      |> Result.map_error (fun (`Msg message) ->
          `Msg (Printf.sprintf "Cannot display %s start: %s" name message))
    in
    let* _end =
      Component_query.get_end_result ~floating_tz:tz item
      |> Result.map_error (fun (`Msg message) ->
          `Msg (Printf.sprintf "Cannot display %s end: %s" name message))
    in
    Ok ()
  in
  List.fold_left
    (fun result item ->
      let* () = result in
      validate item)
    (Ok ()) items

let component_type = Component_kind.to_string
let item_ics ~documents item = Calendar_export.to_ics ~documents [ item ]
let json_option f = function None -> `Null | Some value -> f value
let json_string_option = json_option (fun value -> `String value)
let json_int_option = json_option (fun value -> `Int value)

let wall_datetime timestamp =
  let (year, month, day), ((hour, minute, second), _) =
    Ptime.to_date_time timestamp
  in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d" year month day hour minute
    second

let utc_datetime timestamp = Ptime.to_rfc3339 ~frac_s:0 ~tz_offset_s:0 timestamp

let json_calendar_time = function
  | `Date (year, month, day) ->
      `Assoc
        [
          ("kind", `String "date");
          ("value", `String (Printf.sprintf "%04d-%02d-%02d" year month day));
        ]
  | `Datetime (`Utc timestamp) ->
      `Assoc
        [ ("kind", `String "utc"); ("value", `String (utc_datetime timestamp)) ]
  | `Datetime (`Local timestamp) ->
      `Assoc
        [
          ("kind", `String "floating");
          ("value", `String (wall_datetime timestamp));
        ]
  | `Datetime (`With_tzid (timestamp, (_, tzid))) ->
      `Assoc
        [
          ("kind", `String "tzid");
          ("value", `String (wall_datetime timestamp));
          ("tzid", `String tzid);
        ]

let calendar_time_value = function
  | `Date (year, month, day) -> Printf.sprintf "%04d-%02d-%02d" year month day
  | `Datetime (`Utc timestamp) -> utc_datetime timestamp
  | `Datetime (`Local timestamp) | `Datetime (`With_tzid (timestamp, _)) ->
      wall_datetime timestamp

let seconds_of_span span =
  match Ptime.Span.to_int_s span with
  | Some seconds -> seconds
  | None ->
      invalid_arg "iCalendar duration is not an integral number of seconds"

let weekday = function
  | `Monday -> "monday"
  | `Tuesday -> "tuesday"
  | `Wednesday -> "wednesday"
  | `Thursday -> "thursday"
  | `Friday -> "friday"
  | `Saturday -> "saturday"
  | `Sunday -> "sunday"

let json_ints values = `List (List.map (fun value -> `Int value) values)

let json_recur_part = function
  | `Byminute values ->
      `Assoc [ ("kind", `String "by_minute"); ("values", json_ints values) ]
  | `Byhour values ->
      `Assoc [ ("kind", `String "by_hour"); ("values", json_ints values) ]
  | `Bysecond values ->
      `Assoc [ ("kind", `String "by_second"); ("values", json_ints values) ]
  | `Bymonth values ->
      `Assoc [ ("kind", `String "by_month"); ("values", json_ints values) ]
  | `Bymonthday values ->
      `Assoc [ ("kind", `String "by_month_day"); ("values", json_ints values) ]
  | `Bysetposday values ->
      `Assoc
        [ ("kind", `String "by_set_position"); ("values", json_ints values) ]
  | `Byweek values ->
      `Assoc [ ("kind", `String "by_week"); ("values", json_ints values) ]
  | `Byyearday values ->
      `Assoc [ ("kind", `String "by_year_day"); ("values", json_ints values) ]
  | `Weekday value ->
      `Assoc [ ("kind", `String "weekday"); ("value", `String (weekday value)) ]
  | `Byday values ->
      `Assoc
        [
          ("kind", `String "by_day");
          ( "values",
            `List
              (List.map
                 (fun (ordinal, day) ->
                   `Assoc
                     [
                       ("ordinal", `Int ordinal);
                       ("weekday", `String (weekday day));
                     ])
                 values) );
        ]

let recurrence_frequency = function
  | `Secondly -> "secondly"
  | `Minutely -> "minutely"
  | `Hourly -> "hourly"
  | `Daily -> "daily"
  | `Weekly -> "weekly"
  | `Monthly -> "monthly"
  | `Yearly -> "yearly"

let json_recurrence ?date_until (frequency, limit, interval, parts) =
  let limit =
    match (date_until, limit) with
    | Some (year, month, day), Some (`Until _) ->
        `Assoc
          [
            ("kind", `String "until");
            ("value", json_calendar_time (`Date (year, month, day)));
          ]
    | _, None -> `Null
    | _, Some (`Count count) ->
        `Assoc [ ("kind", `String "count"); ("value", `Int count) ]
    | _, Some (`Until (`Utc timestamp)) ->
        `Assoc
          [
            ("kind", `String "until");
            ("value", json_calendar_time (`Datetime (`Utc timestamp)));
          ]
    | _, Some (`Until (`Local timestamp)) ->
        `Assoc
          [
            ("kind", `String "until");
            ("value", json_calendar_time (`Datetime (`Local timestamp)));
          ]
  in
  `Assoc
    [
      ("frequency", `String (recurrence_frequency frequency));
      ("limit", limit);
      ("interval", json_int_option interval);
      ("parts", `List (List.map json_recur_part parts));
    ]

let status = Component_status.to_string

let json_parameters params =
  Icalendar.Params.bindings params
  |> List.map (fun binding ->
      let name, value = Calendar_codec.canonical_parameter_binding binding in
      `Assoc [ ("name", `String name); ("value", `String value) ])
  |> fun values -> `List values

let json_other_property = function
  | `Iana_prop (name, params, value) ->
      `Assoc
        [
          ("kind", `String "iana");
          ("name", `String name);
          ("value", `String value);
          ("parameters", json_parameters params);
        ]
  | `Xprop ((namespace, name), params, value) ->
      `Assoc
        [
          ("kind", `String "x-property");
          ("namespace", `String namespace);
          ("name", `String name);
          ("value", `String value);
          ("parameters", json_parameters params);
        ]

let json_attachment = function
  | params, `Uri uri ->
      `Assoc
        [
          ("kind", `String "uri");
          ("value", `String (Uri.to_string uri));
          ("parameters", json_parameters params);
        ]
  | params, `Binary value ->
      `Assoc
        [
          ("kind", `String "binary");
          ("encoding", `String "base64");
          ("value", `String value);
          ("parameters", json_parameters params);
        ]

let json_alarm_trigger (params, trigger) =
  match trigger with
  | `Datetime timestamp ->
      `Assoc
        [
          ("kind", `String "absolute");
          ("value", `String (utc_datetime timestamp));
          ("parameters", json_parameters params);
        ]
  | `Duration duration ->
      let related =
        match Icalendar.Params.find Icalendar.Related params with
        | Some `End -> "end"
        | Some `Start | None -> "start"
      in
      `Assoc
        [
          ("kind", `String "relative");
          ("seconds", `Int (seconds_of_span duration));
          ("related", `String related);
          ("parameters", json_parameters params);
        ]

let duration_repeat_fields = function
  | None ->
      [
        ("repeat", `Null);
        ("repeat_interval_seconds", `Null);
        ("duration_parameters", `List []);
        ("repeat_parameters", `List []);
      ]
  | Some ((duration_params, duration), (repeat_params, repeat)) ->
      [
        ("repeat", `Int repeat);
        ("repeat_interval_seconds", `Int (seconds_of_span duration));
        ("duration_parameters", json_parameters duration_params);
        ("repeat_parameters", json_parameters repeat_params);
      ]

let json_alarm index alarm =
  let common ~action trigger duration_repeat summary other fields =
    let summary_value, summary_parameters =
      match summary with
      | None -> (`Null, `List [])
      | Some (params, value) -> (`String value, json_parameters params)
    in
    `Assoc
      ([
         ("index", `Int index);
         ("action", `String action);
         ("trigger", json_alarm_trigger trigger);
         ("summary", summary_value);
         ("summary_parameters", summary_parameters);
         ("other", `List (List.map json_other_property other));
       ]
      @ duration_repeat_fields duration_repeat
      @ fields)
  in
  match alarm with
  | `Display (alarm : Icalendar.display_struct Icalendar.alarm_struct) ->
      let description, description_parameters =
        match alarm.special.Icalendar.description with
        | None -> (`Null, `List [])
        | Some (params, value) -> (`String value, json_parameters params)
      in
      common ~action:"display" alarm.Icalendar.trigger alarm.duration_repeat
        alarm.summary alarm.other
        [
          ("description", description);
          ("description_parameters", description_parameters);
        ]
  | `Audio (alarm : Icalendar.audio_struct Icalendar.alarm_struct) ->
      common ~action:"audio" alarm.Icalendar.trigger alarm.duration_repeat
        alarm.summary alarm.other
        [
          ( "attachment",
            json_option json_attachment alarm.special.Icalendar.attach );
        ]
  | `Email (alarm : Icalendar.email_struct Icalendar.alarm_struct) ->
      common ~action:"email" alarm.Icalendar.trigger alarm.duration_repeat
        alarm.summary alarm.other
        [
          ("description", `String (snd alarm.special.Icalendar.description));
          ( "description_parameters",
            json_parameters (fst alarm.special.Icalendar.description) );
          ( "attendees",
            `List
              (List.map
                 (fun (_, uri) -> `String (Uri.to_string uri))
                 alarm.special.Icalendar.attendees) );
          ( "attendee_values",
            `List
              (List.map
                 (fun (params, uri) ->
                   `Assoc
                     [
                       ("uri", `String (Uri.to_string uri));
                       ("parameters", json_parameters params);
                     ])
                 alarm.special.Icalendar.attendees) );
          ( "attachment",
            json_option json_attachment alarm.special.Icalendar.attach );
        ]
  | `None (alarm : unit Icalendar.alarm_struct) ->
      common ~action:"none" alarm.Icalendar.trigger alarm.duration_repeat
        alarm.summary alarm.other []

let json_alarms alarms =
  `List (List.mapi (fun index alarm -> json_alarm index alarm) alarms)

let event_status event =
  List.find_map
    (function `Status (_, value) -> Some value | _ -> None)
    event.Icalendar.props

let event_priority event =
  List.find_map
    (function `Priority (_, value) -> Some value | _ -> None)
    event.Icalendar.props

let event_recurrence_id event =
  List.find_map
    (function `Recur_id (_, value) -> Some value | _ -> None)
    event.Icalendar.props

let todo_start props =
  List.find_map (function `Dtstart (_, value) -> Some value | _ -> None) props

let todo_due props =
  List.find_map (function `Due (_, value) -> Some value | _ -> None) props

let todo_recurrence props =
  List.find_map
    (function `Rrule (_, value) -> Some (value, None) | _ -> None)
    props

let todo_recurrence_id props =
  List.find_map
    (function `Recur_id (_, value) -> Some value | _ -> None)
    props

let journal_start props =
  List.find_map (function `Dtstart (_, value) -> Some value | _ -> None) props

let journal_recurrence props =
  List.find_map
    (function `Rrule (_, value) -> Some (value, None) | _ -> None)
    props

let journal_recurrence_id props =
  List.find_map
    (function `Recur_id (_, value) -> Some value | _ -> None)
    props

let event_fields ~date_until raw =
  let end_ =
    match raw.Icalendar.dtend_or_duration with
    | None -> `Null
    | Some (`Dtend (_, value)) ->
        `Assoc
          [ ("kind", `String "dtend"); ("value", json_calendar_time value) ]
    | Some (`Duration (_, duration)) ->
        `Assoc
          [
            ("kind", `String "duration");
            ("seconds", `Int (seconds_of_span duration));
          ]
  in
  ( Some (snd raw.dtstart),
    event_recurrence_id raw,
    event_status raw,
    raw.rrule |> Option.map (fun (_, recurrence) -> (recurrence, date_until)),
    raw.alarms,
    [
      ("end", end_);
      ( "location",
        json_string_option
          (List.find_map
             (function `Location (_, value) -> Some value | _ -> None)
             raw.Icalendar.props) );
      ("priority", json_int_option (event_priority raw));
      ("percent_complete", `Null);
      ("completed", `Null);
      ("due", `Null);
      ("parent", `Null);
    ] )

let item_fields = function
  | Component_query.Occurrence { occurrence; _ } ->
      event_fields ~date_until:None
        (Event.Occurrence.effective_ical_event occurrence)
  | Component_query.Stored component -> (
      match
        ( Component.to_event component,
          Component.to_todo component,
          Component.to_journal component )
      with
      | Some event, _, _ ->
          event_fields ~date_until:(Event.date_until event) (Event.master event)
      | _, Some todo, _ ->
          let props = Todo.to_ical_todo todo in
          let end_ =
            match Todo.get_duration todo with
            | None -> `Null
            | Some duration ->
                `Assoc
                  [
                    ("kind", `String "duration");
                    ("seconds", `Int (seconds_of_span duration));
                  ]
          in
          ( todo_start props,
            todo_recurrence_id props,
            Todo.get_status todo,
            todo_recurrence props,
            Todo.get_alarms todo,
            [
              ("end", end_);
              ("location", `Null);
              ("priority", json_int_option (Todo.get_priority todo));
              ("percent_complete", json_int_option (Todo.get_percent todo));
              ( "completed",
                json_option
                  (fun timestamp -> `String (utc_datetime timestamp))
                  (Todo.get_completed todo) );
              ("due", json_option json_calendar_time (todo_due props));
              ("parent", json_string_option (Todo.get_related_parent todo));
            ] )
      | _, _, Some journal ->
          let props = Journal.to_ical_journal journal in
          ( journal_start props,
            journal_recurrence_id props,
            Journal.get_status journal,
            journal_recurrence props,
            [],
            [
              ("end", `Null);
              ("location", `Null);
              ("priority", `Null);
              ("percent_complete", `Null);
              ("completed", `Null);
              ("due", `Null);
              ("parent", `Null);
            ] )
      | _ -> assert false)

let recurrence_lines = function
  | Component_query.Stored component -> (
      match Component.to_event component with
      | None -> Ok []
      | Some event ->
          Calendar_codec.canonical_event_recurrence_lines
            ~date_until:(Event.date_until event) (Event.master event)
          |> Result.map_error (fun message -> `Msg message))
  | Component_query.Occurrence _ -> Ok []

let json_of_item ~documents ~tz:_ item =
  let ( let* ) = Result.bind in
  let* ics = item_ics ~documents item in
  let start, recurrence_id, component_status, recurrence, alarms, extra =
    item_fields item
  in
  let* recurrence_set =
    recurrence_lines item |> Result.map (List.map (fun line -> `String line))
  in
  Ok
    (`Assoc
       ([
          ("schema_version", `Int 1);
          ( "component_type",
            `String (component_type (Component_query.component_type item)) );
          ( "identity",
            `Assoc
              [
                ("calendar_key", `String (Component_query.get_calendar_key item));
                ( "calendar_display_name",
                  `String (Component_query.get_calendar_name item) );
                ("file", `String (snd (Component_query.get_file item)));
                ("uid", `String (Component_query.get_id item));
                ("recurrence_id", json_option json_calendar_time recurrence_id);
                ( "source_fingerprint",
                  json_string_option
                    (Some (Component_query.get_source_fingerprint item)) );
              ] );
          ("id", `String (Component_query.get_id item));
          ("calendar", `String (Component_query.get_calendar_name item));
          ("summary", json_string_option (Component_query.get_summary item));
          ( "description",
            json_string_option (Component_query.get_description item) );
          ( "categories",
            `List
              (List.map
                 (fun category -> `String category)
                 (Component_query.get_categories item)) );
          ("start", json_option json_calendar_time start);
          ( "status",
            json_option (fun value -> `String (status value)) component_status
          );
          ( "recurrence",
            json_option
              (fun (value, date_until) -> json_recurrence ?date_until value)
              recurrence );
          ("recurrence_set", `List recurrence_set);
          ("alarms", json_alarms alarms);
          ("ics", `String ics);
        ]
       @ extra))

let csv_escape value =
  let normalized = Buffer.create (String.length value) in
  let rec normalize index =
    if index < String.length value then
      match value.[index] with
      | '\r' ->
          Buffer.add_string normalized "\r\n";
          normalize
            (if index + 1 < String.length value && value.[index + 1] = '\n' then
               index + 2
             else index + 1)
      | '\n' ->
          Buffer.add_string normalized "\r\n";
          normalize (index + 1)
      | character ->
          Buffer.add_char normalized character;
          normalize (index + 1)
  in
  normalize 0;
  let value = Buffer.contents normalized in
  if
    String.exists
      (fun character -> character = ',' || character = '"' || character = '\r')
      value
  then "\"" ^ String.concat "\"\"" (String.split_on_char '"' value) ^ "\""
  else value

let csv_row fields = String.concat "," (List.map csv_escape fields)

let format_csv ~documents ~tz items =
  let ( let* ) = Result.bind in
  let header =
    csv_row
      [
        "schema_version";
        "component_type";
        "id";
        "calendar";
        "summary";
        "start";
        "details_json";
      ]
  in
  let* rows =
    Command_common.map_result
      (fun item ->
        let* details = json_of_item ~documents ~tz item in
        let start, _, _, _, _, _ = item_fields item in
        Ok
          (csv_row
             [
               "1";
               component_type (Component_query.component_type item);
               Component_query.get_id item;
               Component_query.get_calendar_name item;
               Option.value ~default:"" (Component_query.get_summary item);
               Option.fold ~none:"" ~some:calendar_time_value start;
               Yojson.Safe.to_string details;
             ]))
      items
  in
  Ok (String.concat "\r\n" (header :: rows) ^ "\r\n")

let rec sexp_of_json = function
  | `Null -> Sexplib.Sexp.Atom "null"
  | `Bool value ->
      Sexplib.Sexp.List
        [ Sexplib.Sexp.Atom "bool"; Sexplib.Sexp.Atom (string_of_bool value) ]
  | `Int value ->
      Sexplib.Sexp.List
        [ Sexplib.Sexp.Atom "int"; Sexplib.Sexp.Atom (string_of_int value) ]
  | `Intlit value ->
      Sexplib.Sexp.List [ Sexplib.Sexp.Atom "integer"; Sexplib.Sexp.Atom value ]
  | `Float value ->
      Sexplib.Sexp.List
        [ Sexplib.Sexp.Atom "float"; Sexplib.Sexp.Atom (string_of_float value) ]
  | `String value ->
      Sexplib.Sexp.List [ Sexplib.Sexp.Atom "string"; Sexplib.Sexp.Atom value ]
  | `List values ->
      Sexplib.Sexp.List
        (Sexplib.Sexp.Atom "array" :: List.map sexp_of_json values)
  | `Assoc fields ->
      Sexplib.Sexp.List
        (Sexplib.Sexp.Atom "object"
        :: List.map
             (fun (name, value) ->
               Sexplib.Sexp.List [ Sexplib.Sexp.Atom name; sexp_of_json value ])
             fields)

let format_sexp ~documents ~tz items =
  let ( let* ) = Result.bind in
  let* records =
    Command_common.map_result
      (fun item ->
        let* json = json_of_item ~documents ~tz item in
        Ok (sexp_of_json json))
      items
  in
  Ok (Sexplib.Sexp.List records |> Sexplib.Sexp.to_string_hum)

let human_time ~tz timestamp =
  let local = Date.ptime_to_timedesc ~tz timestamp in
  Printf.sprintf "%02d:%02d" (Timedesc.hour local) (Timedesc.minute local)

let human_datetime ~tz timestamp =
  Printf.sprintf "%s %s(%s)"
    (Format_utils.format_date ~tz timestamp)
    (human_time ~tz timestamp)
    (Timedesc.Time_zone.name tz)

let event_is_date raw =
  match (raw.Icalendar.dtstart, raw.Icalendar.dtend_or_duration) with
  | (_, `Date _), _ | _, Some (`Dtend (_, `Date _)) -> true
  | _ -> false

let event_start_timezone raw =
  match raw.Icalendar.dtstart with
  | _, `Datetime (`With_tzid (_, (_, tzid))) -> Some tzid
  | _, `Datetime (`Utc _) -> Some "UTC"
  | _ -> None

let event_end_timezone raw =
  match raw.Icalendar.dtend_or_duration with
  | Some (`Dtend (_, `Datetime (`With_tzid (_, (_, tzid))))) -> Some tzid
  | Some (`Dtend (_, `Datetime (`Utc _))) -> Some "UTC"
  | _ -> None

let related_type params =
  match Icalendar.Params.find Icalendar.Reltype params with
  | Some `Parent -> "PARENT"
  | Some `Child -> "CHILD"
  | Some `Sibling -> "SIBLING"
  | Some (`Ianatoken value) -> value
  | Some (`Xname (namespace, name)) -> namespace ^ ":" ^ name
  | None -> "PARENT"

let format_other_property = function
  | `Xprop (("CALEDONIA", "CLEARED"), _, _)
  | `Xprop (("", "CALEDONIA-CLEARED"), _, _) ->
      None
  | `Related (params, value) ->
      Some ("Related-To", value ^ " (" ^ related_type params ^ ")")
  | `Seq (_, value) -> Some ("Sequence", string_of_int value)
  | `Created (_, value) -> Some ("Created", Date.rfc3339_utc value)
  | `Lastmod (_, value) -> Some ("Last-Modified", Date.rfc3339_utc value)
  | `Iana_prop ("RELATED", params, value) ->
      Some ("Related-To", value ^ " (" ^ related_type params ^ ")")
  | `Iana_prop (name, _, value) -> Some (name, value)
  | `Xprop ((namespace, name), _, value) -> Some (namespace ^ ":" ^ name, value)
  | _ -> None

let other_properties_text properties =
  properties
  |> List.filter_map format_other_property
  |> List.map (fun (name, value) ->
      Printf.sprintf "%s: %s\n"
        (sanitize_terminal_line name)
        (sanitize_terminal_line value))
  |> String.concat ""

let item_event = function
  | Component_query.Occurrence { occurrence; _ } ->
      Some (Event.Occurrence.effective_ical_event occurrence)
  | Component_query.Stored component ->
      Component.to_event component |> Option.map Event.master

let event_text ~tz item raw =
  let ( let* ) = Result.bind in
  let* start = Component_query.get_start_result ~floating_tz:tz item in
  let* end_ = Component_query.get_end_result ~floating_tz:tz item in
  let* start =
    match start with
    | Some start -> Ok start
    | None -> Error (`Msg "VEVENT is missing DTSTART")
  in
  let start_timezone = event_start_timezone raw in
  let end_timezone = event_end_timezone raw in
  let same_timezone =
    match (start_timezone, end_timezone) with
    | Some left, Some right -> String.equal left right
    | _ -> false
  in
  let start_date = Format_utils.format_date ~tz start in
  let start_time =
    if event_is_date raw then ""
    else
      let suffix =
        if same_timezone then ""
        else
          Option.fold ~none:""
            ~some:(fun value -> " (" ^ value ^ ")")
            start_timezone
      in
      " " ^ human_time ~tz start ^ suffix
  in
  let end_date, end_time =
    match end_ with
    | None -> ("", "")
    | Some end_ when event_is_date raw ->
        let days, _ = Ptime.Span.to_d_ps (Ptime.diff end_ start) in
        if days <= 1 then ("", "")
        else (" - " ^ Format_utils.format_date ~tz end_, "")
    | Some end_ ->
        let days, _ = Ptime.Span.to_d_ps (Ptime.diff end_ start) in
        let suffix =
          Option.fold ~none:""
            ~some:(fun value -> " (" ^ value ^ ")")
            end_timezone
        in
        if days = 0 then ("", " - " ^ human_time ~tz end_ ^ suffix)
        else
          ( " - " ^ Format_utils.format_date ~tz end_,
            " " ^ human_time ~tz end_ ^ suffix )
  in
  let date_time =
    start_date ^ start_time ^ end_date ^ end_time |> sanitize_terminal_line
  in
  let summary =
    Component_query.get_summary item
    |> Option.value ~default:"" |> sanitize_terminal_line
  in
  let location =
    Component_query.get_location item
    |> Option.fold ~none:"" ~some:(fun value ->
        "@" ^ sanitize_terminal_line value)
  in
  let summary_location =
    summary ^ if location = "" then "" else " " ^ location
  in
  let alarms =
    Component_query.get_alarms item |> Format_utils.format_alarms_short
  in
  let alarm_column = if alarms = "" then "" else "  " ^ alarms in
  let calendar =
    Component_query.get_calendar_name item |> sanitize_terminal_line
  in
  Ok
    (Printf.sprintf "%s  %s  %s%s  %s" calendar date_time summary_location
       alarm_column
       (Component_query.get_id item |> sanitize_terminal_line))

let event_entries ~tz item raw =
  let ( let* ) = Result.bind in
  let* start = Component_query.get_start_result ~floating_tz:tz item in
  let* end_ = Component_query.get_end_result ~floating_tz:tz item in
  let* start =
    match start with
    | Some start -> Ok start
    | None -> Error (`Msg "VEVENT is missing DTSTART")
  in
  let format_time timezone timestamp is_end =
    if event_is_date raw then Format_utils.format_date ~tz timestamp
    else
      let same_timezone =
        match (event_start_timezone raw, event_end_timezone raw) with
        | Some left, Some right -> String.equal left right
        | _ -> false
      in
      let suffix =
        if (not is_end) && same_timezone then ""
        else
          Option.fold ~none:"" ~some:(fun value -> " (" ^ value ^ ")") timezone
      in
      human_datetime ~tz timestamp ^ suffix
  in
  let summary =
    Format_utils.format_opt "Summary" sanitize_terminal_line
      (Component_query.get_summary item)
  in
  let start =
    Format_utils.format_opt "Start"
      (fun value ->
        format_time (event_start_timezone raw) value false
        |> sanitize_terminal_line)
      (Some start)
  in
  let end_ =
    Format_utils.format_opt "End"
      (fun value ->
        format_time (event_end_timezone raw) value true
        |> sanitize_terminal_line)
      end_
  in
  let location =
    Format_utils.format_opt "Location" sanitize_terminal_line
      (Component_query.get_location item)
  in
  let description =
    Format_utils.format_opt "Description" sanitize_terminal_line
      (Component_query.get_description item)
  in
  let* recurrence_lines = recurrence_lines item in
  let recurrence =
    recurrence_lines
    |> List.map (fun line ->
        "Recurrence: " ^ sanitize_terminal_line line ^ "\n")
    |> String.concat ""
  in
  let alarms =
    match Component_query.get_alarms item with
    | [] -> ""
    | alarms ->
        Printf.sprintf "Alarms: %s\n" (Format_utils.format_alarms alarms)
  in
  Ok
    (summary ^ start ^ end_ ^ location ^ description ^ recurrence ^ alarms
    ^ other_properties_text raw.Icalendar.props)

type todo_text_data = {
  calendar : string;
  start : string;
  due : string;
  state : string;
  summary : string;
  percent : string;
  categories : string;
  alarms : string;
  id : string;
}

let todo_text_data ~tz ~now item todo =
  let ( let* ) = Result.bind in
  let* start =
    Todo.get_start_result ~floating_tz:tz todo
    |> Result.map_error (fun error ->
        `Msg (Date.string_of_conversion_error error))
  in
  let* due =
    Todo.get_due_result ~floating_tz:tz todo
    |> Result.map_error (fun error ->
        `Msg (Date.string_of_conversion_error error))
  in
  let* overdue =
    Todo.is_overdue_at ~now ~tz todo
    |> Result.map_error (fun error ->
        `Msg (Date.string_of_conversion_error error))
  in
  Ok
    {
      calendar =
        Component_query.get_calendar_name item |> sanitize_terminal_line;
      start = Option.fold ~none:"" ~some:(Format_utils.format_date ~tz) start;
      due = Option.fold ~none:"" ~some:(Format_utils.format_date ~tz) due;
      state =
        (if Todo.is_completed todo then "[x]"
         else if overdue then "[!]"
         else "[ ]");
      summary =
        Component_query.get_summary item
        |> Option.value ~default:"" |> sanitize_terminal_line;
      percent =
        Todo.get_percent todo
        |> Option.fold ~none:"" ~some:(Printf.sprintf "%d%%");
      categories =
        Component_query.get_categories item
        |> String.concat "," |> sanitize_terminal_line;
      alarms =
        Component_query.get_alarms item |> Format_utils.format_alarms_short;
      id = Component_query.get_id item |> sanitize_terminal_line;
    }

let format_todo_rows data =
  let width select = Format_utils.max_width select data in
  let max_calendar = width (fun value -> value.calendar) in
  let max_start = width (fun value -> value.start) in
  let max_due = width (fun value -> value.due) in
  let max_state = width (fun value -> value.state) in
  let max_summary = width (fun value -> value.summary) in
  let max_percent = width (fun value -> value.percent) in
  let max_categories = width (fun value -> value.categories) in
  let has_alarms = List.exists (fun value -> value.alarms <> "") data in
  let max_alarms =
    if has_alarms then width (fun value -> value.alarms) else 0
  in
  let max_id = width (fun value -> value.id) in
  List.map
    (fun value ->
      let alarm_column =
        if has_alarms then
          "  " ^ Format_utils.pad_to_width max_alarms value.alarms
        else ""
      in
      Printf.sprintf "%s  %s  %s  %s  %s  %s  %s%s  %s"
        (Format_utils.pad_to_width max_calendar value.calendar)
        (Format_utils.pad_to_width max_start value.start)
        (Format_utils.pad_to_width max_due value.due)
        (Format_utils.pad_to_width max_state value.state)
        (Format_utils.pad_to_width max_summary value.summary)
        (Format_utils.pad_to_width max_percent value.percent)
        (Format_utils.pad_to_width max_categories value.categories)
        alarm_column
        (Format_utils.pad_to_width max_id value.id))
    data

let status_human = function
  | `Needs_action -> "Needs Action"
  | `Completed -> "Completed"
  | `In_process -> "In Process"
  | `Cancelled -> "Cancelled"
  | `Draft -> "Draft"
  | `Final -> "Final"
  | `Tentative -> "Tentative"
  | `Confirmed -> "Confirmed"

let todo_entries ~tz item todo =
  let ( let* ) = Result.bind in
  let* start =
    Todo.get_start_result ~floating_tz:tz todo
    |> Result.map_error (fun error ->
        `Msg (Date.string_of_conversion_error error))
  in
  let* due =
    Todo.get_due_result ~floating_tz:tz todo
    |> Result.map_error (fun error ->
        `Msg (Date.string_of_conversion_error error))
  in
  let optional label formatter value =
    Format_utils.format_opt label formatter value
  in
  let categories = Component_query.get_categories item in
  let alarms = Component_query.get_alarms item in
  Ok
    (optional "Summary" sanitize_terminal_line
       (Component_query.get_summary item)
    ^ optional "Start" (Format_utils.format_date ~tz) start
    ^ optional "Due" (Format_utils.format_date ~tz) due
    ^ optional "Priority" string_of_int (Todo.get_priority todo)
    ^ optional "Percent" (Printf.sprintf "%d%%") (Todo.get_percent todo)
    ^ optional "Status" status_human (Todo.get_status todo)
    ^ (if categories = [] then ""
       else
         Printf.sprintf "Categories: %s\n"
           (String.concat ", " categories |> sanitize_terminal_line))
    ^ optional "Description" sanitize_terminal_line
        (Component_query.get_description item)
    ^ (if alarms = [] then ""
       else Printf.sprintf "Alarms: %s\n" (Format_utils.format_alarms alarms))
    ^ other_properties_text (Todo.to_ical_todo todo))

let journal_text ~tz item journal =
  let ( let* ) = Result.bind in
  let* start =
    Journal.get_start_result ~floating_tz:tz journal
    |> Result.map_error (fun error ->
        `Msg (Date.string_of_conversion_error error))
  in
  let summary =
    match Component_query.get_summary item with
    | Some value when value <> "" -> value
    | _ ->
        (match Component_query.get_description item with
          | Some value -> (
              match String.split_on_char '\n' value with
              | first :: _ -> first
              | [] -> "")
          | None -> "")
        |> sanitize_terminal_line
  in
  let date = Option.fold ~none:"" ~some:(Format_utils.format_date ~tz) start in
  let categories =
    Component_query.get_categories item
    |> String.concat "," |> sanitize_terminal_line
  in
  let calendar =
    Component_query.get_calendar_name item |> sanitize_terminal_line
  in
  Ok
    (Printf.sprintf "%s  %s  %s  %s  %s" calendar date summary categories
       (Component_query.get_id item |> sanitize_terminal_line))

let journal_entries ~tz item journal =
  let ( let* ) = Result.bind in
  let* start =
    Journal.get_start_result ~floating_tz:tz journal
    |> Result.map_error (fun error ->
        `Msg (Date.string_of_conversion_error error))
  in
  let categories = Component_query.get_categories item in
  Ok
    (Format_utils.format_opt "Summary" sanitize_terminal_line
       (Component_query.get_summary item)
    ^ Format_utils.format_opt "Date" (Format_utils.format_date ~tz) start
    ^ (if categories = [] then ""
       else
         Printf.sprintf "Categories: %s\n"
           (String.concat ", " categories |> sanitize_terminal_line))
    ^ Format_utils.format_opt "Description" sanitize_terminal_line
        (Component_query.get_description item)
    ^ Format_utils.format_opt "Status" status_human
        (Component_query.get_status item)
    ^ other_properties_text (Journal.to_ical_journal journal))

let format_human ~format ~tz ~now ?get_color ~color items =
  let ( let* ) = Result.bind in
  let color_lookup = if color_enabled color then get_color else None in
  let add_calendar_color item output =
    match color_lookup with
    | None -> output
    | Some lookup -> (
        let calendar =
          Component_query.get_calendar_name item |> sanitize_terminal_line
        in
        match lookup (Component_query.get_calendar_key item) with
        | Some color
          when String.length output >= String.length calendar
               && String.sub output 0 (String.length calendar) = calendar ->
            Format_utils.colorize ~color calendar
            ^ String.sub output (String.length calendar)
                (String.length output - String.length calendar)
        | Some _ | None -> output)
  in
  let* todo_rows =
    match format with
    | `Entries -> Ok []
    | `Text ->
        items
        |> List.filter_map (fun item ->
            match
              Option.bind (Component_query.stored item) Component.to_todo
            with
            | Some todo -> Some (item, todo)
            | None -> None)
        |> Command_common.map_result (fun (item, todo) ->
            todo_text_data ~tz ~now item todo)
        |> Result.map format_todo_rows
  in
  let todo_rows = ref todo_rows in
  let next_todo_row () =
    match !todo_rows with
    | row :: rest ->
        todo_rows := rest;
        Ok row
    | [] -> Error (`Msg "missing preformatted todo row")
  in
  let format_item item =
    match
      ( item_event item,
        Option.bind (Component_query.stored item) Component.to_todo,
        Option.bind (Component_query.stored item) Component.to_journal )
    with
    | Some raw, _, _ ->
        if format = `Text then event_text ~tz item raw
        else event_entries ~tz item raw
    | None, Some todo, _ ->
        if format = `Text then next_todo_row () else todo_entries ~tz item todo
    | None, None, Some journal ->
        if format = `Text then journal_text ~tz item journal
        else journal_entries ~tz item journal
    | None, None, None ->
        Error (`Msg "Cannot display an unknown component type")
  in
  let* outputs = Command_common.map_result format_item items in
  Ok
    (List.map2
       (fun item output ->
         let output = sanitize_terminal output in
         if format = `Text then add_calendar_color item output else output)
       items outputs
    |> String.concat "\n")

let format_items ~documents ~format ~tz ?(now = Ptime_clock.now ()) ?get_color
    ~color items =
  match format with
  | `Json ->
      let ( let* ) = Result.bind in
      let* records =
        Command_common.map_result (json_of_item ~documents ~tz) items
      in
      Ok (`List records |> Yojson.Safe.to_string)
  | `Csv -> format_csv ~documents ~tz items
  | `Ics -> Calendar_export.to_ics ~documents items
  | `Sexp -> format_sexp ~documents ~tz items
  | (`Text | `Entries) as format ->
      format_human ~format ~tz ~now ?get_color ~color items

let print_items ~documents ~format ~tz ?now ?get_color ~color items =
  let ( let* ) = Result.bind in
  let* output =
    format_items ~documents ~format ~tz ?now ?get_color ~color items
  in
  (match format with
  | `Text | `Entries -> if output <> "" then Printf.printf "%s\n%!" output
  | `Json | `Csv | `Ics | `Sexp ->
      if output <> "" then Printf.printf "%s%!" output);
  Ok ()
