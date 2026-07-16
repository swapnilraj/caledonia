open Sexplib.Std

type search_field = Summary | Description | Location | Categories
[@@deriving sexp]

type query_request = {
  from : string option; [@sexp.option]
  to_ : string; (* Required field, not optional *)
  timezone : string option; [@sexp.option]
  calendars : string list; [@default []]
  text : string option; [@sexp.option]
  search_in : search_field list; [@default []]
  categories : string list; [@default []]
  id : string option; [@sexp.option]
  statuses : string list; [@default []]
  overdue : bool option; [@sexp.option]
  has_alarm : bool option; [@sexp.option]
  recurring : bool option; [@sexp.option]
  limit : int option; [@sexp.option]
}
[@@deriving sexp]

(* workaround https://github.com/janestreet/ppx_sexp_conv/issues/18#issuecomment-2792574295 *)
let query_request_of_sexp sexp =
  let open Sexplib.Sexp in
  let sexp =
    match sexp with
    | List ss ->
        List
          (List.map
             (function
               | List (Atom "to" :: v) -> List (Atom "to_" :: v) | v -> v)
             ss)
    | v -> v
  in
  query_request_of_sexp sexp

let sexp_of_query_request q =
  let open Sexplib.Sexp in
  let sexp = sexp_of_query_request q in
  let sexp =
    match sexp with
    | List ss ->
        List
          (List.map
             (function
               | List (Atom "to_" :: v) -> List (Atom "to" :: v) | v -> v)
             ss)
    | v -> v
  in
  sexp

type time_kind = Date | Utc | Floating | Tzid of string [@@deriving sexp]

type calendar_time_input = { kind : time_kind; value : string }
[@@deriving sexp]

type event_end_input = Dtend of calendar_time_input | Duration_seconds of int
[@@deriving sexp]

type alarm_action = Audio | Display | Email | None_action [@@deriving sexp]
type alarm_relation = Start | End [@@deriving sexp]

type alarm_trigger =
  | Relative of { seconds : int; related : alarm_relation }
  | Absolute of string
[@@deriving sexp]

type alarm_attachment = Uri of string | Binary of string [@@deriving sexp]
type alarm_parameter = { name : string; value : string } [@@deriving sexp]

type alarm_attendee = { uri : string; parameters : alarm_parameter list }
[@@deriving sexp]

type alarm_other_property =
  | Iana of { name : string; value : string; parameters : alarm_parameter list }
  | X of {
      namespace : string;
      name : string;
      value : string;
      parameters : alarm_parameter list;
    }
[@@deriving sexp]

type alarm_input = {
  action : alarm_action;
  trigger : alarm_trigger;
  trigger_parameters : alarm_parameter list; [@default []]
  repeat : int option; [@sexp.option]
  duration_seconds : int option; [@sexp.option]
  duration_parameters : alarm_parameter list; [@default []]
  repeat_parameters : alarm_parameter list; [@default []]
  summary : string option; [@sexp.option]
  summary_parameters : alarm_parameter list; [@default []]
  description : string option; [@sexp.option]
  description_parameters : alarm_parameter list; [@default []]
  attendees : string list; [@default []]
  attendee_values : alarm_attendee list; [@default []]
  attachment : alarm_attachment option; [@sexp.option]
  attachment_parameters : alarm_parameter list; [@default []]
  other : alarm_other_property list; [@default []]
}
[@@deriving sexp]

type recurrence_input = { rrule : string } [@@deriving sexp]

type create_event_request = {
  calendar : string;
  summary : string;
  start : calendar_time_input;
  end_ : event_end_input option; [@sexp.option]
  location : string option; [@sexp.option]
  description : string option; [@sexp.option]
  categories : string list; [@default []]
  recurrence : recurrence_input option; [@sexp.option]
  alarms : alarm_input list; [@default []]
}
[@@deriving sexp]

type 'a patch = 'a Patch.t = Keep | Clear | Set of 'a [@@deriving sexp]

type edit_event_request = {
  id : string;
  calendar_key : string;
  file : string;
  source_fingerprint : string option; [@sexp.option]
  summary : string patch; [@default Keep]
  start : calendar_time_input patch; [@default Keep]
  end_ : event_end_input patch; [@default Keep]
  location : string patch; [@default Keep]
  description : string patch; [@default Keep]
  categories : string list patch; [@default Keep]
  recurrence : recurrence_input patch; [@default Keep]
  alarms : alarm_input list patch; [@default Keep]
  occurrence_start : string option; [@sexp.option]
  occurrence_timezone : string option; [@sexp.option]
}
[@@deriving sexp]

type delete_event_request = {
  id : string;
  calendar_key : string;
  file : string;
  source_fingerprint : string option; [@sexp.option]
  occurrence_start : string option; [@sexp.option]
  occurrence_timezone : string option; [@sexp.option]
}
[@@deriving sexp]

type request =
  | Handshake
  | ListCalendars
  | Query of query_request
  | Refresh
  | CreateEvent of create_event_request
  | EditEvent of edit_event_request
  | DeleteEvent of delete_event_request
[@@deriving sexp]

type response_payload =
  | Hello of {
      protocol_version : int;
      server_version : string;
      capabilities : string list;
    }
  | Calendars of string list
  | Events of {
      events : Component_query.item list;
      occurrence_timezone : string option;
      documents : Calendar_document.t list;
    }
  | Empty

let atom value = Sexplib.Sexp.Atom value
let field name value = Sexplib.Sexp.List [ atom name; value ]

let local_datetime timestamp =
  let (year, month, day), ((hour, minute, second), _offset) =
    Ptime.to_date_time timestamp
  in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d" year month day hour minute
    second

let calendar_time_sexp = function
  | `Date (year, month, day) ->
      Sexplib.Sexp.List
        [
          field "kind" (atom "date");
          field "value" (atom (Printf.sprintf "%04d-%02d-%02d" year month day));
        ]
  | `Datetime (`Utc timestamp) ->
      Sexplib.Sexp.List
        [
          field "kind" (atom "utc");
          field "value" (atom (local_datetime timestamp));
        ]
  | `Datetime (`Local timestamp) ->
      Sexplib.Sexp.List
        [
          field "kind" (atom "floating");
          field "value" (atom (local_datetime timestamp));
        ]
  | `Datetime (`With_tzid (timestamp, (_, timezone))) ->
      Sexplib.Sexp.List
        [
          field "kind" (atom "tzid");
          field "value" (atom (local_datetime timestamp));
          field "tzid" (atom timezone);
        ]

let parameters_value params =
  Icalendar.Params.bindings params
  |> List.map (fun binding ->
      let name, value = Calendar_codec.canonical_parameter_binding binding in
      Sexplib.Sexp.List [ field "name" (atom name); field "value" (atom value) ])
  |> fun parameters -> Sexplib.Sexp.List parameters

let parameterized_option_fields name parameters_name = function
  | None -> []
  | Some (params, value) ->
      [
        field name (atom value); field parameters_name (parameters_value params);
      ]

let alarm_trigger_sexp
    ((params, trigger) :
      Icalendar.params
      * [ `Duration of Ptime.Span.t | `Datetime of Icalendar.timestamp_utc ]) =
  match trigger with
  | `Datetime timestamp ->
      Sexplib.Sexp.List
        [
          field "kind" (atom "absolute");
          field "value" (atom (Date.rfc3339_utc timestamp));
          field "parameters" (parameters_value params);
        ]
  | `Duration duration ->
      let related =
        match Icalendar.Params.find Icalendar.Related params with
        | Some `End -> "end"
        | Some `Start | None -> "start"
      in
      Sexplib.Sexp.List
        [
          field "kind" (atom "relative");
          field "seconds"
            (atom
               (string_of_int
                  (Option.value ~default:0 (Ptime.Span.to_int_s duration))));
          field "related" (atom related);
          field "parameters" (parameters_value params);
        ]

let duration_repeat_fields = function
  | None -> []
  | Some ((duration_params, duration), (repeat_params, repeat)) ->
      [
        field "duration_seconds"
          (atom
             (string_of_int
                (Option.value ~default:0 (Ptime.Span.to_int_s duration))));
        field "repeat" (atom (string_of_int repeat));
        field "duration_parameters" (parameters_value duration_params);
        field "repeat_parameters" (parameters_value repeat_params);
      ]

let attachment_value = function
  | None -> []
  | Some (params, `Uri uri) ->
      [
        field "attachment"
          (Sexplib.Sexp.List
             [
               field "kind" (atom "uri");
               field "value" (atom (Uri.to_string uri));
               field "parameters" (parameters_value params);
             ]);
      ]
  | Some (params, `Binary value) ->
      [
        field "attachment"
          (Sexplib.Sexp.List
             [
               field "kind" (atom "binary");
               field "value" (atom value);
               field "parameters" (parameters_value params);
             ]);
      ]

let alarm_other_sexp = function
  | `Iana_prop (name, params, value) ->
      Sexplib.Sexp.List
        [
          field "kind" (atom "iana");
          field "name" (atom name);
          field "value" (atom value);
          field "parameters" (parameters_value params);
        ]
  | `Xprop ((namespace, name), params, value) ->
      Sexplib.Sexp.List
        [
          field "kind" (atom "x");
          field "namespace" (atom namespace);
          field "name" (atom name);
          field "value" (atom value);
          field "parameters" (parameters_value params);
        ]

let alarm_other_value other =
  [ field "other" (Sexplib.Sexp.List (List.map alarm_other_sexp other)) ]

let alarm_sexp (alarm_value : Icalendar.alarm) =
  match alarm_value with
  | `Display (alarm : Icalendar.display_struct Icalendar.alarm_struct) ->
      Sexplib.Sexp.List
        (field "action" (atom "display")
        :: field "trigger" (alarm_trigger_sexp alarm.trigger)
        :: (duration_repeat_fields alarm.duration_repeat
           @ parameterized_option_fields "summary" "summary_parameters"
               alarm.summary
           @ parameterized_option_fields "description" "description_parameters"
               alarm.special.description
           @ alarm_other_value alarm.other))
  | `Audio (alarm : Icalendar.audio_struct Icalendar.alarm_struct) ->
      Sexplib.Sexp.List
        (field "action" (atom "audio")
        :: field "trigger" (alarm_trigger_sexp alarm.trigger)
        :: (duration_repeat_fields alarm.duration_repeat
           @ parameterized_option_fields "summary" "summary_parameters"
               alarm.summary
           @ attachment_value alarm.special.attach
           @ alarm_other_value alarm.other))
  | `Email (alarm : Icalendar.email_struct Icalendar.alarm_struct) ->
      let attendees =
        alarm.special.attendees
        |> List.map (fun (_, uri) -> atom (Uri.to_string uri))
      in
      let attendee_values =
        alarm.special.attendees
        |> List.map (fun (params, uri) ->
            Sexplib.Sexp.List
              [
                field "uri" (atom (Uri.to_string uri));
                field "parameters" (parameters_value params);
              ])
      in
      Sexplib.Sexp.List
        (field "action" (atom "email")
        :: field "trigger" (alarm_trigger_sexp alarm.trigger)
        :: field "description" (atom (snd alarm.special.description))
        :: field "description_parameters"
             (parameters_value (fst alarm.special.description))
        :: field "attendees" (Sexplib.Sexp.List attendees)
        :: field "attendee_values" (Sexplib.Sexp.List attendee_values)
        :: (duration_repeat_fields alarm.duration_repeat
           @ parameterized_option_fields "summary" "summary_parameters"
               alarm.summary
           @ attachment_value alarm.special.attach
           @ alarm_other_value alarm.other))
  | `None (alarm : unit Icalendar.alarm_struct) ->
      Sexplib.Sexp.List
        (field "action" (atom "none")
        :: field "trigger" (alarm_trigger_sexp alarm.trigger)
        :: (duration_repeat_fields alarm.duration_repeat
           @ parameterized_option_fields "summary" "summary_parameters"
               alarm.summary
           @ alarm_other_value alarm.other))

let event_wire_sexp ?occurrence_timezone event =
  let start =
    Event.get_start_result ~floating_tz:Timedesc.Time_zone.utc event
  in
  let end_ = Event.get_end_result ~floating_tz:Timedesc.Time_zone.utc event in
  let format_in timezone timestamp =
    let timezone =
      Option.bind timezone Timedesc.Time_zone.make
      |> Option.value ~default:Timedesc.Time_zone.utc
    in
    let local = Date.ptime_to_timedesc ~tz:timezone timestamp in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d" (Timedesc.year local)
      (Timedesc.month local) (Timedesc.day local) (Timedesc.hour local)
      (Timedesc.minute local) (Timedesc.second local)
  in
  let optional_field name = function
    | None -> []
    | Some value -> [ field name (atom value) ]
  in
  let base =
    [ field "id" (atom (Event.get_id event)) ]
    @ optional_field "summary" (Event.get_summary event)
    @ (match start with
      | Ok start ->
          [
            field "start"
              (atom (format_in (Event.get_start_timezone event) start));
            field "start_local" (atom (format_in None start));
            field "start_utc" (atom (Date.rfc3339_utc start));
          ]
      | Error _ -> [])
    @ optional_field "start_tz" (Event.get_start_timezone event)
    @ (match end_ with
      | Ok (Some end_) ->
          [
            field "end" (atom (format_in (Event.get_end_timezone event) end_));
            field "end_local" (atom (format_in None end_));
          ]
      | Ok None | Error _ -> [])
    @ optional_field "end_tz" (Event.get_end_timezone event)
    @ optional_field "location" (Event.get_location event)
    @ optional_field "description" (Event.get_description event)
    @ (match Event.get_alarms event with
      | [] -> []
      | alarms ->
          [ field "alarms" (atom (Format_utils.format_alarms_short alarms)) ])
    @ (if Event.is_date event then [ field "is_date" (atom "true") ] else [])
    @
    if Event.has_recurrence_set event then [ field "recurring" (atom "true") ]
    else []
  in
  let base, query_local_fields =
    match Option.bind occurrence_timezone Timedesc.Time_zone.make with
    | None -> (base, [])
    | Some timezone ->
        let local_value instant =
          let local = Date.ptime_to_timedesc ~tz:timezone instant in
          Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d" (Timedesc.year local)
            (Timedesc.month local) (Timedesc.day local) (Timedesc.hour local)
            (Timedesc.minute local) (Timedesc.second local)
        in
        let start = Event.get_start_result ~floating_tz:timezone event in
        let end_ = Event.get_end_result ~floating_tz:timezone event in
        let fields =
          (match start with
            | Ok start ->
                [
                  field "start_local" (atom (local_value start));
                  field "start_utc" (atom (Date.rfc3339_utc start));
                ]
            | Error _ -> [])
          @
          match end_ with
          | Ok (Some end_) -> [ field "end_local" (atom (local_value end_)) ]
          | Ok None | Error _ -> []
        in
        let replaced = [ "start_local"; "start_utc"; "end_local" ] in
        ( List.filter
            (function
              | Sexplib.Sexp.List [ Sexplib.Sexp.Atom name; _ ] ->
                  not (List.mem name replaced)
              | _ -> true)
            base,
          fields )
  in
  let raw = Event.master event in
  let recurrence_id =
    List.find_map
      (function `Recur_id (_, value) -> Some value | _ -> None)
      raw.props
  in
  let occurrence_fields =
    match recurrence_id with
    | None -> []
    | Some recurrence_id -> (
        let identity_fields =
          [
            field "is_occurrence" (atom "true");
            field "recurring" (atom "true");
            field "recurrence_id_value" (calendar_time_sexp recurrence_id);
          ]
        in
        match occurrence_timezone with
        | None -> identity_fields
        | Some timezone -> (
            match Timedesc.Time_zone.make timezone with
            | None -> identity_fields
            | Some date_tz -> (
                match
                  Date.ptime_of_ical_result ~floating_tz:date_tz recurrence_id
                with
                | Error _ -> identity_fields
                | Ok occurrence_start ->
                    identity_fields
                    @ [
                        field "occurrence_start"
                          (atom (Date.rfc3339_utc occurrence_start));
                        field "occurrence_timezone" (atom timezone);
                      ])))
  in
  let series_master_fields = match recurrence_id with None | Some _ -> [] in
  let recurrence_content_lines, recurrence_error_fields =
    match
      Calendar_codec.canonical_event_recurrence_lines
        ~date_until:(Event.date_until event) raw
    with
    | Ok lines -> (lines, [])
    | Error message -> ([], [ field "recurrence_set_error" (atom message) ])
  in
  let recurrence_value =
    List.find_map
      (fun line ->
        if String.starts_with ~prefix:"RRULE:" line then
          Some
            (field "recurrence_value"
               (Sexplib.Sexp.List
                  [
                    field "rrule"
                      (atom (String.sub line 6 (String.length line - 6)));
                  ]))
        else None)
      recurrence_content_lines
  in
  let recurrence_set_value =
    match recurrence_content_lines with
    | [] -> None
    | lines ->
        Some
          (field "recurrence_set_value"
             (Sexplib.Sexp.List (List.map atom lines)))
  in
  let end_value =
    match raw.dtend_or_duration with
    | None -> []
    | Some (`Dtend (_, value)) ->
        [
          field "end_value"
            (Sexplib.Sexp.List
               [
                 field "kind" (atom "dtend");
                 field "value" (calendar_time_sexp value);
               ]);
        ]
    | Some (`Duration (_, duration)) ->
        [
          field "end_value"
            (Sexplib.Sexp.List
               [
                 field "kind" (atom "duration");
                 field "seconds"
                   (atom
                      (string_of_int
                         (Option.value ~default:0
                            (Ptime.Span.to_int_s duration))));
               ]);
        ]
  in
  Sexplib.Sexp.List
    (base
    @ [
        field "calendar_key" (atom "");
        field "source_fingerprint" (atom "");
        field "start_value" (calendar_time_sexp (snd raw.dtstart));
        field "alarms_value"
          (Sexplib.Sexp.List (List.map alarm_sexp raw.alarms));
        field "categories_value"
          (Sexplib.Sexp.List (List.map atom (Event.get_categories event)));
      ]
    @ Option.to_list recurrence_value
    @ Option.to_list recurrence_set_value
    @ recurrence_error_fields @ end_value @ query_local_fields
    @ occurrence_fields @ series_master_fields)

let stored_event_wire_base ?occurrence_timezone component =
  match Component.to_event component with
  | None -> Sexplib.Sexp.List []
  | Some event -> (
      match event_wire_sexp ?occurrence_timezone event with
      | Sexplib.Sexp.List fields ->
          let replaced =
            [ "calendar"; "calendar_key"; "file"; "source_fingerprint" ]
          in
          let fields =
            List.filter
              (function
                | Sexplib.Sexp.List [ Sexplib.Sexp.Atom name; _ ] ->
                    not (List.mem name replaced)
                | _ -> true)
              fields
          in
          Sexplib.Sexp.List
            (fields
            @ [
                field "calendar" (atom (Component.get_calendar_name component));
                field "calendar_key"
                  (atom (Component.get_calendar_key component));
                field "file" (atom (snd (Component.get_file component)));
                field "source_fingerprint"
                  (atom (Component.get_source_fingerprint component));
              ])
      | value -> value)

let without_fields names = function
  | Sexplib.Sexp.List fields ->
      List.filter
        (function
          | Sexplib.Sexp.List [ Sexplib.Sexp.Atom name; _ ] ->
              not (List.mem name names)
          | _ -> true)
        fields
  | Sexplib.Sexp.Atom _ -> []

let event_end_value (raw : Icalendar.event) =
  match raw.dtend_or_duration with
  | None -> []
  | Some (`Dtend (_, value)) ->
      [
        field "end_value"
          (Sexplib.Sexp.List
             [
               field "kind" (atom "dtend");
               field "value" (calendar_time_sexp value);
             ]);
      ]
  | Some (`Duration (_, duration)) ->
      [
        field "end_value"
          (Sexplib.Sexp.List
             [
               field "kind" (atom "duration");
               field "seconds"
                 (atom
                    (string_of_int
                       (Option.value ~default:0 (Ptime.Span.to_int_s duration))));
             ]);
      ]

let source_ics_fields ~documents item =
  match Calendar_export.to_ics ~documents [ item ] with
  | Ok source -> [ field "source_ics" (atom source) ]
  | Error (`Msg message) ->
      [ field "source_ics" (atom ""); field "source_ics_error" (atom message) ]

let stored_event_wire_sexp ~documents ?occurrence_timezone component =
  let item = Component_query.Stored component in
  let fields =
    stored_event_wire_base ?occurrence_timezone component
    |> without_fields [ "source_ics"; "source_ics_error" ]
  in
  Sexplib.Sexp.List (fields @ source_ics_fields ~documents item)

let local_value ~timezone instant =
  let local = Date.ptime_to_timedesc ~tz:timezone instant in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d" (Timedesc.year local)
    (Timedesc.month local) (Timedesc.day local) (Timedesc.hour local)
    (Timedesc.minute local) (Timedesc.second local)

let source_timezone_value timezone instant =
  let timezone =
    Option.bind timezone Timedesc.Time_zone.make
    |> Option.value ~default:Timedesc.Time_zone.utc
  in
  local_value ~timezone instant

let occurrence_wire_sexp ~documents ~occurrence_timezone item stored_series
    occurrence =
  let raw = Event.Occurrence.effective_ical_event occurrence in
  let reference = Event.Occurrence.reference occurrence in
  let start = Event.Occurrence.get_start_result occurrence in
  let end_ = Event.Occurrence.get_end_result occurrence in
  let start_timezone = Event.Occurrence.get_start_timezone occurrence in
  let end_timezone = Event.Occurrence.get_end_timezone occurrence in
  let query_timezone =
    Timedesc.Time_zone.make occurrence_timezone
    |> Option.value
         ~default:(Event.Occurrence.Reference.query_timezone reference)
  in
  let start_fields =
    match start with
    | Error _ -> []
    | Ok start ->
        [
          field "start" (atom (source_timezone_value start_timezone start));
          field "start_local"
            (atom (local_value ~timezone:query_timezone start));
          field "start_utc" (atom (Date.rfc3339_utc start));
        ]
  in
  let end_fields =
    match end_ with
    | Ok (Some end_) ->
        [
          field "end" (atom (source_timezone_value end_timezone end_));
          field "end_local" (atom (local_value ~timezone:query_timezone end_));
        ]
    | Ok None | Error _ -> []
  in
  let optional_field name = function
    | None -> []
    | Some value -> [ field name (atom value) ]
  in
  let alarm_fields =
    match Event.Occurrence.get_alarms occurrence with
    | [] -> []
    | alarms ->
        [ field "alarms" (atom (Format_utils.format_alarms_short alarms)) ]
  in
  let identity_fields =
    [
      field "recurring" (atom "true");
      field "is_occurrence" (atom "true");
      field "recurrence_id_value"
        (calendar_time_sexp
           (Event.Occurrence.Reference.recurrence_id reference));
      field "occurrence_start"
        (atom
           (Event.Occurrence.Reference.occurrence_start reference
           |> Date.rfc3339_utc));
      field "occurrence_timezone" (atom occurrence_timezone);
    ]
  in
  let source_fields =
    [
      field "calendar" (atom (Component_query.get_calendar_name item));
      field "calendar_key" (atom (Component_query.get_calendar_key item));
      field "file" (atom (snd (Component_query.get_file item)));
      field "source_fingerprint"
        (atom (Component_query.get_source_fingerprint item));
    ]
  in
  let fields =
    [ field "id" (atom (Component_query.get_id item)) ]
    @ optional_field "summary" (Event.Occurrence.get_summary occurrence)
    @ start_fields
    @ optional_field "start_tz" start_timezone
    @ end_fields
    @ optional_field "end_tz" end_timezone
    @ optional_field "location" (Event.Occurrence.get_location occurrence)
    @ optional_field "description" (Event.Occurrence.get_description occurrence)
    @ alarm_fields
    @ (if Event.Occurrence.is_date occurrence then
         [ field "is_date" (atom "true") ]
       else [])
    @ source_fields
    @ [
        field "start_value" (calendar_time_sexp (snd raw.dtstart));
        field "alarms_value"
          (Sexplib.Sexp.List
             (List.map alarm_sexp (Event.Occurrence.get_alarms occurrence)));
        field "categories_value"
          (Sexplib.Sexp.List
             (List.map atom (Event.Occurrence.get_categories occurrence)));
      ]
    @ source_ics_fields ~documents item
    @ event_end_value raw @ identity_fields
    @ [
        field "series_master"
          (stored_event_wire_sexp ~documents ~occurrence_timezone stored_series);
      ]
  in
  Sexplib.Sexp.List fields

let query_item_wire_sexp ~documents ~occurrence_timezone = function
  | Component_query.Stored component ->
      stored_event_wire_sexp ~documents ~occurrence_timezone component
  | Component_query.Occurrence { stored_series; occurrence } as item ->
      occurrence_wire_sexp ~documents ~occurrence_timezone item stored_series
        occurrence

let sexp_of_response_payload = function
  | Hello { protocol_version; server_version; capabilities } ->
      Sexplib.Sexp.List
        [
          atom "Hello";
          Sexplib.Sexp.List
            [
              field "protocol_version" (atom (string_of_int protocol_version));
              field "server_version" (atom server_version);
              field "capabilities"
                (Sexplib.Sexp.List (List.map atom capabilities));
            ];
        ]
  | Calendars calendars ->
      Sexplib.Sexp.List
        [ atom "Calendars"; Sexplib.Sexp.List (List.map atom calendars) ]
  | Events { events; occurrence_timezone; documents } ->
      let event_sexp item =
        match occurrence_timezone with
        | Some timezone ->
            query_item_wire_sexp ~documents ~occurrence_timezone:timezone item
        | None -> (
            match item with
            | Component_query.Stored component ->
                stored_event_wire_sexp ~documents component
            | Component_query.Occurrence _ ->
                invalid_arg
                  "An occurrence response requires an occurrence timezone")
      in
      Sexplib.Sexp.List
        [ atom "Events"; Sexplib.Sexp.List (List.map event_sexp events) ]
  | Empty -> atom "Empty"

type protocol_error = { code : string; message : string; retryable : bool }
[@@deriving sexp_of]

type response = Ok of response_payload | Error of protocol_error
[@@deriving sexp_of]

type request_envelope = {
  version : int;
  request_id : string;
  request : request;
}
[@@deriving sexp]

type response_envelope = {
  version : int;
  request_id : string;
  response : response;
}
[@@deriving sexp_of]

type wire_request = Request of request_envelope [@@deriving sexp]
type wire_response = Response of response_envelope [@@deriving sexp_of]

let protocol_version = 1

let protocol_error ?(retryable = false) ~code message =
  { code; message; retryable }

let valid_request_id request_id =
  request_id <> ""
  && String.length request_id <= 256
  && Uutf.String.fold_utf_8
       (fun valid _ -> function
         | `Malformed _ -> false
         | `Uchar uchar -> (
             valid
             &&
             match Uucp.Gc.general_category uchar with
             | `Cc | `Cf | `Cs | `Cn -> false
             | _ -> true))
       true request_id

let parse_wire_request sexp =
  match wire_request_of_sexp sexp with
  | Request envelope when envelope.request_id = "" ->
      Result.Error
        ( "unknown",
          protocol_error ~code:"invalid_request"
            "request_id must be a non-empty string" )
  | Request envelope when not (valid_request_id envelope.request_id) ->
      Result.Error
        ( "unknown",
          protocol_error ~code:"invalid_request"
            "request_id must be at most 256 bytes of printable UTF-8 without \
             control characters" )
  | Request envelope when envelope.version = protocol_version ->
      Result.Ok envelope
  | Request envelope ->
      Result.Error
        ( envelope.request_id,
          protocol_error ~code:"unsupported_version"
            (Printf.sprintf "protocol version %d is unsupported; expected %d"
               envelope.version protocol_version) )

let wire_response ~request_id response =
  Response { version = protocol_version; request_id; response }

let parse_timezone ~timezone =
  match timezone with
  | Some tzid -> Component_query.timezone_of_name tzid
  | None -> Ok (Date.local_timezone ())

let query_text_field = function
  | Summary -> Component_query.Summary
  | Description -> Component_query.Description
  | Location -> Component_query.Location
  | Categories -> Component_query.Categories

let generate_query_params ~now (req : query_request) =
  let ( let* ) = Result.bind in
  let* () =
    match req.overdue with
    | None -> Ok ()
    | Some _ ->
        Error
          (`Msg
             "unsupported capability: overdue is unavailable for event-only \
              queries")
  in
  let* tz = parse_timezone ~timezone:req.timezone in
  let* from =
    match req.from with
    | None -> Ok None
    | Some s -> Result.map Option.some (Date.parse_date ~now ~tz s `From)
  in
  let* to_ =
    let* to_date = Date.parse_date ~now ~tz req.to_ `To in
    Date.next_midnight_result ~tz to_date
    |> Result.map_error (fun error ->
        `Msg (Date.string_of_conversion_error error))
  in
  let* statuses =
    let rec parse accumulated = function
      | [] -> Result.Ok (List.rev accumulated)
      | status :: rest ->
          let* status = Component_query.status_of_string status in
          parse (status :: accumulated) rest
    in
    parse [] req.statuses
  in
  let criteria =
    Component_query.
      {
        no_criteria with
        calendars = req.calendars;
        component_types = [ Component_kind.Event ];
        text = req.text;
        text_fields = List.map query_text_field req.search_in;
        categories = req.categories;
        id = req.id;
        statuses;
        overdue = req.overdue;
        recurring = req.recurring;
        has_alarm = req.has_alarm;
      }
  in
  let* () = Component_query.validate_criteria criteria in
  Ok (criteria, from, to_, req.limit, tz)
