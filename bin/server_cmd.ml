open Eio
open Cmdliner
open Caledonia_lib
open Caledonia_lib.Sexp

let protocol_storage_error error =
  let message = Storage_error.message error in
  if Storage_error.is_conflict error then `Conflict message else `Msg message

let protocol_storage_result result =
  Result.map_error protocol_storage_error result

let server_version = "0.5.0"

let decimal_slice value offset length =
  if offset + length > String.length value then None
  else
    String.sub value offset length
    |> String.to_seq
    |> Seq.for_all (function '0' .. '9' -> true | _ -> false)
    |> function
    | false -> None
    | true -> int_of_string_opt (String.sub value offset length)

let parse_protocol_date value =
  match
    ( String.length value,
      decimal_slice value 0 4,
      decimal_slice value 5 2,
      decimal_slice value 8 2 )
  with
  | 10, Some year, Some month, Some day when value.[4] = '-' && value.[7] = '-'
    -> (
      let date = (year, month, day) in
      match Ptime.of_date date with
      | Some _ -> Result.Ok date
      | None -> Result.Error (`Msg "date value is outside the calendar range"))
  | _ -> Result.Error (`Msg "date value must use exact YYYY-MM-DD format")

let parse_protocol_datetime value =
  let ( let* ) = Result.bind in
  if
    String.length value <> 19
    || value.[10] <> 'T'
    || value.[13] <> ':'
    || value.[16] <> ':'
  then
    Result.Error
      (`Msg "datetime value must use exact YYYY-MM-DDTHH:MM:SS format")
  else
    let* date = parse_protocol_date (String.sub value 0 10) in
    match
      ( decimal_slice value 11 2,
        decimal_slice value 14 2,
        decimal_slice value 17 2 )
    with
    | Some hour, Some minute, Some second -> (
        match Ptime.of_date_time (date, ((hour, minute, second), 0)) with
        | Some timestamp -> Result.Ok timestamp
        | None ->
            Result.Error (`Msg "datetime value is outside the calendar range"))
    | _ ->
        Result.Error
          (`Msg "datetime value must use exact YYYY-MM-DDTHH:MM:SS format")

let parse_required_start input =
  let ( let* ) = Result.bind in
  match input.kind with
  | Date ->
      let* date = parse_protocol_date input.value in
      Result.Ok
        ( Icalendar.Params.add Icalendar.Valuetype `Date Icalendar.Params.empty,
          `Date date )
  | Utc ->
      let* timestamp = parse_protocol_datetime input.value in
      Result.Ok (Icalendar.Params.empty, `Datetime (`Utc timestamp))
  | Floating ->
      let* timestamp = parse_protocol_datetime input.value in
      Result.Ok (Icalendar.Params.empty, `Datetime (`Local timestamp))
  | Tzid timezone ->
      let* () =
        match Timedesc.Time_zone.make timezone with
        | Some _ -> Result.Ok ()
        | None ->
            Result.Error (`Msg (Printf.sprintf "unknown timezone %S" timezone))
      in
      let* timestamp = parse_protocol_datetime input.value in
      Result.Ok
        ( Icalendar.Params.empty,
          `Datetime (`With_tzid (timestamp, (false, timezone))) )

let parse_required_end = function
  | Duration_seconds seconds ->
      Result.Ok
        (`Duration (Icalendar.Params.empty, Ptime.Span.of_int_s seconds))
  | Dtend input ->
      Result.map
        (fun (params, value) -> `Dtend (params, value))
        (parse_required_start input)

let valid_parameter_name name =
  name <> ""
  && String.length name <= 128
  && String.for_all
       (function
         | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' -> true | _ -> false)
       name

let uri_alarm_parameter_names =
  [ "ALTREP"; "DELEGATED-FROM"; "DELEGATED-TO"; "DIR"; "MEMBER"; "SENT-BY" ]

let validate_alarm_parameter_value ~name value =
  let ( let* ) = Result.bind in
  let invalid () =
    Result.Error
      (`Msg (Printf.sprintf "invalid lexical value for alarm parameter %s" name))
  in
  let* () =
    Alarm.validate_content_line_value ~field:("alarm parameter " ^ name) value
  in
  let length = String.length value in
  let rec quoted index =
    if index >= length then invalid ()
    else if value.[index] = '"' then
      if index + 1 = length then Result.Ok ()
      else if value.[index + 1] = ',' then item (index + 2)
      else invalid ()
    else quoted (index + 1)
  and unquoted index =
    if index = length then Result.Ok ()
    else
      match value.[index] with
      | ',' -> item (index + 1)
      | '"' | ';' | ':' -> invalid ()
      | _ -> unquoted (index + 1)
  and item index =
    if index >= length then invalid ()
    else if value.[index] = '"' then
      if index + 1 >= length || value.[index + 1] = '"' then invalid ()
      else quoted (index + 1)
    else unquoted index
  in
  item 0

let validate_alarm_parameter_uri_source ({ name; value } : alarm_parameter) =
  if List.mem (String.uppercase_ascii name) uri_alarm_parameter_names then
    let without_quotes =
      value |> String.to_seq
      |> Seq.filter (fun character -> character <> '"')
      |> String.of_seq
    in
    Alarm.validate_uri ~field:("alarm parameter " ^ name) without_quotes
  else Result.Ok ()

let verify_parsed_alarm_parameters ~submitted ~serialized params =
  let same_binding ({ name; value } : alarm_parameter) =
    List.exists
      (fun (parsed_name, parsed_value) ->
        String.equal
          (String.uppercase_ascii parsed_name)
          (String.uppercase_ascii name)
        && String.equal parsed_value value)
      serialized
  in
  if
    Icalendar.Params.cardinal params <> List.length submitted
    || List.length serialized <> List.length submitted
    || not (List.for_all same_binding submitted)
  then
    Result.Error
      (`Msg
         "alarm parameters did not parse as the exact submitted name/value \
          bindings")
  else Result.Ok ()

let validate_alarm_parameter_uris params =
  let ( let* ) = Result.bind in
  let validate_one field = function
    | None -> Result.Ok ()
    | Some uri -> Alarm.validate_uri ~field (Uri.to_string uri)
  in
  let validate_many field = function
    | None -> Result.Ok ()
    | Some uris ->
        let rec loop = function
          | [] -> Result.Ok ()
          | uri :: rest ->
              let* () = Alarm.validate_uri ~field (Uri.to_string uri) in
              loop rest
        in
        loop uris
  in
  let* () =
    validate_one "alarm ALTREP parameter URI"
      (Icalendar.Params.find Icalendar.Altrep params)
  in
  let* () =
    validate_one "alarm DIR parameter URI"
      (Icalendar.Params.find Icalendar.Dir params)
  in
  let* () =
    validate_one "alarm SENT-BY parameter URI"
      (Icalendar.Params.find Icalendar.Sentby params)
  in
  let* () =
    validate_many "alarm MEMBER parameter URI"
      (Icalendar.Params.find Icalendar.Member params)
  in
  let* () =
    validate_many "alarm DELEGATED-FROM parameter URI"
      (Icalendar.Params.find Icalendar.Delegated_from params)
  in
  validate_many "alarm DELEGATED-TO parameter URI"
    (Icalendar.Params.find Icalendar.Delegated_to params)

let parse_alarm_parameters parameters =
  let ( let* ) = Result.bind in
  let* () =
    let rec validate seen = function
      | [] -> Result.Ok ()
      | ({ name; value } : alarm_parameter) :: rest ->
          let canonical = String.uppercase_ascii name in
          if not (valid_parameter_name name) then
            Result.Error
              (`Msg (Printf.sprintf "invalid alarm parameter name %S" name))
          else if
            String.length value > 4096
            || String.contains value '\r' || String.contains value '\n'
          then
            Result.Error
              (`Msg (Printf.sprintf "invalid value for alarm parameter %s" name))
          else if List.mem canonical seen then
            Result.Error
              (`Msg (Printf.sprintf "duplicate alarm parameter %s" name))
          else
            let* () = validate_alarm_parameter_value ~name value in
            let* () = validate_alarm_parameter_uri_source { name; value } in
            validate (canonical :: seen) rest
    in
    validate [] parameters
  in
  match parameters with
  | [] -> Result.Ok Icalendar.Params.empty
  | _ ->
      let encoded =
        parameters
        |> List.map (fun ({ name; value } : alarm_parameter) ->
            name ^ "=" ^ value)
        |> String.concat ";"
      in
      let* params, serialized =
        match Calendar_codec.parse_content_line_parameters encoded with
        | Ok parsed -> Result.Ok parsed
        | Error message ->
            Result.Error (`Msg ("invalid alarm parameters: " ^ message))
      in
      let* () =
        verify_parsed_alarm_parameters ~submitted:parameters ~serialized params
      in
      let* () = validate_alarm_parameter_uris params in
      Result.Ok params

let parse_alarm_uri ~field value =
  let ( let* ) = Result.bind in
  let* () = Alarm.validate_uri ~field value in
  try Result.Ok (Uri.of_string value)
  with Invalid_argument message ->
    Result.Error (`Msg (Printf.sprintf "%s is invalid: %s" field message))

let find_raw_alarm_parameter name parameters =
  List.find_opt
    (fun (parameter : alarm_parameter) ->
      String.equal (String.uppercase_ascii parameter.name) name)
    parameters

let validate_exact_alarm_parameter ~field ~name ~value parameters =
  match find_raw_alarm_parameter name parameters with
  | None -> Result.Ok ()
  | Some parameter
    when String.equal parameter.name name && String.equal parameter.value value
    ->
      Result.Ok ()
  | Some _ ->
      Result.Error
        (`Msg
           (Printf.sprintf "%s requires the exact parameter %s=%s" field name
              value))

let reject_alarm_parameter ~field ~name parameters =
  match find_raw_alarm_parameter name parameters with
  | None -> Result.Ok ()
  | Some _ ->
      Result.Error
        (`Msg (Printf.sprintf "%s cannot declare parameter %s" field name))

let validate_alarm_text ~field = function
  | None -> Result.Ok ()
  | Some value ->
      if String.length value > Alarm.max_content_line_value_bytes then
        Result.Error
          (`Msg
             (Printf.sprintf "%s exceeds the %d-byte limit" field
                Alarm.max_content_line_value_bytes))
      else
        let decoder = Uutf.decoder ~encoding:`UTF_8 (`String value) in
        let rec decode () =
          match Uutf.decode decoder with
          | `Uchar scalar ->
              let scalar = Uchar.to_int scalar in
              if
                (scalar < 0x20 && scalar <> 0x09 && scalar <> 0x0a)
                || (scalar >= 0x7f && scalar <= 0x9f)
              then
                Result.Error
                  (`Msg
                     (Printf.sprintf "%s contains a forbidden control character"
                        field))
              else decode ()
          | `End -> Result.Ok ()
          | `Malformed _ ->
              Result.Error
                (`Msg (Printf.sprintf "%s must be valid UTF-8" field))
          | `Await -> assert false
        in
        decode ()

let parse_alarm (request : alarm_input) =
  let ( let* ) = Result.bind in
  let* () = validate_alarm_text ~field:"alarm summary" request.summary in
  let* () =
    validate_alarm_text ~field:"alarm description" request.description
  in
  let* () =
    match (request.duration_seconds, request.duration_parameters) with
    | None, _ :: _ ->
        Result.Error (`Msg "duration_parameters require alarm duration_seconds")
    | None, [] | Some _, _ -> Result.Ok ()
  in
  let* () =
    match (request.repeat, request.repeat_parameters) with
    | None, _ :: _ ->
        Result.Error (`Msg "repeat_parameters require alarm repeat")
    | None, [] | Some _, _ -> Result.Ok ()
  in
  let* () =
    match (request.description, request.description_parameters) with
    | None, _ :: _ ->
        Result.Error
          (`Msg "description_parameters require an alarm description")
    | None, [] | Some _, _ -> Result.Ok ()
  in
  let has_attendees =
    request.attendees <> [] || request.attendee_values <> []
  in
  let* () =
    if request.attendees <> [] && request.attendee_values <> [] then
      Result.Error
        (`Msg
           "email alarm attendees and attendee_values are aliases and cannot \
            both be supplied")
    else Result.Ok ()
  in
  let* () =
    match request.action with
    | Email -> Result.Ok ()
    | Audio when Option.is_some request.summary ->
        Result.Error (`Msg "AUDIO alarms cannot contain SUMMARY")
    | Audio when Option.is_some request.description ->
        Result.Error (`Msg "AUDIO alarms cannot contain DESCRIPTION")
    | Audio when has_attendees ->
        Result.Error (`Msg "AUDIO alarms cannot contain ATTENDEE")
    | Display when Option.is_some request.attachment ->
        Result.Error (`Msg "DISPLAY alarms cannot contain ATTACH")
    | Display when Option.is_some request.summary ->
        Result.Error (`Msg "DISPLAY alarms cannot contain SUMMARY")
    | Display when has_attendees ->
        Result.Error (`Msg "DISPLAY alarms cannot contain ATTENDEE")
    | None_action when Option.is_some request.description ->
        Result.Error (`Msg "NONE alarms cannot contain DESCRIPTION")
    | None_action when Option.is_some request.summary ->
        Result.Error (`Msg "NONE alarms cannot contain SUMMARY")
    | None_action when Option.is_some request.attachment ->
        Result.Error (`Msg "NONE alarms cannot contain ATTACH")
    | None_action when has_attendees ->
        Result.Error (`Msg "NONE alarms cannot contain ATTENDEE")
    | Audio | Display | None_action -> Result.Ok ()
  in
  let* trigger_parameters = parse_alarm_parameters request.trigger_parameters in
  let* trigger =
    match request.trigger with
    | Relative { seconds; related } ->
        let related = match related with Start -> `Start | End -> `End in
        let related_value =
          match related with `Start -> "START" | `End -> "END"
        in
        let* () =
          validate_exact_alarm_parameter ~field:"relative alarm trigger"
            ~name:"VALUE" ~value:"DURATION" request.trigger_parameters
        in
        let* () =
          validate_exact_alarm_parameter ~field:"relative alarm trigger"
            ~name:"RELATED" ~value:related_value request.trigger_parameters
        in
        let* params =
          match
            Icalendar.Params.find Icalendar.Valuetype trigger_parameters
          with
          | None ->
              Result.Ok
                (Icalendar.Params.add Icalendar.Valuetype `Duration
                   trigger_parameters)
          | Some `Duration -> Result.Ok trigger_parameters
          | Some _ ->
              Result.Error
                (`Msg "relative alarm trigger requires VALUE=DURATION")
        in
        let* params =
          match Icalendar.Params.find Icalendar.Related params with
          | None ->
              Result.Ok (Icalendar.Params.add Icalendar.Related related params)
          | Some existing when existing = related -> Result.Ok params
          | Some _ ->
              Result.Error
                (`Msg
                   "relative alarm trigger RELATED parameter conflicts with \
                    its structured relation")
        in
        Result.Ok (params, `Duration (Ptime.Span.of_int_s seconds))
    | Absolute value -> (
        let* () =
          validate_exact_alarm_parameter ~field:"absolute alarm trigger"
            ~name:"VALUE" ~value:"DATE-TIME" request.trigger_parameters
        in
        let* () =
          reject_alarm_parameter ~field:"absolute alarm trigger" ~name:"RELATED"
            request.trigger_parameters
        in
        let* params =
          match
            Icalendar.Params.find Icalendar.Valuetype trigger_parameters
          with
          | None ->
              Result.Ok
                (Icalendar.Params.add Icalendar.Valuetype `Datetime
                   trigger_parameters)
          | Some `Datetime -> Result.Ok trigger_parameters
          | Some _ ->
              Result.Error
                (`Msg "absolute alarm trigger requires VALUE=DATE-TIME")
        in
        match Ptime.of_rfc3339 value with
        | Result.Ok (timestamp, _, _)
          when Ptime.Span.compare (Ptime.frac_s timestamp) Ptime.Span.zero = 0
          ->
            Result.Ok (params, `Datetime timestamp)
        | Result.Ok _ ->
            Result.Error
              (`Msg "absolute alarm trigger must use whole RFC 3339 seconds")
        | Result.Error _ ->
            Result.Error (`Msg "absolute alarm trigger must be RFC 3339"))
  in
  let* duration_repeat =
    match (request.duration_seconds, request.repeat) with
    | None, None -> Result.Ok None
    | Some duration, Some repeat when repeat > 0 ->
        let* duration_params =
          parse_alarm_parameters request.duration_parameters
        in
        let* repeat_params = parse_alarm_parameters request.repeat_parameters in
        Result.Ok
          (Some
             ( (duration_params, Ptime.Span.of_int_s duration),
               (repeat_params, repeat) ))
    | _ ->
        Result.Error
          (`Msg "alarm duration_seconds and repeat must be supplied together")
  in
  let* summary_params = parse_alarm_parameters request.summary_parameters in
  let* summary =
    match (request.summary, request.summary_parameters) with
    | None, [] -> Result.Ok None
    | None, _ ->
        Result.Error (`Msg "summary_parameters require an alarm summary")
    | Some value, _ -> Result.Ok (Some (summary_params, value))
  in
  let* attachment_params =
    parse_alarm_parameters request.attachment_parameters
  in
  let* attachment =
    match (request.attachment, request.attachment_parameters) with
    | None, [] -> Result.Ok None
    | None, _ ->
        Result.Error (`Msg "attachment_parameters require an alarm attachment")
    | Some (Uri value), _ ->
        let* () =
          validate_exact_alarm_parameter ~field:"URI alarm attachment"
            ~name:"VALUE" ~value:"URI" request.attachment_parameters
        in
        let* () =
          match Icalendar.Params.find Icalendar.Valuetype attachment_params with
          | None | Some `Uri -> Result.Ok ()
          | Some _ ->
              Result.Error
                (`Msg "URI alarm attachment requires VALUE=URI when specified")
        in
        let* () =
          match
            find_raw_alarm_parameter "ENCODING" request.attachment_parameters
          with
          | None -> Result.Ok ()
          | Some _ ->
              Result.Error (`Msg "URI alarm attachment cannot declare ENCODING")
        in
        let* uri = parse_alarm_uri ~field:"URI alarm attachment" value in
        Result.Ok (Some (attachment_params, `Uri uri))
    | Some (Binary value), _ ->
        let* () = Alarm.validate_binary_attachment value in
        let* () =
          validate_exact_alarm_parameter ~field:"binary alarm attachment"
            ~name:"VALUE" ~value:"BINARY" request.attachment_parameters
        in
        let* () =
          match
            find_raw_alarm_parameter "ENCODING" request.attachment_parameters
          with
          | None -> Result.Ok ()
          | Some { name; value }
            when String.equal name "ENCODING" && String.equal value "BASE64" ->
              Result.Ok ()
          | Some _ ->
              Result.Error
                (`Msg "binary alarm attachment requires ENCODING=BASE64")
        in
        let* attachment_params =
          match Icalendar.Params.find Icalendar.Valuetype attachment_params with
          | None ->
              Result.Ok
                (Icalendar.Params.add Icalendar.Valuetype `Binary
                   attachment_params)
          | Some `Binary -> Result.Ok attachment_params
          | Some _ ->
              Result.Error
                (`Msg "binary alarm attachment requires VALUE=BINARY")
        in
        let attachment_params =
          match Icalendar.Params.find Icalendar.Encoding attachment_params with
          | Some `Base64 -> attachment_params
          | None ->
              Icalendar.Params.add Icalendar.Encoding `Base64 attachment_params
        in
        Result.Ok (Some (attachment_params, `Binary value))
  in
  let* other =
    Command_common.map_result
      (function
        | Iana { name; value; parameters } ->
            let* params = parse_alarm_parameters parameters in
            let property = `Iana_prop (name, params, value) in
            let* () = Alarm.validate_other_property property in
            Result.Ok property
        | X { namespace; name; value; parameters } ->
            let* params = parse_alarm_parameters parameters in
            let property = `Xprop ((namespace, name), params, value) in
            let* () = Alarm.validate_other_property property in
            Result.Ok property)
      request.other
  in
  let* description_params =
    parse_alarm_parameters request.description_parameters
  in
  match request.action with
  | Display ->
      Result.Ok
        (`Display
           Icalendar.
             {
               trigger;
               duration_repeat;
               summary;
               other;
               special =
                 Icalendar.
                   {
                     description =
                       Option.map
                         (fun value -> (description_params, value))
                         request.description;
                   };
             })
  | Audio ->
      Result.Ok
        (`Audio
           Icalendar.
             {
               trigger;
               duration_repeat;
               summary;
               other;
               special = Icalendar.{ attach = attachment };
             })
  | None_action ->
      Result.Ok
        (`None
           Icalendar.{ trigger; duration_repeat; summary; other; special = () })
  | Email ->
      let* description =
        match request.description with
        | Some value -> Result.Ok (description_params, value)
        | None -> Result.Error (`Msg "email alarms require description")
      in
      let* attendees =
        Command_common.map_result
          (fun value ->
            let* uri = parse_alarm_uri ~field:"alarm attendee URI" value in
            Result.Ok (Icalendar.Params.empty, uri))
          request.attendees
      in
      let* attendee_values =
        Command_common.map_result
          (fun ({ uri; parameters } : alarm_attendee) ->
            let* params = parse_alarm_parameters parameters in
            let* uri = parse_alarm_uri ~field:"alarm attendee URI" uri in
            Result.Ok (params, uri))
          request.attendee_values
      in
      let attendees = attendees @ attendee_values in
      Result.Ok
        (`Email
           Icalendar.
             {
               trigger;
               duration_repeat;
               summary;
               other;
               special =
                 Icalendar.{ description; attendees; attach = attachment };
             })

let parse_alarms requests =
  let rec loop parsed = function
    | [] -> Result.Ok (List.rev parsed)
    | request :: rest -> (
        match parse_alarm request with
        | Result.Ok alarm -> loop (alarm :: parsed) rest
        | Result.Error _ as error -> error)
  in
  loop [] requests

let parse_recurrence_source request =
  if String.contains request.rrule '\n' || String.contains request.rrule '\r'
  then Result.Error (`Msg "RRULE must be one unfolded content-line value")
  else
    Calendar_codec.parse_event_rrule request.rrule
    |> Result.map_error (fun message -> `Msg ("invalid RRULE: " ^ message))

let parse_start_patch = function
  | Keep -> Result.Ok Patch.Keep
  | Clear -> Result.Ok Patch.Clear
  | Set input ->
      Result.map (fun value -> Patch.Set value) (parse_required_start input)

let parse_end_patch = function
  | Keep -> Result.Ok Patch.Keep
  | Clear -> Result.Ok Patch.Clear
  | Set input ->
      Result.map (fun value -> Patch.Set value) (parse_required_end input)

let parse_recurrence_patch = function
  | Keep -> Result.Ok Patch.Keep
  | Clear -> Result.Ok Patch.Clear
  | Set recurrence ->
      Result.map
        (fun value -> Patch.Set value)
        (parse_recurrence_source recurrence)

let parse_alarm_patch = function
  | Keep -> Result.Ok Patch.Keep
  | Clear -> Result.Ok Patch.Clear
  | Set alarms ->
      Result.map (fun value -> Patch.Set value) (parse_alarms alarms)

let find_event components ~id ~calendar_key ~file =
  match
    List.filter
      (fun component ->
        Component.component_type component = Component_kind.Event
        && Component.get_id component = id
        && Component.get_calendar_key component = calendar_key
        && snd (Component.get_file component) = file)
      components
  with
  | [ component ] -> Result.Ok component
  | [] -> Result.Error (`Msg ("No event found for id " ^ id))
  | _ -> Result.Error (`Msg ("Multiple events found for id " ^ id))

let verify_fingerprint component expected =
  match (expected, Component.get_source_fingerprint component) with
  | None, _ ->
      Result.Error
        (`Conflict "mutation is missing its source fingerprint; reload first")
  | Some expected, actual when String.equal actual expected -> Result.Ok ()
  | Some _, _ ->
      Result.Error
        (`Conflict "calendar file changed since the form/query snapshot")

let parse_occurrence_context occurrence_start occurrence_timezone =
  match (occurrence_start, occurrence_timezone) with
  | None, None -> Result.Ok None
  | None, Some _ ->
      Result.Error (`Msg "occurrence_timezone requires occurrence_start")
  | Some _, None ->
      Result.Error
        (`Msg "occurrence_start requires the query occurrence_timezone")
  | Some occurrence_start, Some timezone -> (
      match
        (Ptime.of_rfc3339 occurrence_start, Timedesc.Time_zone.make timezone)
      with
      | Result.Error _, _ ->
          Result.Error (`Msg "occurrence_start must be an RFC 3339 timestamp")
      | _, None ->
          Result.Error
            (`Msg (Printf.sprintf "Unknown occurrence timezone %S" timezone))
      | Result.Ok (timestamp, _, _), Some timezone ->
          Result.Ok (Some (timestamp, timezone)))

let handle_request ~now ~fs calendar_dir request =
  let ( let* ) = Result.bind in
  match request with
  | Handshake ->
      Result.Ok
        (Hello
           {
             protocol_version;
             server_version;
             capabilities =
               [
                 "event-query";
                 "event-create";
                 "event-edit-patch";
                 "event-delete";
                 "occurrence-edit";
                 "occurrence-delete";
               ];
           })
  | ListCalendars ->
      let* names = Calendar_dir.list_calendar_names ~fs calendar_dir in
      Result.Ok (Calendars names)
  | Refresh -> Result.Ok Empty
  | Query query_request ->
      let query_params :
          ( Component_query.criteria
            * Ptime.t option
            * Ptime.t
            * int option
            * Timedesc.Time_zone.t,
            [ `Msg of string | `Conflict of string ] )
          result =
        generate_query_params ~now query_request
        |> Result.map_error (fun (`Msg message) -> `Msg message)
      in
      let* criteria, from, to_, limit, timezone = query_params in
      let* documents = Calendar_dir.get_documents ~fs calendar_dir in
      let components = List.concat_map Calendar_document.components documents in
      let query_result :
          ( Component_query.item list,
            [ `Msg of string | `Conflict of string ] )
          result =
        Component_query.run ~timezone ~now ~from ~to_ ?limit ~criteria
          components
        |> Result.map_error (fun (`Msg message) -> `Msg message)
      in
      let* items = query_result in
      Result.Ok
        (Events
           {
             events = items;
             occurrence_timezone = Some (Timedesc.Time_zone.name timezone);
             documents;
           })
  | CreateEvent request ->
      let* start = parse_required_start request.start in
      let* end_ =
        match request.end_ with
        | None -> Result.Ok None
        | Some input -> Result.map Option.some (parse_required_end input)
      in
      let* recurrence =
        match request.recurrence with
        | None -> Result.Ok None
        | Some recurrence ->
            Result.map Option.some (parse_recurrence_source recurrence)
      in
      let* alarms = parse_alarms request.alarms in
      let recurrence_params =
        Option.map (fun (params, _, _) -> params) recurrence
      in
      let recurrence_date_until =
        Option.bind recurrence (fun (_, _, date_until) -> date_until)
      in
      let recurrence =
        Option.map (fun (_, recurrence, _) -> recurrence) recurrence
      in
      let* event =
        Event.create ~now ~summary:request.summary ~start ?end_
          ?location:request.location ?description:request.description
          ~categories:request.categories ?recurrence ?recurrence_params
          ?recurrence_date_until ~alarms ()
      in
      let* canonical =
        Calendar_dir.create_stored_component ~fs calendar_dir
          ~calendar_key:request.calendar
          (Component.event_body event)
        |> protocol_storage_result
      in
      let* documents = Calendar_dir.get_documents ~fs calendar_dir in
      Result.Ok
        (Events
           {
             events = [ Component_query.Stored canonical ];
             occurrence_timezone = None;
             documents;
           })
  | EditEvent request -> (
      let* occurrence_context =
        parse_occurrence_context request.occurrence_start
          request.occurrence_timezone
      in
      let* components = Calendar_dir.get_components ~fs calendar_dir in
      let* component =
        find_event components ~id:request.id ~calendar_key:request.calendar_key
          ~file:request.file
      in
      let* () = verify_fingerprint component request.source_fingerprint in
      let event = Option.get (Component.to_event component) in
      let* start = parse_start_patch request.start in
      let* end_ = parse_end_patch request.end_ in
      let* recurrence = parse_recurrence_patch request.recurrence in
      let recurrence_params =
        match recurrence with
        | Patch.Set (params, _, _) -> Some params
        | Patch.Keep | Patch.Clear -> None
      in
      let recurrence_date_until =
        match recurrence with
        | Patch.Set (_, _, date_until) -> date_until
        | Patch.Keep | Patch.Clear -> None
      in
      let recurrence =
        match recurrence with
        | Patch.Keep -> Patch.Keep
        | Patch.Clear -> Patch.Clear
        | Patch.Set (_, recurrence, _) -> Patch.Set recurrence
      in
      let* alarms = parse_alarm_patch request.alarms in
      let summary = request.summary in
      let location = request.location in
      let description = request.description in
      let categories = request.categories in
      match occurrence_context with
      | None ->
          let* modified =
            Event.edit_patch ~now ~summary ~start ~end_ ~location ~description
              ~categories ~recurrence ?recurrence_params ?recurrence_date_until
              ~alarms event
          in
          let* canonical =
            Calendar_dir.replace_stored_component ~fs calendar_dir
              ~original:component
              ~replacement:(Component.event_body modified)
            |> protocol_storage_result
          in
          let* documents = Calendar_dir.get_documents ~fs calendar_dir in
          Result.Ok
            (Events
               {
                 events = [ Component_query.Stored canonical ];
                 occurrence_timezone = None;
                 documents;
               })
      | Some (occurrence_start, date_tz) ->
          let* () =
            match recurrence with
            | Patch.Keep -> Result.Ok ()
            | Patch.Set _ | Patch.Clear ->
                Result.Error
                  (`Msg "recurrence cannot be changed on one occurrence")
          in
          let resolved_reference :
              ( Event.Occurrence.Reference.t,
                [ `Msg of string | `Conflict of string ] )
              result =
            match
              Event.Recurrence.resolve_reference ~floating_tz:date_tz event
                occurrence_start
            with
            | Ok reference -> Ok reference
            | Error (`Msg message) -> Error (`Msg message)
          in
          let* reference = resolved_reference in
          let checked_override :
              (Icalendar.event, [ `Msg of string | `Conflict of string ]) result
              =
            match
              Event.Recurrence.create_override ~now event reference ~summary
                ~start ~end_ ~location ~description ~categories ~alarms ()
            with
            | Ok value -> Ok value
            | Error (`Msg message) -> Error (`Msg message)
          in
          let* override_event = checked_override in
          let* canonical =
            Calendar_dir.add_occurrence_override ~fs calendar_dir component
              reference override_event
            |> protocol_storage_result
          in
          let* documents = Calendar_dir.get_documents ~fs calendar_dir in
          Result.Ok
            (Events
               {
                 events = [ Component_query.Stored canonical ];
                 occurrence_timezone = None;
                 documents;
               }))
  | DeleteEvent request ->
      let* occurrence_context =
        parse_occurrence_context request.occurrence_start
          request.occurrence_timezone
      in
      let* components = Calendar_dir.get_components ~fs calendar_dir in
      let* component =
        find_event components ~id:request.id ~calendar_key:request.calendar_key
          ~file:request.file
      in
      let* () = verify_fingerprint component request.source_fingerprint in
      let* _outcome =
        match occurrence_context with
        | None ->
            Calendar_dir.remove_stored_component ~fs calendar_dir component
            |> protocol_storage_result
        | Some (occurrence_start, date_tz) ->
            let* event =
              match Component.to_event component with
              | Some event -> Ok event
              | None -> Error (`Msg "Occurrence deletion requires a VEVENT")
            in
            let resolved_reference :
                ( Event.Occurrence.Reference.t,
                  [ `Msg of string | `Conflict of string ] )
                result =
              match
                Event.Recurrence.resolve_reference ~floating_tz:date_tz event
                  occurrence_start
              with
              | Ok reference -> Ok reference
              | Error (`Msg message) -> Error (`Msg message)
            in
            let* reference = resolved_reference in
            Calendar_dir.delete_occurrence ~fs calendar_dir component reference
            |> protocol_storage_result
            |> Result.map (fun _ -> Calendar_dir.Document_rewritten [])
      in
      Result.Ok Empty

let classify_error message =
  let lowercase = String.lowercase_ascii message in
  if String.starts_with ~prefix:"unsupported capability:" lowercase then
    "unsupported_capability"
  else if String.starts_with ~prefix:"no event found" lowercase then "not_found"
  else if String.starts_with ~prefix:"multiple events found" lowercase then
    "ambiguous_identity"
  else if String.starts_with ~prefix:"conflict" lowercase then "conflict"
  else "invalid_request"

let error_response ?(request_id = "unknown") ?(retryable = false) ~code message
    =
  wire_response ~request_id (Error (protocol_error ~retryable ~code message))

let request_id_from_sexp = function
  | Sexplib.Sexp.List [ Sexplib.Sexp.Atom "Request"; Sexplib.Sexp.List fields ]
    ->
      List.find_map
        (function
          | Sexplib.Sexp.List
              [ Sexplib.Sexp.Atom "request_id"; Sexplib.Sexp.Atom request_id ]
            when valid_request_id request_id ->
              Some request_id
          | _ -> None)
        fields
  | _ -> None

let request_id_from_raw line =
  let marker = "(request_id" in
  let inspected_length = min (String.length line) 4096 in
  let marker_length = String.length marker in
  let rec find index =
    if index + marker_length > inspected_length then None
    else if String.sub line index marker_length = marker then Some index
    else find (index + 1)
  in
  let whitespace = function ' ' | '\t' -> true | _ -> false in
  let rec skip index =
    if index < inspected_length && whitespace line.[index] then skip (index + 1)
    else index
  in
  match find 0 with
  | None -> None
  | Some marker_start ->
      let value_start = skip (marker_start + marker_length) in
      if value_start >= inspected_length then None
      else
        let finish value_end closing =
          let closing = skip closing in
          if closing < inspected_length && line.[closing] = ')' then
            let value = String.sub line value_start (value_end - value_start) in
            if valid_request_id value then Some value else None
          else None
        in
        if line.[value_start] = '"' then
          let rec quoted index escaped =
            if index >= inspected_length then None
            else if escaped then quoted (index + 1) false
            else
              match line.[index] with
              | '\\' -> quoted (index + 1) true
              | '"' -> (
                  let encoded =
                    String.sub line value_start (index - value_start + 1)
                  in
                  match Sexplib.Sexp.of_string encoded with
                  | Sexplib.Sexp.Atom value when valid_request_id value ->
                      let closing = skip (index + 1) in
                      if closing < inspected_length && line.[closing] = ')' then
                        Some value
                      else None
                  | _ -> None
                  | exception Failure _ -> None
                  | exception Sexplib.Sexp.Parse_error _ -> None)
              | _ -> quoted (index + 1) false
          in
          quoted (value_start + 1) false
        else
          let rec atom_end index =
            if index >= inspected_length then None
            else if whitespace line.[index] || line.[index] = ')' then
              finish index index
            else atom_end (index + 1)
          in
          atom_end value_start

let valid_utf8 value =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String value) in
  let rec decode () =
    match Uutf.decode decoder with
    | `Uchar _ -> decode ()
    | `End -> true
    | `Malformed _ -> false
    | `Await -> assert false
  in
  decode ()

let max_request_line_bytes = 1_000_000
let max_response_line_bytes = 1_000_000

let rec discard_overlong_line reader =
  let buffered = Buf_read.buffered_bytes reader in
  if buffered > 0 then Buf_read.skip buffered reader;
  match Buf_read.line reader with
  | _ -> ()
  | exception End_of_file -> ()
  | exception Buf_read.Buffer_limit_exceeded -> discard_overlong_line reader

let run ~stdin ~stdout ~fs calendar_dir () =
  let reader = Buf_read.of_flow stdin ~max_size:max_request_line_bytes in
  let handshake_complete = ref false in
  let write_response response =
    let encode response =
      sexp_of_wire_response response |> Sexplib.Sexp.to_string
    in
    let response_line = encode response in
    let response_line =
      if String.length response_line <= max_response_line_bytes then
        response_line
      else
        let request_id =
          match response with Response envelope -> envelope.request_id
        in
        encode
          (error_response ~request_id ~code:"response_too_large"
             (Printf.sprintf
                "response exceeds the %d-byte limit; narrow the query or set a \
                 limit"
                max_response_line_bytes))
    in
    Flow.copy_string (response_line ^ "\n") stdout
  in
  let rec serve () =
    match Buf_read.line reader with
    | exception End_of_file -> ()
    | exception Buf_read.Buffer_limit_exceeded ->
        let prefix =
          Buf_read.take (min (Buf_read.buffered_bytes reader) 4096) reader
        in
        let request_id = request_id_from_raw prefix in
        discard_overlong_line reader;
        error_response
          ~request_id:(Option.value ~default:"unknown" request_id)
          ~code:"invalid_request"
          (Printf.sprintf "request line exceeds the %d-byte limit"
             max_request_line_bytes)
        |> write_response;
        serve ()
    | line ->
        let raw_request_id = request_id_from_raw line in
        let wire_response =
          if not (valid_utf8 line) then
            error_response
              ~request_id:(Option.value ~default:"unknown" raw_request_id)
              ~code:"invalid_request" "request line is not valid UTF-8"
          else
            try
              let sexp = Sexplib.Sexp.of_string line in
              let request_id =
                Option.value ~default:"unknown" (request_id_from_sexp sexp)
              in
              try
                match parse_wire_request sexp with
                | Result.Error (request_id, error) ->
                    wire_response ~request_id (Error error)
                | Result.Ok envelope ->
                    if
                      (not !handshake_complete) && envelope.request <> Handshake
                    then
                      error_response ~request_id:envelope.request_id
                        ~code:"handshake_required"
                        "Handshake must be the first request on a connection"
                    else if !handshake_complete && envelope.request = Handshake
                    then
                      error_response ~request_id:envelope.request_id
                        ~code:"invalid_request"
                        "Handshake has already completed on this connection"
                    else (
                      if envelope.request = Handshake then
                        handshake_complete := true;
                      match
                        handle_request ~now:(Ptime_clock.now ()) ~fs
                          calendar_dir envelope.request
                      with
                      | Result.Ok payload ->
                          wire_response ~request_id:envelope.request_id
                            (Ok payload)
                      | Result.Error (`Msg message) ->
                          error_response ~request_id:envelope.request_id
                            ~code:(classify_error message) message
                      | Result.Error (`Conflict message) ->
                          error_response ~request_id:envelope.request_id
                            ~retryable:true ~code:"conflict" message)
              with
              | Sexplib.Conv.Of_sexp_error (_, bad_sexp) ->
                  error_response ~request_id ~code:"invalid_request"
                    ("invalid protocol request: "
                    ^ Sexplib.Sexp.to_string bad_sexp)
              | Failure message ->
                  error_response ~request_id ~code:"invalid_request" message
            with
            | Failure message ->
                error_response
                  ~request_id:(Option.value ~default:"unknown" raw_request_id)
                  ~code:"invalid_request" message
            | Sexplib.Sexp.Parse_error _ ->
                error_response
                  ~request_id:(Option.value ~default:"unknown" raw_request_id)
                  ~code:"invalid_request" "invalid S-expression request"
            | exn ->
                error_response
                  ~request_id:(Option.value ~default:"unknown" raw_request_id)
                  ~code:"internal_error"
                  ("unexpected server error: " ^ Printexc.to_string exn)
        in
        write_response wire_response;
        serve ()
  in
  serve ()

let cmd ~stdin ~stdout ~fs calendar_dir =
  let run () =
    run ~stdin ~stdout ~fs calendar_dir ();
    0
  in
  let term = Term.(const run) in
  let doc = "Process protocol-v1 S-expression requests from stdin" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Read one versioned Request envelope per line and emit one correlated \
         Response envelope per line. Diagnostics are written only to stderr.";
      `P
        "Request lines are limited to 1000000 bytes. Request identifiers must \
         be 1 to 256 bytes of printable UTF-8 without control characters. \
         Invalid identifiers and input that cannot be correlated safely use \
         the identifier 'unknown' in the error response.";
      `P
        "Alarm binary attachments must be non-empty, canonically padded base64 \
         and are limited to 750000 encoded bytes. Alarm attachment and \
         attendee URIs must be absolute RFC 3986 URIs and are limited to 8192 \
         bytes. Absolute alarm triggers must use whole RFC 3339 seconds. \
         Standard alarm parameters are accepted only on their RFC 5545 \
         properties. Invalid alarm input is rejected before mutation.";
      `P "The first request must be a Handshake.";
      `S Manpage.s_examples;
      `Pre
        "echo '(Request ((version 1) (request_id req-1) (request Handshake)))' \
         | $(mname) $(tname)";
    ]
  in
  Cmd.v (Cmd.info "server" ~doc ~man) term
