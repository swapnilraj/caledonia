open Icalendar

type 'owner fire = {
  fire_time : Ptime.t;
  owner : 'owner;
  alarm : Icalendar.alarm;
  alarm_index : int;
}

let ( let* ) = Result.bind
let nonempty value = String.trim value <> ""
let max_binary_attachment_bytes = 750_000
let max_uri_bytes = 8_192
let max_content_line_value_bytes = 65_536

let base64_digit = function
  | 'A' .. 'Z' as value -> Some (Char.code value - Char.code 'A')
  | 'a' .. 'z' as value -> Some (Char.code value - Char.code 'a' + 26)
  | '0' .. '9' as value -> Some (Char.code value - Char.code '0' + 52)
  | '+' -> Some 62
  | '/' -> Some 63
  | _ -> None

let validate_binary_attachment value =
  let length = String.length value in
  let invalid reason = Error (`Msg ("binary alarm attachment " ^ reason)) in
  if length = 0 then invalid "must not be empty"
  else if length > max_binary_attachment_bytes then
    invalid
      (Printf.sprintf "exceeds the %d-byte encoded-size limit"
         max_binary_attachment_bytes)
  else if length mod 4 <> 0 then
    invalid "must be padded base64 with a length divisible by four"
  else
    let padding =
      if value.[length - 1] <> '=' then 0
      else if length >= 2 && value.[length - 2] = '=' then 2
      else 1
    in
    let data_length = length - padding in
    let rec validate_alphabet index =
      if index = data_length then Ok ()
      else
        match base64_digit value.[index] with
        | Some _ -> validate_alphabet (index + 1)
        | None -> invalid "contains a non-base64 character or misplaced padding"
    in
    let* () = validate_alphabet 0 in
    let rec validate_padding index =
      if index = length then Ok ()
      else if value.[index] = '=' then validate_padding (index + 1)
      else invalid "contains misplaced padding"
    in
    let* () = validate_padding data_length in
    match padding with
    | 0 -> Ok ()
    | 1 -> (
        match base64_digit value.[data_length - 1] with
        | Some digit when digit land 0x03 = 0 -> Ok ()
        | Some _ -> invalid "has non-zero unused bits in its final quantum"
        | None -> assert false)
    | 2 -> (
        match base64_digit value.[data_length - 1] with
        | Some digit when digit land 0x0f = 0 -> Ok ()
        | Some _ -> invalid "has non-zero unused bits in its final quantum"
        | None -> assert false)
    | _ -> assert false

let valid_token value =
  value <> ""
  && String.length value <= 128
  && String.for_all
       (function
         | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' -> true | _ -> false)
       value

let validate_token ~field value =
  if valid_token value then Ok ()
  else
    Error
      (`Msg
         (Printf.sprintf
            "%s must be a 1-to-128-byte RFC 5545 token containing only \
             letters, digits, and hyphens"
            field))

let validate_content_line_value ~field value =
  let length = String.length value in
  if length > max_content_line_value_bytes then
    Error
      (`Msg
         (Printf.sprintf "%s exceeds the %d-byte limit" field
            max_content_line_value_bytes))
  else
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String value) in
    let rec decode () =
      match Uutf.decode decoder with
      | `Uchar scalar ->
          let scalar = Uchar.to_int scalar in
          if
            (scalar < 0x20 && scalar <> 0x09)
            || (scalar >= 0x7f && scalar <= 0x9f)
          then
            Error
              (`Msg
                 (Printf.sprintf "%s contains a forbidden control character"
                    field))
          else decode ()
      | `End -> Ok ()
      | `Malformed _ ->
          Error (`Msg (Printf.sprintf "%s must be valid UTF-8" field))
      | `Await -> assert false
    in
    decode ()

let uri_character = function
  | 'A' .. 'Z'
  | 'a' .. 'z'
  | '0' .. '9'
  | '-' | '.' | '_' | '~' | ':' | '/' | '?' | '#' | '[' | ']' | '@' | '!' | '$'
  | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' ->
      true
  | _ -> false

let hex_digit = function
  | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
  | _ -> false

let validate_uri ~field value =
  let length = String.length value in
  let invalid reason = Error (`Msg (Printf.sprintf "%s %s" field reason)) in
  if length = 0 then invalid "must not be empty"
  else if length > max_uri_bytes then
    invalid (Printf.sprintf "exceeds the %d-byte limit" max_uri_bytes)
  else
    let rec validate_characters index =
      if index = length then Ok ()
      else if value.[index] = '%' then
        if
          index + 2 < length
          && hex_digit value.[index + 1]
          && hex_digit value.[index + 2]
        then validate_characters (index + 3)
        else invalid "contains an invalid percent escape"
      else if uri_character value.[index] then validate_characters (index + 1)
      else invalid "contains a character forbidden in an RFC 3986 URI"
    in
    let* () = validate_characters 0 in
    match String.index_opt value ':' with
    | None -> invalid "must be absolute and include a URI scheme"
    | Some 0 -> invalid "has an empty URI scheme"
    | Some separator ->
        let scheme = String.sub value 0 separator in
        let valid_scheme =
          match scheme.[0] with
          | 'A' .. 'Z' | 'a' .. 'z' ->
              String.for_all
                (function
                  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '+' | '-' | '.' ->
                      true
                  | _ -> false)
                scheme
          | _ -> false
        in
        if valid_scheme then Ok () else invalid "has an invalid URI scheme"

let validate_text_value ~field value =
  if String.length value > max_content_line_value_bytes then
    Error
      (`Msg
         (Printf.sprintf "%s exceeds the %d-byte limit" field
            max_content_line_value_bytes))
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
            Error
              (`Msg
                 (Printf.sprintf "%s contains a forbidden control character"
                    field))
          else decode ()
      | `End -> Ok ()
      | `Malformed _ ->
          Error (`Msg (Printf.sprintf "%s must be valid UTF-8" field))
      | `Await -> assert false
    in
    decode ()

let validate_parameter_value ~field = function
  | `String value ->
      let* () = validate_content_line_value ~field value in
      if
        value = ""
        || String.exists
             (function '"' | ';' | ':' | ',' -> true | _ -> false)
             value
      then Error (`Msg (field ^ " is not a safe unquoted parameter value"))
      else Ok ()
  | `Quoted value ->
      let* () = validate_content_line_value ~field value in
      if value = "" || String.contains value '"' then
        Error (`Msg (field ^ " is not a safe quoted parameter value"))
      else Ok ()

let validate_parameter_values ~field values =
  let rec loop = function
    | [] -> Error (`Msg (field ^ " must contain at least one value"))
    | [ value ] -> validate_parameter_value ~field value
    | value :: rest ->
        let* () = validate_parameter_value ~field value in
        loop rest
  in
  loop values

let registered_parameter_names =
  [
    "ALTREP";
    "CN";
    "CUTYPE";
    "DELEGATED-FROM";
    "DELEGATED-TO";
    "DIR";
    "ENCODING";
    "FBTYPE";
    "FMTTYPE";
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

let validate_x_name ~field namespace name =
  let* () = if namespace = "" then Ok () else validate_token ~field namespace in
  let* () = validate_token ~field name in
  let serialized_length =
    2 + String.length name
    + if namespace = "" then 0 else String.length namespace + 1
  in
  if serialized_length <= 128 then Ok ()
  else Error (`Msg (field ^ " exceeds the 128-byte serialized-name limit"))

let validate_media_token ~field value =
  if
    value <> ""
    && String.for_all
         (function
           | 'A' .. 'Z'
           | 'a' .. 'z'
           | '0' .. '9'
           | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^'
           | '_' | '`' | '|' | '~' ->
               true
           | _ -> false)
         value
  then Ok ()
  else Error (`Msg (field ^ " is not a valid media-type token"))

let validate_params params =
  let validate_uri_value field uri = validate_uri ~field (Uri.to_string uri) in
  let validate_uris field uris =
    let rec loop = function
      | [] -> Error (`Msg (field ^ " must contain at least one URI"))
      | [ uri ] -> validate_uri_value field uri
      | uri :: rest ->
          let* () = validate_uri_value field uri in
          loop rest
    in
    loop uris
  in
  let validate_binding (Params.B (key, value)) =
    match key with
    | Altrep -> validate_uri_value "VALARM ALTREP parameter" value
    | Delegated_from -> validate_uris "VALARM DELEGATED-FROM parameter" value
    | Delegated_to -> validate_uris "VALARM DELEGATED-TO parameter" value
    | Dir -> validate_uri_value "VALARM DIR parameter" value
    | Member -> validate_uris "VALARM MEMBER parameter" value
    | Sentby -> validate_uri_value "VALARM SENT-BY parameter" value
    | Cn -> validate_parameter_value ~field:"VALARM CN parameter" value
    | Language ->
        validate_parameter_value ~field:"VALARM LANGUAGE parameter"
          (`String value)
    | Tzid ->
        validate_parameter_value ~field:"VALARM TZID parameter"
          (`String (snd value))
    | Media_type ->
        let* () = validate_media_token ~field:"VALARM media type" (fst value) in
        validate_media_token ~field:"VALARM media subtype" (snd value)
    | Iana_param name ->
        let* () = validate_token ~field:"VALARM IANA parameter name" name in
        let canonical = String.uppercase_ascii name in
        if List.mem canonical registered_parameter_names then
          Error
            (`Msg
               (Printf.sprintf
                  "VALARM IANA parameter %s shadows a registered parameter" name))
        else validate_parameter_values ~field:("VALARM parameter " ^ name) value
    | Xparam (namespace, name) ->
        let* () =
          validate_x_name ~field:"VALARM X parameter name" namespace name
        in
        validate_parameter_values
          ~field:("VALARM parameter X-" ^ namespace ^ "-" ^ name)
          value
    | Cutype | Encoding | Fbtype | Partstat | Range | Related | Reltype | Role
    | Rsvp | Valuetype ->
        Ok ()
  in
  let rec loop = function
    | [] -> Ok ()
    | binding :: rest ->
        let* () = validate_binding binding in
        loop rest
  in
  loop (Params.bindings params)

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

let validate_allowed_params ~field ~allowed params =
  let rec loop = function
    | [] -> Ok ()
    | Params.B (key, _) :: rest -> (
        match standard_parameter_name key with
        | None -> loop rest
        | Some name when List.mem name allowed -> loop rest
        | Some name ->
            Error
              (`Msg
                 (Printf.sprintf "%s does not permit the %s parameter" field
                    name)))
  in
  loop (Params.bindings params)

let reserved_alarm_property_names =
  [
    "ACTION";
    "ATTACH";
    "ATTENDEE";
    "BEGIN";
    "DESCRIPTION";
    "DURATION";
    "END";
    "REPEAT";
    "SUMMARY";
    "TRIGGER";
  ]

let validate_other_property = function
  | `Iana_prop (name, params, value) ->
      let* () = validate_token ~field:"VALARM IANA property name" name in
      let canonical = String.uppercase_ascii name in
      let* () =
        if String.starts_with ~prefix:"X-" canonical then
          Error
            (`Msg "VALARM IANA properties beginning X- must be X properties")
        else if
          String.starts_with ~prefix:"BEGIN" canonical
          || String.starts_with ~prefix:"END" canonical
        then
          Error (`Msg "VALARM IANA property cannot use BEGIN or END prefixes")
        else if List.mem canonical reserved_alarm_property_names then
          Error
            (`Msg
               (Printf.sprintf
                  "VALARM IANA property %s shadows a standard property" name))
        else Ok ()
      in
      let* () = validate_params params in
      validate_content_line_value ~field:"VALARM IANA property value" value
  | `Xprop ((namespace, name), params, value) ->
      let* () =
        validate_x_name ~field:"VALARM X property name" namespace name
      in
      let* () = validate_params params in
      validate_content_line_value ~field:"VALARM X property value" value

let validate_other_properties properties =
  let rec loop = function
    | [] -> Ok ()
    | property :: rest ->
        let* () = validate_other_property property in
        loop rest
  in
  loop properties

let validate_attachment (params, attachment) =
  let* () = validate_params params in
  let* () =
    validate_allowed_params ~field:"VALARM ATTACH"
      ~allowed:[ "FMTTYPE"; "ENCODING"; "VALUE" ]
      params
  in
  match attachment with
  | `Uri uri ->
      let* () =
        validate_uri ~field:"VALARM URI attachment" (Uri.to_string uri)
      in
      let* () =
        match Params.find Valuetype params with
        | None | Some `Uri -> Ok ()
        | Some _ -> Error (`Msg "VALARM URI attachment requires VALUE=URI")
      in
      if Params.mem Encoding params then
        Error (`Msg "VALARM URI attachment cannot use ENCODING")
      else Ok ()
  | `Binary value ->
      let* () = validate_binary_attachment value in
      if Params.find Valuetype params <> Some `Binary then
        Error (`Msg "VALARM binary attachment requires VALUE=BINARY")
      else if Params.find Encoding params <> Some `Base64 then
        Error (`Msg "VALARM binary attachment requires ENCODING=BASE64")
      else Ok ()

let validate_attendees attendees =
  let rec loop = function
    | [] -> Ok ()
    | (params, attendee) :: rest ->
        let* () = validate_params params in
        let* () =
          validate_allowed_params ~field:"VALARM ATTENDEE"
            ~allowed:
              [
                "CN";
                "CUTYPE";
                "DELEGATED-FROM";
                "DELEGATED-TO";
                "DIR";
                "LANGUAGE";
                "MEMBER";
                "PARTSTAT";
                "ROLE";
                "RSVP";
                "SENT-BY";
              ]
            params
        in
        let* () =
          validate_uri ~field:"VALARM attendee URI" (Uri.to_string attendee)
        in
        loop rest
  in
  loop attendees

let validate_whole_second_span ~field span =
  match Ptime.Span.to_int_s span with
  | None -> Error (`Msg (field ^ " exceeds the supported whole-second range"))
  | Some seconds ->
      if Ptime.Span.equal span (Ptime.Span.of_int_s seconds) then Ok ()
      else Error (`Msg (field ^ " must use whole seconds"))

let validate_duration_repeat = function
  | None -> Ok ()
  | Some ((duration_params, duration), (repeat_params, repeat)) ->
      let* () = validate_params duration_params in
      let* () = validate_params repeat_params in
      let* () =
        validate_allowed_params ~field:"VALARM DURATION" ~allowed:[]
          duration_params
      in
      let* () =
        validate_allowed_params ~field:"VALARM REPEAT" ~allowed:[] repeat_params
      in
      let* () = validate_whole_second_span ~field:"VALARM DURATION" duration in
      if Ptime.Span.compare duration Ptime.Span.zero <= 0 then
        Error (`Msg "VALARM DURATION must be greater than zero")
      else if repeat <= 0 then
        Error (`Msg "VALARM REPEAT must be greater than zero")
      else Ok ()

let validate_trigger (params, trigger) =
  let* () = validate_params params in
  let* () =
    validate_allowed_params ~field:"VALARM TRIGGER"
      ~allowed:[ "RELATED"; "VALUE" ] params
  in
  let* () =
    match trigger with
    | `Duration span ->
        validate_whole_second_span ~field:"relative VALARM TRIGGER" span
    | `Datetime _ -> Ok ()
  in
  match (Params.find Related params, Params.find Valuetype params, trigger) with
  | Some _, _, `Datetime _ ->
      Error (`Msg "VALARM RELATED is only valid on relative TRIGGER values")
  | _, Some `Datetime, `Datetime timestamp
    when Ptime.Span.compare (Ptime.frac_s timestamp) Ptime.Span.zero = 0 ->
      Ok ()
  | _, Some `Datetime, `Datetime _ ->
      Error (`Msg "absolute VALARM TRIGGER must use whole seconds")
  | _, (None | Some _), `Datetime _ ->
      Error (`Msg "absolute VALARM TRIGGER requires explicit VALUE=DATE-TIME")
  | _, (None | Some `Duration), `Duration _ -> Ok ()
  | _, Some _, `Duration _ ->
      Error (`Msg "relative VALARM TRIGGER requires VALUE=DURATION")

let validate alarm =
  let validate_common trigger duration_repeat summary other =
    let* () = validate_trigger trigger in
    let* () = validate_duration_repeat duration_repeat in
    let* () =
      match summary with
      | None -> Ok ()
      | Some (params, summary) ->
          let* () = validate_params params in
          let* () =
            validate_allowed_params ~field:"VALARM SUMMARY"
              ~allowed:[ "ALTREP"; "LANGUAGE" ] params
          in
          validate_text_value ~field:"VALARM SUMMARY" summary
    in
    validate_other_properties other
  in
  match alarm with
  | `Display (alarm : display_struct alarm_struct) -> (
      let* () =
        validate_common alarm.trigger alarm.duration_repeat alarm.summary
          alarm.other
      in
      let* () =
        match alarm.summary with
        | None -> Ok ()
        | Some _ -> Error (`Msg "DISPLAY VALARM cannot contain SUMMARY")
      in
      match alarm.special.description with
      | Some (params, description) when nonempty description ->
          let* () = validate_params params in
          let* () =
            validate_allowed_params ~field:"DISPLAY VALARM DESCRIPTION"
              ~allowed:[ "ALTREP"; "LANGUAGE" ] params
          in
          validate_text_value ~field:"DISPLAY VALARM DESCRIPTION" description
      | _ -> Error (`Msg "DISPLAY VALARM requires a non-empty DESCRIPTION"))
  | `Email (alarm : email_struct alarm_struct) -> (
      let* () =
        validate_common alarm.trigger alarm.duration_repeat alarm.summary
          alarm.other
      in
      let* () =
        match alarm.summary with
        | Some (_, summary) when nonempty summary -> Ok ()
        | _ -> Error (`Msg "EMAIL VALARM requires a non-empty SUMMARY")
      in
      let* () =
        if nonempty (snd alarm.special.description) then
          let* () = validate_params (fst alarm.special.description) in
          let* () =
            validate_allowed_params ~field:"EMAIL VALARM DESCRIPTION"
              ~allowed:[ "ALTREP"; "LANGUAGE" ]
              (fst alarm.special.description)
          in
          validate_text_value ~field:"EMAIL VALARM DESCRIPTION"
            (snd alarm.special.description)
        else Error (`Msg "EMAIL VALARM requires a non-empty DESCRIPTION")
      in
      let* () =
        if alarm.special.attendees = [] then
          Error (`Msg "EMAIL VALARM requires at least one ATTENDEE")
        else validate_attendees alarm.special.attendees
      in
      match alarm.special.attach with
      | None -> Ok ()
      | Some attachment -> validate_attachment attachment)
  | `Audio (alarm : audio_struct alarm_struct) -> (
      let* () =
        validate_common alarm.trigger alarm.duration_repeat alarm.summary
          alarm.other
      in
      let* () =
        match alarm.summary with
        | None -> Ok ()
        | Some _ -> Error (`Msg "AUDIO VALARM cannot contain SUMMARY")
      in
      match alarm.special.attach with
      | None -> Ok ()
      | Some attachment -> validate_attachment attachment)
  | `None (alarm : unit alarm_struct) ->
      let* () =
        validate_common alarm.trigger alarm.duration_repeat alarm.summary
          alarm.other
      in
      if Option.is_some alarm.summary then
        Error (`Msg "NONE VALARM cannot contain SUMMARY")
      else Ok ()

let validate_all alarms =
  let rec loop = function
    | [] -> Ok ()
    | alarm :: rest ->
        let* () = validate alarm in
        loop rest
  in
  loop alarms

let trigger = function
  | `Audio alarm -> alarm.trigger
  | `Display alarm -> alarm.trigger
  | `Email alarm -> alarm.trigger
  | `None alarm -> alarm.trigger

let duration_repeat = function
  | `Audio alarm -> alarm.duration_repeat
  | `Display alarm -> alarm.duration_repeat
  | `Email alarm -> alarm.duration_repeat
  | `None alarm -> alarm.duration_repeat

let validate_references ~has_start ~has_end alarms =
  let validate_reference alarm =
    match trigger alarm with
    | _, `Datetime _ -> Ok ()
    | params, `Duration _ -> (
        match Params.find Related params with
        | Some `End when not has_end ->
            Error (`Msg "END-relative VALARM requires an end/due value")
        | (Some `Start | None) when not has_start ->
            Error (`Msg "START-relative VALARM requires a start value")
        | Some (`Start | `End) | None -> Ok ())
  in
  let rec loop = function
    | [] -> Ok ()
    | alarm :: rest ->
        let* () = validate_reference alarm in
        loop rest
  in
  loop alarms

let repeated_spans ?(max_repetitions = 100_000) initial alarm =
  match duration_repeat alarm with
  | None -> Ok [ initial ]
  | Some ((_, duration), (_, repeat)) ->
      if repeat > max_repetitions then
        Error
          (`Msg
             (Printf.sprintf
                "VALARM REPEAT exceeds the %d-repetition safety limit"
                max_repetitions))
      else
        let rec loop remaining current accumulated =
          if remaining = 0 then Ok (List.rev accumulated)
          else
            let current = Ptime.Span.add current duration in
            loop (remaining - 1) current (current :: accumulated)
        in
        loop repeat initial [ initial ]

let repeated_instants ?(max_repetitions = 100_000) initial alarm =
  match duration_repeat alarm with
  | None -> Ok [ initial ]
  | Some ((_, duration), (_, repeat)) ->
      if repeat > max_repetitions then
        Error
          (`Msg
             (Printf.sprintf
                "VALARM REPEAT exceeds the %d-repetition safety limit"
                max_repetitions))
      else
        let rec loop remaining current accumulated =
          if remaining = 0 then Ok (List.rev accumulated)
          else
            match Ptime.add_span current duration with
            | None -> Error (`Msg "repeated VALARM fire time is out of range")
            | Some current ->
                loop (remaining - 1) current (current :: accumulated)
        in
        loop repeat initial [ initial ]
