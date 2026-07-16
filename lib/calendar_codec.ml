let marker_vendor = "CALEDONIA"
let marker_name = "INTERNAL-ALARM-MARKER"
let placeholder_name = "INTERNAL-OPAQUE-PLACEHOLDER"
let raw_component_vendor = "CALEDONIA"
let raw_component_name = "RAW-COMPONENT"
let raw_component_marker = "X-CALEDONIA-RAW-COMPONENT:"
let date_until_vendor = "CALEDONIA"
let date_until_name = "DATE-UNTIL"
let ( let* ) = Result.bind
let fresh_uuid = Fresh_id.generate
let opaque_marker_secret = fresh_uuid ()

let encode_date_until date =
  let signature =
    Digest.string (opaque_marker_secret ^ "\000DATE-UNTIL\000" ^ date)
    |> Digest.to_hex
  in
  date ^ "-" ^ signature

let decode_date_until encoded =
  if String.length encoded <> 8 + 1 + 32 || encoded.[8] <> '-' then None
  else
    let date = String.sub encoded 0 8 in
    let signature = String.sub encoded 9 32 in
    let expected =
      Digest.string (opaque_marker_secret ^ "\000DATE-UNTIL\000" ^ date)
      |> Digest.to_hex
    in
    if
      String.equal signature expected
      && String.for_all (function '0' .. '9' -> true | _ -> false) date
    then Some date
    else None

let date_until_of_params params =
  let encoded =
    match
      Icalendar.Params.find
        (Icalendar.Iana_param ("X-" ^ date_until_vendor ^ "-" ^ date_until_name))
        params
    with
    | Some _ as value -> value
    | None ->
        Icalendar.Params.find
          (Icalendar.Xparam (date_until_vendor, date_until_name))
          params
  in
  match encoded with
  | Some [ (`String encoded | `Quoted encoded) ] -> decode_date_until encoded
  | Some _ | None -> None

let hex_of_string value =
  let result = Bytes.create (String.length value * 2) in
  let digits = "0123456789abcdef" in
  String.iteri
    (fun index character ->
      let code = Char.code character in
      Bytes.set result (index * 2) digits.[code lsr 4];
      Bytes.set result ((index * 2) + 1) digits.[code land 0x0f])
    value;
  Bytes.unsafe_to_string result

let string_of_hex value =
  let digit = function
    | '0' .. '9' as value -> Some (Char.code value - Char.code '0')
    | 'a' .. 'f' as value -> Some (10 + Char.code value - Char.code 'a')
    | 'A' .. 'F' as value -> Some (10 + Char.code value - Char.code 'A')
    | _ -> None
  in
  if String.length value mod 2 <> 0 then None
  else
    let result = Bytes.create (String.length value / 2) in
    let rec decode index =
      if index = String.length value then Some (Bytes.unsafe_to_string result)
      else
        match (digit value.[index], digit value.[index + 1]) with
        | Some high, Some low ->
            Bytes.set result (index / 2) (Char.chr ((high lsl 4) lor low));
            decode (index + 2)
        | _ -> None
    in
    decode 0

let encode_opaque_component raw =
  let signature =
    Digest.string (opaque_marker_secret ^ "\000" ^ raw) |> Digest.to_hex
  in
  signature ^ ":" ^ hex_of_string raw

let decode_opaque_component encoded =
  match String.index_opt encoded ':' with
  | None -> None
  | Some separator -> (
      let signature = String.sub encoded 0 separator in
      let payload =
        String.sub encoded (separator + 1)
          (String.length encoded - separator - 1)
      in
      match string_of_hex payload with
      | None -> None
      | Some raw ->
          let expected =
            Digest.string (opaque_marker_secret ^ "\000" ^ raw) |> Digest.to_hex
          in
          if String.equal signature expected then Some raw else None)

let logical_line line =
  if String.ends_with ~suffix:"\r" line then
    String.sub line 0 (String.length line - 1)
  else line

let begin_tag line =
  let line = logical_line line |> String.uppercase_ascii in
  if String.starts_with ~prefix:"BEGIN:" line then
    Some (String.sub line 6 (String.length line - 6))
  else None

let end_tag line =
  let line = logical_line line |> String.uppercase_ascii in
  if String.starts_with ~prefix:"END:" line then
    Some (String.sub line 4 (String.length line - 4))
  else None

let known_component = function
  | "VCALENDAR" | "VEVENT" | "VTODO" | "VJOURNAL" | "VFREEBUSY" | "VTIMEZONE"
  | "VALARM" | "STANDARD" | "DAYLIGHT" ->
      true
  | _ -> false

let xprop_matches ~vendor:expected_vendor ~name:expected_name = function
  | `Xprop ((vendor, name), _, value) ->
      let vendor = String.uppercase_ascii vendor in
      let name = String.uppercase_ascii name in
      if
        (String.equal vendor expected_vendor && String.equal name expected_name)
        || String.equal vendor ""
           && String.equal name (expected_vendor ^ "-" ^ expected_name)
      then Some value
      else None
  | _ -> None

let protect_unknown_components content =
  let rec take_block stack accumulated = function
    | [] -> None
    | line :: rest -> (
        let next_stack =
          match (begin_tag line, end_tag line, stack) with
          | Some tag, _, _ -> Some (tag :: stack)
          | None, Some tag, current :: tail when String.equal tag current ->
              Some tail
          | None, Some _, _ -> None
          | None, None, _ -> Some stack
        in
        match next_stack with
        | None -> None
        | Some next_stack ->
            let accumulated = line :: accumulated in
            if next_stack = [] then Some (List.rev accumulated, rest)
            else take_block next_stack accumulated rest)
  in
  let top_level_markers = ref [] in
  let rec protect stack accumulated = function
    | [] -> List.rev accumulated
    | line :: rest -> (
        match begin_tag line with
        | Some tag when not (known_component tag) -> (
            match take_block [] [] (line :: rest) with
            | None -> List.rev_append accumulated (line :: rest)
            | Some (block, remaining) ->
                let marker =
                  raw_component_marker
                  ^ encode_opaque_component (String.concat "\n" block)
                in
                if stack = [ "VCALENDAR" ] then (
                  top_level_markers := marker :: !top_level_markers;
                  protect stack accumulated remaining)
                else protect stack (marker :: accumulated) remaining)
        | Some tag -> protect (tag :: stack) (line :: accumulated) rest
        | None ->
            let stack =
              match (end_tag line, stack) with
              | Some tag, current :: tail when String.equal tag current -> tail
              | _ -> stack
            in
            protect stack (line :: accumulated) rest)
  in
  let protected = content |> String.split_on_char '\n' |> protect [] [] in
  let markers = List.rev !top_level_markers in
  let rec insert_top_level_markers = function
    | [] -> []
    | line :: rest when begin_tag line = Some "VCALENDAR" ->
        (line :: markers) @ rest
    | line :: rest -> line :: insert_top_level_markers rest
  in
  protected |> insert_top_level_markers |> String.concat "\n"

let add_opaque_placeholder_if_needed content token =
  let rec has_top_level_component stack = function
    | [] -> false
    | line :: rest -> (
        match begin_tag line with
        | Some tag when stack = [ "VCALENDAR" ] && known_component tag -> true
        | Some tag -> has_top_level_component (tag :: stack) rest
        | None ->
            let stack =
              match (end_tag line, stack) with
              | Some tag, current :: tail when String.equal tag current -> tail
              | _ -> stack
            in
            has_top_level_component stack rest)
  in
  let lines = String.split_on_char '\n' content in
  if has_top_level_component [] lines then content
  else
    let placeholder =
      [
        "BEGIN:VJOURNAL";
        "UID:caledonia-internal-opaque-placeholder-" ^ token;
        "DTSTAMP:19700101T000000Z";
        Printf.sprintf "X-%s-%s:%s" marker_vendor placeholder_name token;
        "END:VJOURNAL";
      ]
    in
    let rec insert accumulated = function
      | [] -> List.rev_append accumulated placeholder
      | line :: rest when end_tag line = Some "VCALENDAR" ->
          List.rev_append accumulated (placeholder @ (line :: rest))
      | line :: rest -> insert (line :: accumulated) rest
    in
    insert [] lines |> String.concat "\n"

let has_marker_value token value =
  let prefix = token ^ ":" in
  String.starts_with ~prefix value
  && String.length value > String.length prefix
  && String.sub value (String.length prefix)
       (String.length value - String.length prefix)
     |> String.for_all (function '0' .. '9' -> true | _ -> false)

let mark_alarm_blocks content token =
  let ordinal = ref 0 in
  content |> String.split_on_char '\n'
  |> List.concat_map (fun line ->
      let logical = logical_line line in
      if String.equal (String.uppercase_ascii logical) "BEGIN:VALARM" then (
        let marker =
          Printf.sprintf "X-%s-%s:%s:%d" marker_vendor marker_name token
            !ordinal
        in
        incr ordinal;
        [ line; marker ])
      else [ line ])
  |> String.concat "\n"

let is_marker token = function
  | property -> (
      match xprop_matches ~vendor:marker_vendor ~name:marker_name property with
      | Some value -> has_marker_value token value
      | None -> false)

let is_opaque_placeholder token = function
  | `Journal properties ->
      List.exists
        (fun property ->
          match
            xprop_matches ~vendor:marker_vendor ~name:placeholder_name property
          with
          | Some value -> String.equal value token
          | None -> false)
        properties
  | `Event _ | `Todo _ | `Freebusy _ | `Timezone _ -> false

let strip_alarm_marker token = function
  | `Audio (alarm : Icalendar.audio_struct Icalendar.alarm_struct) ->
      `Audio
        {
          alarm with
          other =
            List.rev alarm.other |> List.filter (Fun.negate (is_marker token));
        }
  | `Display (alarm : Icalendar.display_struct Icalendar.alarm_struct) ->
      `Display
        {
          alarm with
          other =
            List.rev alarm.other |> List.filter (Fun.negate (is_marker token));
        }
  | `Email (alarm : Icalendar.email_struct Icalendar.alarm_struct) ->
      `Email
        {
          alarm with
          other =
            List.rev alarm.other |> List.filter (Fun.negate (is_marker token));
          special =
            { alarm.special with attendees = List.rev alarm.special.attendees };
        }
  | `None (alarm : unit Icalendar.alarm_struct) ->
      `None
        {
          alarm with
          other =
            List.rev alarm.other |> List.filter (Fun.negate (is_marker token));
        }

let strip_component_markers token = function
  | `Event (event : Icalendar.event) ->
      `Event
        {
          event with
          alarms = List.rev_map (strip_alarm_marker token) event.alarms;
        }
  | `Todo (properties, alarms) ->
      `Todo (properties, List.rev_map (strip_alarm_marker token) alarms)
  | (`Journal _ | `Freebusy _ | `Timezone _) as component -> component

(* RFC 5545 names and registered tokens are ASCII case-insensitive, but the
   pinned parser compares many of them byte-for-byte.  Normalize the parser's
   private input without case-folding property values such as SUMMARY, CN,
   URIs, or TZIDs. *)
let split_unquoted separator value =
  let length = String.length value in
  let rec loop start index quoted accumulated =
    if index = length then
      List.rev (String.sub value start (length - start) :: accumulated)
    else
      match value.[index] with
      | '\\' when quoted && index + 1 < length ->
          loop start (index + 2) quoted accumulated
      | '"' -> loop start (index + 1) (not quoted) accumulated
      | character when (not quoted) && Char.equal character separator ->
          loop (index + 1) (index + 1) quoted
            (String.sub value start (index - start) :: accumulated)
      | _ -> loop start (index + 1) quoted accumulated
  in
  loop 0 0 false []

let split_first_unquoted separator value =
  match split_unquoted separator value with
  | [] -> None
  | [ _ ] -> None
  | first :: rest -> Some (first, String.concat (String.make 1 separator) rest)

let uppercase_if_recognized recognized value =
  let upper = String.uppercase_ascii value in
  if List.mem upper recognized then upper else value

let map_quoted value fn =
  let length = String.length value in
  if length >= 2 && value.[0] = '"' && value.[length - 1] = '"' then
    let inner = String.sub value 1 (length - 2) in
    "\"" ^ fn inner ^ "\""
  else fn value

let normalize_enum_list recognized value =
  map_quoted value (fun value ->
      split_unquoted ',' value
      |> List.map (uppercase_if_recognized recognized)
      |> String.concat ",")

let value_tokens =
  [
    "BINARY";
    "BOOLEAN";
    "CAL-ADDRESS";
    "DATE";
    "DATE-TIME";
    "DURATION";
    "FLOAT";
    "INTEGER";
    "PERIOD";
    "RECUR";
    "TEXT";
    "TIME";
    "URI";
    "UTC-OFFSET";
  ]

let enum_parameter_tokens = function
  | "VALUE" -> Some value_tokens
  | "ENCODING" -> Some [ "8BIT"; "BASE64" ]
  | "CUTYPE" -> Some [ "INDIVIDUAL"; "GROUP"; "RESOURCE"; "ROOM"; "UNKNOWN" ]
  | "FBTYPE" -> Some [ "FREE"; "BUSY"; "BUSY-UNAVAILABLE"; "BUSY-TENTATIVE" ]
  | "PARTSTAT" ->
      Some
        [
          "NEEDS-ACTION";
          "ACCEPTED";
          "DECLINED";
          "TENTATIVE";
          "DELEGATED";
          "COMPLETED";
          "IN-PROCESS";
        ]
  | "RANGE" -> Some [ "THISANDFUTURE" ]
  | "RELATED" -> Some [ "START"; "END" ]
  | "RELTYPE" -> Some [ "PARENT"; "CHILD"; "SIBLING" ]
  | "ROLE" ->
      Some [ "CHAIR"; "REQ-PARTICIPANT"; "OPT-PARTICIPANT"; "NON-PARTICIPANT" ]
  | "RSVP" -> Some [ "TRUE"; "FALSE" ]
  | _ -> None

let normalize_parameter parameter =
  match split_first_unquoted '=' parameter with
  | None -> String.uppercase_ascii parameter
  | Some (name, value) ->
      let name = String.uppercase_ascii name in
      let value =
        match enum_parameter_tokens name with
        | Some recognized -> normalize_enum_list recognized value
        | None -> value
      in
      name ^ "=" ^ value

let weekdays = [ "MO"; "TU"; "WE"; "TH"; "FR"; "SA"; "SU" ]

let normalize_byday value =
  let normalize token =
    let upper = String.uppercase_ascii token in
    let length = String.length upper in
    if length < 2 then token
    else
      let suffix = String.sub upper (length - 2) 2 in
      let prefix = String.sub upper 0 (length - 2) in
      let valid_prefix =
        prefix = ""
        || String.for_all
             (function '0' .. '9' | '+' | '-' -> true | _ -> false)
             prefix
      in
      if valid_prefix && List.mem suffix weekdays then upper else token
  in
  split_unquoted ',' value |> List.map normalize |> String.concat ","

let normalize_rrule value =
  let normalize_part part =
    match split_first_unquoted '=' part with
    | None -> part
    | Some (name, value) ->
        let name = String.uppercase_ascii name in
        let value =
          match name with
          | "FREQ" ->
              uppercase_if_recognized
                [
                  "SECONDLY";
                  "MINUTELY";
                  "HOURLY";
                  "DAILY";
                  "WEEKLY";
                  "MONTHLY";
                  "YEARLY";
                ]
                value
          | "WKST" -> uppercase_if_recognized weekdays value
          | "BYDAY" -> normalize_byday value
          | _ -> value
        in
        name ^ "=" ^ value
  in
  split_unquoted ';' value |> List.map normalize_part |> String.concat ";"

let enum_property_tokens = function
  | "BEGIN" | "END" ->
      Some
        [
          "VCALENDAR";
          "VEVENT";
          "VTODO";
          "VJOURNAL";
          "VFREEBUSY";
          "VTIMEZONE";
          "VALARM";
          "STANDARD";
          "DAYLIGHT";
        ]
  | "ACTION" -> Some [ "AUDIO"; "DISPLAY"; "EMAIL"; "NONE" ]
  | "CALSCALE" -> Some [ "GREGORIAN" ]
  | "CLASS" -> Some [ "PUBLIC"; "PRIVATE"; "CONFIDENTIAL" ]
  | "METHOD" ->
      Some
        [
          "PUBLISH";
          "REQUEST";
          "REPLY";
          "ADD";
          "CANCEL";
          "REFRESH";
          "COUNTER";
          "DECLINECOUNTER";
        ]
  | "STATUS" ->
      Some
        [
          "TENTATIVE";
          "CONFIRMED";
          "CANCELLED";
          "NEEDS-ACTION";
          "COMPLETED";
          "IN-PROCESS";
          "DRAFT";
          "FINAL";
        ]
  | "TRANSP" -> Some [ "OPAQUE"; "TRANSPARENT" ]
  | _ -> None

let property_base_name name =
  match String.rindex_opt name '.' with
  | Some index -> String.sub name (index + 1) (String.length name - index - 1)
  | None -> name

let normalize_content_line line =
  match split_first_unquoted ':' line with
  | None -> line
  | Some (left, value) -> (
      match split_unquoted ';' left with
      | [] -> line
      | property :: parameters ->
          let property = String.uppercase_ascii property in
          let base = property_base_name property in
          let parameters = List.map normalize_parameter parameters in
          let value =
            if String.equal base "RRULE" then normalize_rrule value
            else
              match enum_property_tokens base with
              | Some recognized -> uppercase_if_recognized recognized value
              | None -> value
          in
          String.concat ";" (property :: parameters) ^ ":" ^ value)

let normalize_rfc_syntax content =
  let continuation line =
    let line = logical_line line in
    String.length line > 0 && (line.[0] = ' ' || line.[0] = '\t')
  in
  let rec unfold accumulated = function
    | line :: rest when continuation line -> (
        let continuation = logical_line line in
        let suffix =
          String.sub continuation 1 (String.length continuation - 1)
        in
        match accumulated with
        | previous :: tail -> unfold ((previous ^ suffix) :: tail) rest
        | [] -> unfold (continuation :: accumulated) rest)
    | line :: rest -> unfold (logical_line line :: accumulated) rest
    | [] -> List.rev accumulated
  in
  content |> String.split_on_char '\n' |> unfold []
  |> List.map normalize_content_line
  |> String.concat "\n"

let rrule_date_until value =
  value |> String.split_on_char ';'
  |> List.find_map (fun part ->
      let upper = String.uppercase_ascii part in
      if String.starts_with ~prefix:"UNTIL=" upper then
        let candidate = String.sub part 6 (String.length part - 6) in
        if
          String.length candidate = 8
          && String.for_all
               (function '0' .. '9' -> true | _ -> false)
               candidate
        then Some candidate
        else None
      else None)

let mark_date_until content =
  let internal_prefix = "X-CALEDONIA-DATE-UNTIL=" in
  let rec loop stack accumulated = function
    | [] -> List.rev accumulated
    | line :: rest -> (
        match (begin_tag line, end_tag line) with
        | Some tag, _ -> loop (tag :: stack) (line :: accumulated) rest
        | None, Some tag ->
            let stack =
              match stack with
              | current :: tail when String.equal current tag -> tail
              | _ -> stack
            in
            loop stack (line :: accumulated) rest
        | None, None ->
            let line =
              match (stack, split_first_unquoted ':' line) with
              | ("VEVENT" | "VTODO" | "VJOURNAL") :: _, Some (left, value)
                when String.equal
                       (split_unquoted ';' left |> List.hd
                      |> String.uppercase_ascii)
                       "RRULE" -> (
                  match rrule_date_until value with
                  | None -> line
                  | Some date ->
                      let clean_left =
                        split_unquoted ';' left
                        |> List.filter (fun parameter ->
                            not
                              (String.starts_with ~prefix:internal_prefix
                                 (String.uppercase_ascii parameter)))
                        |> String.concat ";"
                      in
                      clean_left ^ ";" ^ internal_prefix
                      ^ encode_date_until date ^ ":" ^ value)
              | _ -> line
            in
            loop stack (line :: accumulated) rest)
  in
  content |> String.split_on_char '\n' |> loop [] [] |> String.concat "\n"

let validate_registered_numbers content =
  let validate_range component property minimum maximum value =
    match int_of_string_opt value with
    | Some number when number >= minimum && number <= maximum -> Ok ()
    | _ ->
        Error
          (Printf.sprintf "%s %s must be an integer between %d and %d" component
             property minimum maximum)
  in
  let rec validate stack = function
    | [] -> Ok ()
    | line :: rest -> (
        match (begin_tag line, end_tag line) with
        | Some tag, _ -> validate (tag :: stack) rest
        | None, Some tag ->
            let stack =
              match stack with
              | current :: tail when String.equal current tag -> tail
              | _ -> stack
            in
            validate stack rest
        | None, None -> (
            match (stack, split_first_unquoted ':' line) with
            | (("VEVENT" | "VTODO") as component) :: _, Some (left, value) ->
                let property =
                  match split_unquoted ';' left with
                  | name :: _ -> property_base_name name
                  | [] -> left
                in
                let result =
                  match (component, property) with
                  | ("VEVENT" | "VTODO"), "PRIORITY" ->
                      validate_range component property 0 9 value
                  | "VTODO", ("PERCENT" | "PERCENT-COMPLETE") ->
                      validate_range component "PERCENT-COMPLETE" 0 100 value
                  | _ -> Ok ()
                in
                Result.bind result (fun () -> validate stack rest)
            | _ -> validate stack rest))
  in
  validate [] (String.split_on_char '\n' content)

let validate_utf8 content =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String content) in
  let rec decode () =
    match Uutf.decode decoder with
    | `Uchar _ -> decode ()
    | `End -> Ok ()
    | `Malformed _ -> Error "iCalendar input is not valid UTF-8"
    | `Await -> assert false
  in
  decode ()

type event_scalar_counts = {
  uid : int;
  dtstamp : int;
  dtstart : int;
  rrule : int;
  dtend : int;
  duration : int;
}

let empty_event_scalar_counts =
  { uid = 0; dtstamp = 0; dtstart = 0; rrule = 0; dtend = 0; duration = 0 }

let validate_event_scalar_counts counts =
  let exactly_one name count =
    if count = 1 then Ok ()
    else if count = 0 then
      Error (Printf.sprintf "VEVENT requires exactly one %s" name)
    else Error (Printf.sprintf "VEVENT contains duplicate %s properties" name)
  in
  let at_most_one name count =
    if count <= 1 then Ok ()
    else Error (Printf.sprintf "VEVENT contains duplicate %s properties" name)
  in
  let* () = exactly_one "UID" counts.uid in
  let* () = exactly_one "DTSTAMP" counts.dtstamp in
  let* () = exactly_one "DTSTART" counts.dtstart in
  let* () = at_most_one "RRULE" counts.rrule in
  let* () = at_most_one "DTEND" counts.dtend in
  let* () = at_most_one "DURATION" counts.duration in
  if counts.dtend + counts.duration <= 1 then Ok ()
  else Error "VEVENT must not contain both DTEND and DURATION"

(* The pinned parser stores these properties in dedicated record fields.  A
   lexical guard is therefore required: once parsing has completed, duplicate
   source lines may already have been collapsed and are no longer observable.
   Count only direct VEVENT children so VALARM properties cannot interfere. *)
let validate_event_structural_scalars content =
  let increment property counts =
    match property with
    | "UID" -> { counts with uid = counts.uid + 1 }
    | "DTSTAMP" -> { counts with dtstamp = counts.dtstamp + 1 }
    | "DTSTART" -> { counts with dtstart = counts.dtstart + 1 }
    | "RRULE" -> { counts with rrule = counts.rrule + 1 }
    | "DTEND" -> { counts with dtend = counts.dtend + 1 }
    | "DURATION" -> { counts with duration = counts.duration + 1 }
    | _ -> counts
  in
  let rec validate stack current = function
    | [] -> Ok ()
    | line :: rest -> (
        match (begin_tag line, end_tag line) with
        | Some "VEVENT", _ ->
            validate ("VEVENT" :: stack) (Some empty_event_scalar_counts) rest
        | Some tag, _ -> validate (tag :: stack) current rest
        | None, Some "VEVENT" ->
            let* () =
              match current with
              | Some counts -> validate_event_scalar_counts counts
              | None -> Ok ()
            in
            let stack =
              match stack with "VEVENT" :: tail -> tail | _ -> stack
            in
            validate stack None rest
        | None, Some tag ->
            let stack =
              match stack with
              | current_tag :: tail when String.equal current_tag tag -> tail
              | _ -> stack
            in
            validate stack current rest
        | None, None ->
            let current =
              match (stack, current, split_first_unquoted ':' line) with
              | "VEVENT" :: _, Some counts, Some (left, _) ->
                  let property =
                    match split_unquoted ';' left with
                    | name :: _ ->
                        property_base_name name |> String.uppercase_ascii
                    | [] -> String.uppercase_ascii left
                  in
                  Some (increment property counts)
              | _ -> current
            in
            validate stack current rest)
  in
  validate [] None (String.split_on_char '\n' content)

let validate_calendar_properties properties =
  let versions =
    List.filter_map
      (function `Version (_, value) -> Some value | _ -> None)
      properties
  in
  let prodids =
    List.filter_map
      (function `Prodid (_, value) -> Some value | _ -> None)
      properties
  in
  let calscales =
    List.filter_map
      (function `Calscale (_, value) -> Some value | _ -> None)
      properties
  in
  let methods =
    List.filter_map
      (function `Method (_, value) -> Some value | _ -> None)
      properties
  in
  let singleton name = function
    | [] | [ _ ] -> Ok ()
    | _ -> Error (Printf.sprintf "VCALENDAR contains duplicate %s" name)
  in
  let* () =
    match versions with
    | [ "2.0" ] -> Ok ()
    | [] -> Error "VCALENDAR requires exactly one VERSION:2.0"
    | [ _ ] -> Error "VCALENDAR VERSION must be 2.0"
    | _ -> Error "VCALENDAR contains duplicate VERSION properties"
  in
  let* () =
    match prodids with
    | [ value ] when String.trim value <> "" -> Ok ()
    | [] -> Error "VCALENDAR requires exactly one non-empty PRODID"
    | [ _ ] -> Error "VCALENDAR PRODID must not be empty"
    | _ -> Error "VCALENDAR contains duplicate PRODID properties"
  in
  let* () = singleton "CALSCALE" calscales in
  singleton "METHOD" methods

let parse content =
  (* The pinned iCalendar fork treats equal VALARMs as duplicates while
     parsing.  A private, per-parse marker makes each source block distinct;
     it is removed from the typed value immediately and can never be written. *)
  let* () = validate_utf8 content in
  let token = fresh_uuid () in
  let protected = protect_unknown_components content in
  let protected = add_opaque_placeholder_if_needed protected token in
  let protected = normalize_rfc_syntax protected in
  let* () = validate_event_structural_scalars protected in
  let protected = mark_date_until protected in
  match validate_registered_numbers protected with
  | Error _ as error -> error
  | Ok () -> (
      match Icalendar.parse (mark_alarm_blocks protected token) with
      | Error _ as error -> error
      | Ok (properties, components) ->
          let* () = validate_calendar_properties properties in
          Ok
            ( properties,
              components
              |> List.filter (Fun.negate (is_opaque_placeholder token))
              |> List.map (strip_component_markers token) ))

let canonicalize_property old_name new_name line =
  let old_length = String.length old_name in
  if
    String.length line > old_length
    && String.starts_with ~prefix:old_name line
    && (line.[old_length] = ':' || line.[old_length] = ';')
  then new_name ^ String.sub line old_length (String.length line - old_length)
  else line

let valid_unknown_block raw =
  let lines = String.split_on_char '\n' raw in
  let rec balanced stack = function
    | [] -> stack = []
    | line :: rest -> (
        match (begin_tag line, end_tag line, stack) with
        | Some tag, _, _ -> balanced (tag :: stack) rest
        | None, Some tag, current :: tail when String.equal tag current ->
            if tail = [] then rest = [] else balanced tail rest
        | None, Some _, _ -> false
        | None, None, _ -> balanced stack rest)
  in
  match lines with
  | first :: _ -> (
      match begin_tag first with
      | Some tag when not (known_component tag) -> balanced [] lines
      | Some _ | None -> false)
  | [] -> false

let opaque_component_of_property property =
  match
    xprop_matches ~vendor:raw_component_vendor ~name:raw_component_name property
  with
  | Some encoded -> (
      match decode_opaque_component encoded with
      | Some raw when valid_unknown_block raw -> Some raw
      | Some _ | None -> None)
  | None -> None

let has_opaque_components (properties, _) =
  List.exists
    (fun property -> Option.is_some (opaque_component_of_property property))
    properties

let decode_unknown_component logical =
  if String.starts_with ~prefix:raw_component_marker logical then
    let encoded =
      String.sub logical
        (String.length raw_component_marker)
        (String.length logical - String.length raw_component_marker)
    in
    match decode_opaque_component encoded with
    | Some raw when valid_unknown_block raw -> Some raw
    | Some _ | None -> None
  else None

let restore_unknown_components lines =
  let continuation line =
    let logical = logical_line line in
    String.length logical > 0 && (logical.[0] = ' ' || logical.[0] = '\t')
  in
  let rec take_continuations logical originals = function
    | line :: rest when continuation line ->
        let continuation = logical_line line in
        take_continuations
          (logical ^ String.sub continuation 1 (String.length continuation - 1))
          (line :: originals) rest
    | rest -> (logical, List.rev originals, rest)
  in
  let rec restore accumulated = function
    | [] -> List.rev accumulated
    | line :: rest -> (
        let unfolded, continuations, remaining =
          take_continuations (logical_line line) [] rest
        in
        match decode_unknown_component unfolded with
        | Some raw -> restore (raw :: accumulated) remaining
        | None ->
            restore
              (List.rev_append continuations (line :: accumulated))
              remaining)
  in
  restore [] lines

let fold_ascii_content_line ~cr line =
  let suffix = if cr then "\r" else "" in
  let rec chunks first accumulated remaining =
    let capacity = if first then 75 else 74 in
    if String.length remaining <= capacity then
      List.rev
        (((if first then "" else " ") ^ remaining ^ suffix) :: accumulated)
    else
      let chunk = String.sub remaining 0 capacity in
      let rest =
        String.sub remaining capacity (String.length remaining - capacity)
      in
      chunks false
        (((if first then "" else " ") ^ chunk ^ suffix) :: accumulated)
        rest
  in
  chunks true [] line

let restore_date_until ~cr lines =
  let prefix = "X-CALEDONIA-DATE-UNTIL=" in
  let continuation line =
    let logical = logical_line line in
    String.length logical > 0 && (logical.[0] = ' ' || logical.[0] = '\t')
  in
  let rec take logical originals = function
    | line :: rest when continuation line ->
        let physical = logical_line line in
        take
          (logical ^ String.sub physical 1 (String.length physical - 1))
          (line :: originals) rest
    | rest -> (logical, List.rev originals, rest)
  in
  let restore_rrule logical =
    match split_first_unquoted ':' logical with
    | None -> None
    | Some (left, value) -> (
        match split_unquoted ';' left with
        | property :: parameters
          when String.equal (String.uppercase_ascii property) "RRULE" ->
            let marker, parameters =
              List.fold_left
                (fun (marker, kept) parameter ->
                  let upper = String.uppercase_ascii parameter in
                  if String.starts_with ~prefix upper then
                    let encoded =
                      String.sub parameter (String.length prefix)
                        (String.length parameter - String.length prefix)
                    in
                    (decode_date_until encoded, kept)
                  else (marker, parameter :: kept))
                (None, []) parameters
            in
            Option.map
              (fun date ->
                let until_datetime = "UNTIL=" ^ date ^ "T000000Z" in
                let value =
                  value |> String.split_on_char ';'
                  |> List.map (fun part ->
                      if
                        String.equal
                          (String.uppercase_ascii part)
                          until_datetime
                      then "UNTIL=" ^ date
                      else part)
                  |> String.concat ";"
                in
                String.concat ";" (property :: List.rev parameters)
                ^ ":" ^ value)
              marker
        | _ -> None)
  in
  let rec loop accumulated = function
    | [] -> List.rev accumulated
    | line :: rest -> (
        let logical, continuations, remaining =
          take (logical_line line) [] rest
        in
        match restore_rrule logical with
        | Some restored ->
            loop
              (List.rev_append
                 (fold_ascii_content_line ~cr restored)
                 accumulated)
              remaining
        | None ->
            loop (List.rev_append continuations (line :: accumulated)) remaining
        )
  in
  loop [] lines

let to_ics ?(cr = false) calendar =
  Icalendar.to_ics ~cr calendar
  |> String.split_on_char '\n'
  |> List.map (fun line ->
      line
      |> canonicalize_property "PERCENT" "PERCENT-COMPLETE"
      |> canonicalize_property "RELATED" "RELATED-TO"
      |> canonicalize_property "RESOURCE" "RESOURCES")
  |> restore_date_until ~cr |> restore_unknown_components |> String.concat "\n"

(* The legacy codec above exposes the pinned parser's compatibility markers in
   an [Icalendar.calendar].  The ordered document representation below is the
   migration boundary for document ownership: markers are removed from typed
   values, and unsupported blocks are represented explicitly instead of being
   disguised as calendar properties. *)

type raw_item = Raw_line of string | Raw_block of raw_block

and raw_block = {
  tag : string;
  begin_line : string;
  items : raw_item list;
  end_line : string;
}

type opaque_entry = { name : string; raw : string }

type opaque_attachment = {
  parent_path : int list;
  before_child : int;
  opaque : opaque_entry;
}

type known_entry = {
  payload : Icalendar.component;
  rrule_date_untils : Ptime.date option list;
  nested_opaque : opaque_attachment list;
}

type entry = Known of known_entry | Opaque of opaque_entry

type t = {
  calendar_properties : Icalendar.cal_prop list;
  ordered_entries : entry list;
}

let rec raw_block_lines block =
  block.begin_line
  :: (List.concat_map
        (function
          | Raw_line line -> [ line ] | Raw_block child -> raw_block_lines child)
        block.items
     @ [ block.end_line ])

let raw_block_string block = String.concat "\n" (raw_block_lines block)

let rec parse_raw_block begin_line remaining =
  match begin_tag begin_line with
  | None -> Error "Internal calendar block scan did not start at BEGIN"
  | Some tag ->
      let rec collect accumulated = function
        | [] -> Error ("Unterminated " ^ tag ^ " component block")
        | line :: rest -> (
            match begin_tag line with
            | Some _ ->
                let* child, rest = parse_raw_block line rest in
                collect (Raw_block child :: accumulated) rest
            | None -> (
                match end_tag line with
                | Some closing when String.equal closing tag ->
                    Ok
                      ( {
                          tag;
                          begin_line;
                          items = List.rev accumulated;
                          end_line = line;
                        },
                        rest )
                | Some closing ->
                    Error
                      (Printf.sprintf "Mismatched END:%s inside BEGIN:%s"
                         closing tag)
                | None -> collect (Raw_line line :: accumulated) rest))
      in
      collect [] remaining

let raw_calendar_block content =
  match String.split_on_char '\n' content with
  | begin_line :: rest when begin_tag begin_line = Some "VCALENDAR" ->
      let* block, trailing = parse_raw_block begin_line rest in
      if
        List.for_all (fun line -> String.trim (logical_line line) = "") trailing
      then Ok (block, trailing)
      else Error "Content appears after the VCALENDAR envelope"
  | _ -> Error "VCALENDAR must begin with BEGIN:VCALENDAR"

let top_level_component = function
  | "VEVENT" | "VTODO" | "VJOURNAL" | "VFREEBUSY" | "VTIMEZONE" -> true
  | _ -> false

let tag_of_component = function
  | `Event _ -> "VEVENT"
  | `Todo _ -> "VTODO"
  | `Journal _ -> "VJOURNAL"
  | `Freebusy _ -> "VFREEBUSY"
  | `Timezone _ -> "VTIMEZONE"

let opaque_of_block block = { name = block.tag; raw = raw_block_string block }

let rec nested_opaque_at path block =
  let rec collect child_index = function
    | [] -> []
    | Raw_line _ :: rest -> collect child_index rest
    | Raw_block child :: rest when known_component child.tag ->
        nested_opaque_at (path @ [ child_index ]) child
        @ collect (child_index + 1) rest
    | Raw_block child :: rest ->
        {
          parent_path = path;
          before_child = child_index;
          opaque = opaque_of_block child;
        }
        :: collect child_index rest
  in
  collect 0 block.items

let date_until_parameter : type value. value Icalendar.icalparameter -> bool =
  function
  | Icalendar.Iana_param name ->
      String.equal (String.uppercase_ascii name) "X-CALEDONIA-DATE-UNTIL"
  | Icalendar.Xparam (vendor, name) ->
      String.equal (String.uppercase_ascii vendor) date_until_vendor
      && String.equal (String.uppercase_ascii name) date_until_name
  | _ -> false

let remove_date_until_parameter params =
  params
  |> Icalendar.Params.remove
       (Icalendar.Iana_param ("X-" ^ date_until_vendor ^ "-" ^ date_until_name))
  |> Icalendar.Params.remove
       (Icalendar.Xparam (date_until_vendor, date_until_name))

let ptime_date_of_basic value =
  if
    String.length value = 8
    && String.for_all (function '0' .. '9' -> true | _ -> false) value
  then
    let year = int_of_string (String.sub value 0 4) in
    let month = int_of_string (String.sub value 4 2) in
    let day = int_of_string (String.sub value 6 2) in
    let date = (year, month, day) in
    if Option.is_some (Ptime.of_date date) then Some date else None
  else None

let basic_of_ptime_date (year, month, day) =
  Printf.sprintf "%04d%02d%02d" year month day

let extract_rrule_params params =
  let marker_count =
    Icalendar.Params.bindings params
    |> List.fold_left
         (fun count (Icalendar.Params.B (parameter, _)) ->
           if date_until_parameter parameter then count + 1 else count)
         0
  in
  let date = date_until_of_params params in
  if marker_count = 0 then Ok (params, None)
  else
    match (marker_count, Option.bind date ptime_date_of_basic) with
    | 1, Some date -> Ok (remove_date_until_parameter params, Some date)
    | _ -> Error "RRULE contains an unauthenticated internal DATE UNTIL marker"

let extract_rrule_properties properties =
  let rec collect accumulated_properties accumulated_dates = function
    | [] -> Ok (List.rev accumulated_properties, List.rev accumulated_dates)
    | `Rrule (params, recurrence) :: rest ->
        let* params, date = extract_rrule_params params in
        collect
          (`Rrule (params, recurrence) :: accumulated_properties)
          (date :: accumulated_dates)
          rest
    | property :: rest ->
        collect (property :: accumulated_properties) accumulated_dates rest
  in
  collect [] [] properties

let keep_nonopaque_property property =
  Option.is_none (opaque_component_of_property property)

let strip_opaque_alarm = function
  | `Audio (alarm : Icalendar.audio_struct Icalendar.alarm_struct) ->
      `Audio
        { alarm with other = List.filter keep_nonopaque_property alarm.other }
  | `Display (alarm : Icalendar.display_struct Icalendar.alarm_struct) ->
      `Display
        { alarm with other = List.filter keep_nonopaque_property alarm.other }
  | `Email (alarm : Icalendar.email_struct Icalendar.alarm_struct) ->
      `Email
        { alarm with other = List.filter keep_nonopaque_property alarm.other }
  | `None (alarm : unit Icalendar.alarm_struct) ->
      `None
        { alarm with other = List.filter keep_nonopaque_property alarm.other }

let extract_component_compatibility = function
  | `Event (event : Icalendar.event) ->
      let* rrule, rrule_dates =
        match event.rrule with
        | None -> Ok (None, [])
        | Some (params, recurrence) ->
            let* params, date = extract_rrule_params params in
            Ok (Some (params, recurrence), [ date ])
      in
      let props = List.filter keep_nonopaque_property event.props in
      let* props, property_dates = extract_rrule_properties props in
      Ok
        ( `Event
            {
              event with
              rrule;
              props;
              alarms = List.map strip_opaque_alarm event.alarms;
            },
          rrule_dates @ property_dates )
  | `Todo (properties, alarms) ->
      let properties = List.filter keep_nonopaque_property properties in
      let* properties, dates = extract_rrule_properties properties in
      Ok (`Todo (properties, List.map strip_opaque_alarm alarms), dates)
  | `Journal properties ->
      let properties = List.filter keep_nonopaque_property properties in
      let* properties, dates = extract_rrule_properties properties in
      Ok (`Journal properties, dates)
  | `Freebusy properties ->
      Ok (`Freebusy (List.filter keep_nonopaque_property properties), [])
  | `Timezone properties ->
      let rec collect accumulated_properties accumulated_dates = function
        | [] -> Ok (List.rev accumulated_properties, List.rev accumulated_dates)
        | `Standard properties :: rest ->
            let properties = List.filter keep_nonopaque_property properties in
            let* properties, dates = extract_rrule_properties properties in
            collect
              (`Standard properties :: accumulated_properties)
              (List.rev_append dates accumulated_dates)
              rest
        | `Daylight properties :: rest ->
            let properties = List.filter keep_nonopaque_property properties in
            let* properties, dates = extract_rrule_properties properties in
            collect
              (`Daylight properties :: accumulated_properties)
              (List.rev_append dates accumulated_dates)
              rest
        | property :: rest ->
            if keep_nonopaque_property property then
              collect
                (property :: accumulated_properties)
                accumulated_dates rest
            else collect accumulated_properties accumulated_dates rest
      in
      let* properties, dates = collect [] [] properties in
      Ok (`Timezone properties, dates)

let add_date_until_parameter date params =
  let params = remove_date_until_parameter params in
  let date = basic_of_ptime_date date in
  Icalendar.Params.add
    (Icalendar.Iana_param ("X-" ^ date_until_vendor ^ "-" ^ date_until_name))
    [ `String (encode_date_until date) ]
    params

let restore_rrule_params dates params =
  match dates with
  | [] -> Error "Codec RRULE metadata no longer matches its component"
  | None :: rest -> Ok (params, rest)
  | Some date :: rest -> Ok (add_date_until_parameter date params, rest)

let restore_rrule_properties dates properties =
  let rec collect accumulated dates = function
    | [] -> Ok (List.rev accumulated, dates)
    | `Rrule (params, recurrence) :: rest ->
        let* params, dates = restore_rrule_params dates params in
        collect (`Rrule (params, recurrence) :: accumulated) dates rest
    | property :: rest -> collect (property :: accumulated) dates rest
  in
  collect [] dates properties

let restore_component_compatibility known =
  let finish component = function
    | [] -> Ok component
    | _ -> Error "Codec RRULE metadata no longer matches its component"
  in
  match known.payload with
  | `Event (event : Icalendar.event) ->
      let* rrule, dates =
        match event.rrule with
        | None -> Ok (None, known.rrule_date_untils)
        | Some (params, recurrence) ->
            let* params, dates =
              restore_rrule_params known.rrule_date_untils params
            in
            Ok (Some (params, recurrence), dates)
      in
      let* props, dates = restore_rrule_properties dates event.props in
      finish (`Event { event with rrule; props }) dates
  | `Todo (properties, alarms) ->
      let* properties, dates =
        restore_rrule_properties known.rrule_date_untils properties
      in
      finish (`Todo (properties, alarms)) dates
  | `Journal properties ->
      let* properties, dates =
        restore_rrule_properties known.rrule_date_untils properties
      in
      finish (`Journal properties) dates
  | `Freebusy _ as component -> finish component known.rrule_date_untils
  | `Timezone properties ->
      let rec collect accumulated dates = function
        | [] -> Ok (List.rev accumulated, dates)
        | `Standard properties :: rest ->
            let* properties, dates =
              restore_rrule_properties dates properties
            in
            collect (`Standard properties :: accumulated) dates rest
        | `Daylight properties :: rest ->
            let* properties, dates =
              restore_rrule_properties dates properties
            in
            collect (`Daylight properties :: accumulated) dates rest
        | property :: rest -> collect (property :: accumulated) dates rest
      in
      let* properties, dates = collect [] known.rrule_date_untils properties in
      finish (`Timezone properties) dates

let rec remove_nested_opaque block =
  let items =
    List.filter_map
      (function
        | Raw_line _ as line -> Some line
        | Raw_block child when known_component child.tag ->
            Some (Raw_block (remove_nested_opaque child))
        | Raw_block _ -> None)
      block.items
  in
  { block with items }

let parser_source_without_nested_opaque source trailing =
  let items =
    List.map
      (function
        | Raw_block block when top_level_component block.tag ->
            Raw_block (remove_nested_opaque block)
        | item -> item)
      source.items
  in
  String.concat "\n" (raw_block_lines { source with items } @ trailing)

let parse_document content =
  let* source, trailing = raw_calendar_block content in
  let parser_source = parser_source_without_nested_opaque source trailing in
  let* properties, components = parse parser_source in
  let source_entries =
    List.filter_map
      (function
        | Raw_line _ -> None
        | Raw_block block when top_level_component block.tag ->
            Some (`Known block)
        | Raw_block block -> Some (`Opaque block))
      source.items
  in
  let rec zip accumulated components = function
    | [] ->
        if components = [] then Ok (List.rev accumulated)
        else Error "Parsed component count does not match the source document"
    | `Opaque block :: rest ->
        let raw = raw_block_string block in
        if valid_unknown_block raw then
          zip (Opaque (opaque_of_block block) :: accumulated) components rest
        else Error ("Invalid opaque " ^ block.tag ^ " component block")
    | `Known block :: rest -> (
        match components with
        | [] ->
            Error "Parsed component count does not match the source document"
        | component :: components ->
            if not (String.equal block.tag (tag_of_component component)) then
              Error
                (Printf.sprintf
                   "Parsed %s component does not match source %s component"
                   (tag_of_component component)
                   block.tag)
            else
              let* payload, rrule_date_untils =
                extract_component_compatibility component
              in
              let known =
                {
                  payload;
                  rrule_date_untils;
                  nested_opaque = nested_opaque_at [] block;
                }
              in
              if
                List.for_all
                  (fun attachment -> valid_unknown_block attachment.opaque.raw)
                  known.nested_opaque
              then zip (Known known :: accumulated) components rest
              else
                Error
                  ("Invalid nested opaque component in " ^ block.tag
                 ^ " component"))
  in
  let* ordered_entries = zip [] components source_entries in
  let calendar_properties = List.filter keep_nonopaque_property properties in
  Ok { calendar_properties; ordered_entries }

let properties document = document.calendar_properties
let entries document = document.ordered_entries
let component known = known.payload
let rrule_date_untils known = known.rrule_date_untils
let opaque_name opaque = opaque.name

let rrules_of_properties properties =
  List.filter_map
    (function `Rrule (_, recurrence) -> Some recurrence | _ -> None)
    properties

let rrules_of_component = function
  | `Event (event : Icalendar.event) ->
      Option.to_list (Option.map snd event.rrule)
      @ rrules_of_properties event.props
  | `Todo (properties, _) -> rrules_of_properties properties
  | `Journal properties -> rrules_of_properties properties
  | `Freebusy _ -> []
  | `Timezone properties ->
      List.concat_map
        (function
          | `Standard properties | `Daylight properties ->
              rrules_of_properties properties
          | _ -> [])
        properties

let metadata_matches_rrule recurrence = function
  | None -> true
  | Some date -> (
      match recurrence with
      | _, Some (`Until (`Utc until)), _, _ -> (
          match Ptime.of_date date with
          | Some midnight -> Ptime.compare until midnight = 0
          | None -> false)
      | _, (None | Some (`Count _ | `Until (`Local _))), _, _ -> false)

let make_known ?rrule_date_untils payload =
  let* payload, inferred = extract_component_compatibility payload in
  let* rrule_date_untils =
    match rrule_date_untils with
    | None -> Ok inferred
    | Some supplied when List.length supplied <> List.length inferred ->
        Error "Explicit DATE UNTIL metadata does not match the RRULE count"
    | Some supplied ->
        if
          List.for_all
            (function
              | None -> true | Some date -> Option.is_some (Ptime.of_date date))
            supplied
        then Ok supplied
        else Error "Explicit DATE UNTIL metadata contains an invalid date"
  in
  let recurrences = rrules_of_component payload in
  if
    List.length recurrences = List.length rrule_date_untils
    && List.for_all2 metadata_matches_rrule recurrences rrule_date_untils
  then Ok { payload; rrule_date_untils; nested_opaque = [] }
  else Error "DATE UNTIL metadata does not match the typed RRULE limit"

let create ~properties components =
  let rec make accumulated = function
    | [] -> Ok (List.rev accumulated)
    | component :: rest ->
        let* known = make_known component in
        make (Known known :: accumulated) rest
  in
  let* ordered_entries = make [] components in
  Ok
    {
      calendar_properties = List.filter keep_nonopaque_property properties;
      ordered_entries;
    }

let create_known ~properties known_entries =
  {
    calendar_properties = List.filter keep_nonopaque_property properties;
    ordered_entries = List.map (fun known -> Known known) known_entries;
  }

let has_opaque_entries document =
  List.exists
    (function Opaque _ -> true | Known known -> known.nested_opaque <> [])
    document.ordered_entries

type rewrite = Keep | Delete | Replace of known_entry list

let rewrite_known document ~f =
  let rec rewrite accumulated = function
    | [] -> Ok (List.rev accumulated)
    | (Opaque _ as opaque) :: rest -> rewrite (opaque :: accumulated) rest
    | Known known :: rest -> (
        match f known with
        | Keep -> rewrite (Known known :: accumulated) rest
        | Delete -> rewrite accumulated rest
        | Replace [] ->
            Error "Replace requires at least one known entry; use Delete"
        | Replace (first :: additional) ->
            let first = { first with nested_opaque = known.nested_opaque } in
            let replacements =
              List.map (fun known -> Known known) (first :: additional)
            in
            rewrite (List.rev_append replacements accumulated) rest)
  in
  let* ordered_entries = rewrite [] document.ordered_entries in
  Ok { document with ordered_entries }

let block_from_raw raw =
  match String.split_on_char '\n' raw with
  | begin_line :: rest ->
      let* block, trailing = parse_raw_block begin_line rest in
      if trailing = [] then Ok block
      else Error "Opaque block contains trailing content"
  | [] -> Error "Opaque block is empty"

let attachments_at path attachments =
  List.filter (fun attachment -> attachment.parent_path = path) attachments

let attachment_blocks attachments =
  List.map
    (fun attachment ->
      match block_from_raw attachment.opaque.raw with
      | Ok block -> Raw_block block
      | Error error -> invalid_arg ("Calendar codec invariant: " ^ error))
    attachments

let rec inject_nested_opaque attachments path block =
  let direct = attachments_at path attachments in
  let before child_index =
    direct
    |> List.filter (fun attachment -> attachment.before_child = child_index)
    |> attachment_blocks
  in
  let after child_count =
    direct
    |> List.filter (fun attachment -> attachment.before_child >= child_count)
    |> attachment_blocks
  in
  let rec inject child_index = function
    | [] -> after child_index
    | Raw_line line :: rest -> Raw_line line :: inject child_index rest
    | Raw_block child :: rest when known_component child.tag ->
        before child_index
        @ Raw_block
            (inject_nested_opaque attachments (path @ [ child_index ]) child)
          :: inject (child_index + 1) rest
    | Raw_block child :: rest -> Raw_block child :: inject child_index rest
  in
  { block with items = inject 0 block.items }

let explicit_date_recurrence_id_params params = function
  | `Date _ -> Icalendar.Params.add Icalendar.Valuetype `Date params
  | `Datetime _ -> params

let explicit_event_recurrence_id (property : Icalendar.event_prop) =
  match property with
  | `Recur_id (params, value) ->
      `Recur_id (explicit_date_recurrence_id_params params value, value)
  | property -> property

let explicit_todo_recurrence_id (property : Icalendar.todo_prop) =
  match property with
  | `Recur_id (params, value) ->
      `Recur_id (explicit_date_recurrence_id_params params value, value)
  | property -> property

let explicit_journal_recurrence_id (property : Icalendar.journal_prop) =
  match property with
  | `Recur_id (params, value) ->
      `Recur_id (explicit_date_recurrence_id_params params value, value)
  | property -> property

let with_explicit_date_recurrence_ids = function
  | `Event (event : Icalendar.event) ->
      `Event
        { event with props = List.map explicit_event_recurrence_id event.props }
  | `Todo (properties, alarms) ->
      `Todo (List.map explicit_todo_recurrence_id properties, alarms)
  | `Journal properties ->
      `Journal (List.map explicit_journal_recurrence_id properties)
  | (`Freebusy _ | `Timezone _) as component -> component

let single_component_block ~cr properties component =
  let component = with_explicit_date_recurrence_ids component in
  let serialized = to_ics ~cr (properties, [ component ]) in
  let* calendar, _ = raw_calendar_block serialized in
  match
    List.filter_map
      (function Raw_block block -> Some block | Raw_line _ -> None)
      calendar.items
  with
  | [ block ] -> Ok block
  | _ -> Error "Compatibility writer did not emit exactly one component"

let serialize ?(cr = false) document =
  let envelope = to_ics ~cr (document.calendar_properties, []) in
  let envelope, trailing =
    match raw_calendar_block envelope with
    | Ok parsed -> parsed
    | Error error -> invalid_arg ("Calendar codec invariant: " ^ error)
  in
  let item_of_entry = function
    | Opaque opaque -> (
        match block_from_raw opaque.raw with
        | Ok block -> Raw_block block
        | Error error -> invalid_arg ("Calendar codec invariant: " ^ error))
    | Known known ->
        let component =
          match restore_component_compatibility known with
          | Ok component -> component
          | Error error -> invalid_arg ("Calendar codec invariant: " ^ error)
        in
        let block =
          match
            single_component_block ~cr document.calendar_properties component
          with
          | Ok block -> block
          | Error error -> invalid_arg ("Calendar codec invariant: " ^ error)
        in
        Raw_block (inject_nested_opaque known.nested_opaque [] block)
  in
  let envelope =
    {
      envelope with
      items = envelope.items @ List.map item_of_entry document.ordered_entries;
    }
  in
  String.concat "\n" (raw_block_lines envelope @ trailing)

let parse_event_rrule value =
  if String.trim value = "" then Error "Recurrence rule cannot be empty"
  else if String.contains value '\n' || String.contains value '\r' then
    Error "RRULE must be one unfolded content-line value"
  else
    let source =
      String.concat "\r\n"
        [
          "BEGIN:VCALENDAR";
          "VERSION:2.0";
          "PRODID:-//Caledonia//Recurrence validator//EN";
          "BEGIN:VEVENT";
          "UID:recurrence-validation";
          "DTSTAMP:20260101T000000Z";
          "DTSTART:20260101T000000Z";
          "RRULE:" ^ value;
          "END:VEVENT";
          "END:VCALENDAR";
          "";
        ]
    in
    let* document = parse_document source in
    match document.ordered_entries with
    | [ Known known ] -> (
        match (known.payload, known.rrule_date_untils) with
        | `Event { rrule = Some (params, recurrence); _ }, [ date_until ] ->
            Ok (params, recurrence, date_until)
        | `Event _, _ -> Error "RRULE did not produce one typed recurrence"
        | _ -> Error "RRULE validator did not produce a VEVENT")
    | _ -> Error "RRULE validator produced an unexpected document shape"

let canonical_unfolded_lines source =
  source |> String.split_on_char '\n'
  |> List.fold_left
       (fun lines line ->
         let line = logical_line line in
         match (lines, String.length line > 0) with
         | previous :: rest, true when line.[0] = ' ' || line.[0] = '\t' ->
             (previous ^ String.sub line 1 (String.length line - 1)) :: rest
         | _ -> line :: lines)
       []
  |> List.rev

let canonical_event_recurrence_lines ~date_until event =
  let metadata =
    match event.Icalendar.rrule with None -> [] | Some _ -> [ date_until ]
  in
  let* known = make_known ~rrule_date_untils:metadata (`Event event) in
  let document =
    create_known
      ~properties:
        [
          `Prodid (Icalendar.Params.empty, "-//Freumh//Caledonia//EN");
          `Version (Icalendar.Params.empty, "2.0");
        ]
      [ known ]
  in
  Ok
    (serialize ~cr:true document
    |> canonical_unfolded_lines
    |> List.filter (fun line ->
        List.exists
          (fun property ->
            String.starts_with ~prefix:(property ^ ":") line
            || String.starts_with ~prefix:(property ^ ";") line)
          [ "RRULE"; "RDATE"; "EXDATE" ]))

let parameter_probe_name = "X-CALEDONIA-PARAMETER"
let parameter_probe_value = "caledonia-parameter-value"

let encoded_parameter_bindings source =
  let prefix = parameter_probe_name ^ ";" in
  let suffix = ":" ^ parameter_probe_value in
  match
    canonical_unfolded_lines source
    |> List.find_opt (String.starts_with ~prefix)
  with
  | Some line when String.ends_with ~suffix line ->
      let encoded =
        String.sub line (String.length prefix)
          (String.length line - String.length prefix - String.length suffix)
      in
      encoded |> split_unquoted ';'
      |> List.fold_left
           (fun parsed binding ->
             let* parsed = parsed in
             match String.index_opt binding '=' with
             | Some separator ->
                 Ok
                   (( String.sub binding 0 separator,
                      String.sub binding (separator + 1)
                        (String.length binding - separator - 1) )
                   :: parsed)
             | None -> Error "could not serialize content-line parameters")
           (Ok [])
      |> Result.map List.rev
  | _ -> Error "could not serialize content-line parameters"

let parameter_probe_document encoded =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia//Codec parameter probe//EN";
      "BEGIN:VEVENT";
      "UID:caledonia-codec-parameter-probe";
      "DTSTAMP:20260101T000000Z";
      "DTSTART:20260101T000000Z";
      parameter_probe_name ^ ";" ^ encoded ^ ":" ^ parameter_probe_value;
      "END:VEVENT";
      "END:VCALENDAR";
      "";
    ]

let parse_content_line_parameters encoded =
  let* document = parse_document (parameter_probe_document encoded) in
  let params =
    document.ordered_entries
    |> List.find_map (function
      | Known { payload = `Event event; _ } ->
          List.find_map
            (function `Xprop (_, params, _) -> Some params | _ -> None)
            event.props
      | Known _ | Opaque _ -> None)
  in
  match params with
  | None -> Error "parameter probe did not produce typed parameters"
  | Some params ->
      let* bindings =
        encoded_parameter_bindings (serialize ~cr:true document)
      in
      Ok (params, bindings)

let canonical_parameter_binding (Icalendar.Params.B (key, value)) =
  let properties =
    [
      `Version (Icalendar.Params.empty, "2.0");
      `Prodid (Icalendar.Params.empty, "-//Caledonia//Codec parameter//EN");
      `Xprop
        ( ("CALEDONIA", "PARAMETER"),
          Icalendar.Params.singleton key value,
          parameter_probe_value );
    ]
  in
  match create ~properties [] with
  | Error _ -> ("INVALID", "")
  | Ok document -> (
      match encoded_parameter_bindings (serialize ~cr:true document) with
      | Ok [ binding ] -> binding
      | Ok _ | Error _ -> ("INVALID", ""))

let synthetic_event ?(props = []) ?(alarms = []) start =
  {
    Icalendar.dtstamp = (Icalendar.Params.empty, Ptime.epoch);
    uid = (Icalendar.Params.empty, "caledonia-codec-probe");
    dtstart = (Icalendar.Params.empty, start);
    dtend_or_duration = None;
    rrule = None;
    props;
    alarms;
  }

let canonical_recurrence_id_line (params, value) =
  let recurrence_id =
    (explicit_date_recurrence_id_params params value, value)
  in
  let event = synthetic_event ~props:[ `Recur_id recurrence_id ] value in
  Icalendar.to_ics ~cr:true
    ( [
        `Prodid (Icalendar.Params.empty, "-//Caledonia codec probe//EN");
        `Version (Icalendar.Params.empty, "2.0");
      ],
      [ `Event event ] )
  |> canonical_unfolded_lines
  |> List.find_opt (String.starts_with ~prefix:"RECURRENCE-ID")
  |> Option.value ~default:"RECURRENCE-ID"

let canonical_alarm_block alarm =
  let event =
    synthetic_event ~alarms:[ alarm ] (`Datetime (`Utc Ptime.epoch))
  in
  let lines =
    Icalendar.to_ics ~cr:true
      ( [
          `Prodid (Icalendar.Params.empty, "-//Caledonia codec probe//EN");
          `Version (Icalendar.Params.empty, "2.0");
        ],
        [ `Event event ] )
    |> canonical_unfolded_lines
  in
  let rec collect inside accumulated = function
    | [] -> None
    | "BEGIN:VALARM" :: rest -> collect true [ "BEGIN:VALARM" ] rest
    | "END:VALARM" :: _ when inside ->
        Some (String.concat "\r\n" (List.rev ("END:VALARM" :: accumulated)))
    | line :: rest when inside -> collect true (line :: accumulated) rest
    | _ :: rest -> collect false accumulated rest
  in
  collect false [] lines |> Option.value ~default:"BEGIN:VALARM\r\nEND:VALARM"

module Legacy = struct
  let parse = parse
  let to_ics = to_ics
  let date_until_of_params = date_until_of_params
  let has_opaque_components = has_opaque_components
end
