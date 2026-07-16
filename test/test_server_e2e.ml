open Caledonia_lib

let field name = function
  | Sexplib.Sexp.List fields ->
      List.find_map
        (function
          | Sexplib.Sexp.List [ Sexplib.Sexp.Atom key; value ]
            when String.equal key name ->
              Some value
          | _ -> None)
        fields
  | _ -> None

let atom = function Sexplib.Sexp.Atom value -> Some value | _ -> None

let response_request_id response =
  match response with
  | Sexplib.Sexp.List [ Sexplib.Sexp.Atom "Response"; fields ] ->
      Option.bind (field "request_id" fields) atom
  | _ -> None

let response_value response =
  match response with
  | Sexplib.Sexp.List [ Sexplib.Sexp.Atom "Response"; fields ] ->
      field "response" fields
  | _ -> None

let first_event response =
  match response_value response with
  | Some
      (Sexplib.Sexp.List
         [
           Sexplib.Sexp.Atom "Ok";
           Sexplib.Sexp.List
             [ Sexplib.Sexp.Atom "Events"; Sexplib.Sexp.List (event :: _) ];
         ]) ->
      Some event
  | _ -> None

let response_events response =
  match response_value response with
  | Some
      (Sexplib.Sexp.List
         [
           Sexplib.Sexp.Atom "Ok";
           Sexplib.Sexp.List
             [ Sexplib.Sexp.Atom "Events"; Sexplib.Sexp.List events ];
         ]) ->
      events
  | _ -> []

let event_with_id id response =
  response_events response
  |> List.find_opt (fun event -> Option.bind (field "id" event) atom = Some id)

let atom_field name value =
  Option.bind (field name value) atom |> Option.value ~default:"missing"

let error_record response =
  match response_value response with
  | Some (Sexplib.Sexp.List [ Sexplib.Sexp.Atom "Error"; record ]) ->
      Some record
  | _ -> None

let read_response channel = input_line channel |> Sexplib.Sexp.of_string

let read_response_line channel =
  let line = input_line channel in
  (line, Sexplib.Sexp.of_string line)

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

let send channel request =
  output_string channel request;
  output_char channel '\n';
  flush channel

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then (
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path)
    else Unix.unlink path

let server_executable () =
  [
    Filename.concat (Sys.getcwd ()) "../bin/main.exe";
    Filename.concat (Sys.getcwd ()) "_build/default/bin/main.exe";
    Filename.concat (Sys.getcwd ()) "bin/main.exe";
  ]
  |> List.find Sys.file_exists

let initial_calendar =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia server e2e//EN";
      "BEGIN:VEVENT";
      "UID:series-e2e";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:20260715T100000Z";
      "RRULE:FREQ=WEEKLY;BYDAY=MO,WE;COUNT=4";
      "CATEGORIES:work,project";
      "SUMMARY:Master";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:series-e2e";
      "DTSTAMP:20260701T000000Z";
      "RECURRENCE-ID:20260720T100000Z";
      "DTSTART:20260720T100000Z";
      "SUMMARY:Existing override";
      "DESCRIPTION:Original override";
      "END:VEVENT";
      "END:VCALENDAR";
      "";
    ]

let occurrence_calendar =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia occurrence e2e//EN";
      "BEGIN:VEVENT";
      "UID:floating-e2e";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:20260715T090000";
      "DTEND:20260715T100000";
      "RRULE:FREQ=DAILY;COUNT=2";
      "SUMMARY:Floating master";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:date-e2e";
      "DTSTAMP:20260701T000000Z";
      "DTSTART;VALUE=DATE:20260715";
      "DTEND;VALUE=DATE:20260716";
      "RRULE:FREQ=DAILY;COUNT=2";
      "SUMMARY:Date master";
      "END:VEVENT";
      "END:VCALENDAR";
      "";
    ]

let series_deletion_calendar =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia server series deletion//EN";
      "BEGIN:VEVENT";
      "UID:delete-series-e2e";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:20260715T100000Z";
      "RRULE:FREQ=DAILY;COUNT=2";
      "SUMMARY:Delete master";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:delete-series-e2e";
      "RECURRENCE-ID:20260716T100000Z";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:20260716T110000Z";
      "SUMMARY:Delete override";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:delete-sibling-e2e";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:20260717T100000Z";
      "SUMMARY:Keep sibling";
      "END:VEVENT";
      "BEGIN:VTODO";
      "UID:delete-todo-sibling-e2e";
      "DTSTAMP:20260701T000000Z";
      "SUMMARY:Keep todo sibling";
      "END:VTODO";
      "BEGIN:VAVAILABILITY";
      "UID:delete-opaque-sibling-e2e";
      "DTSTAMP:20260701T000000Z";
      "X-OPAQUE:keep";
      "END:VAVAILABILITY";
      "END:VCALENDAR";
      "";
    ]

let request ~id payload =
  Printf.sprintf "(Request ((version 1) (request_id %S) (request %s)))" id
    payload

let has_line ~prefix source =
  source |> String.split_on_char '\n'
  |> List.exists (String.starts_with ~prefix)

let edit_request ~request_id ~file ~fingerprint ~summary ~description =
  request ~id:request_id
    (Printf.sprintf
       "(EditEvent ((id series-e2e) (calendar_key personal) (file %S) \
        (source_fingerprint %S) (summary %s) (description %s) \
        (occurrence_start 2026-07-20T10:00:00Z) (occurrence_timezone UTC)))"
       file fingerprint summary description)

let%expect_test
    "real server correlates errors and round-trips recurrence, conflicts, and \
     overrides" =
  let root = Filename.temp_file "caledonia-server-e2e-" "" in
  Sys.remove root;
  Sys.mkdir root 0o700;
  Fun.protect
    ~finally:(fun () -> remove_tree root)
    (fun () ->
      let calendar = Filename.concat root "personal" in
      Sys.mkdir calendar 0o700;
      let file = Filename.concat calendar "series.ics" in
      Out_channel.with_open_bin file (fun channel ->
          output_string channel initial_calendar);
      let initial_fingerprint =
        Digest.to_hex (Digest.string initial_calendar)
      in
      let environment =
        Array.append
          [| "CALENDAR_DIR=" ^ root; "TZ=Europe/London" |]
          (Unix.environment () |> Array.to_list
          |> List.filter (fun value ->
              not
                (String.starts_with ~prefix:"CALENDAR_DIR=" value
                || String.starts_with ~prefix:"TZ=" value))
          |> Array.of_list)
      in
      let executable = server_executable () in
      let input, output, errors =
        Unix.open_process_args_full executable [| executable; "server" |]
          environment
      in
      send output (request ~id:"hello-42" "Handshake");
      let handshake = read_response input in
      Printf.printf "handshake-id=%s\n"
        (Option.value ~default:"missing" (response_request_id handshake));

      send output
        (request ~id:"malformed-9" "(CreateEvent ((calendar personal)))");
      let malformed = read_response input in
      let malformed_error = Option.get (error_record malformed) in
      Printf.printf "malformed-id=%s code=%s\n"
        (Option.value ~default:"missing" (response_request_id malformed))
        (Option.bind (field "code" malformed_error) atom
        |> Option.value ~default:"missing");

      send output
        "(Request ((version 1) (request_id \"syntax-7\") (request \
         ListCalendars))";
      let malformed_syntax = read_response input in
      send output (request ~id:"after-syntax" "ListCalendars");
      let after_syntax = read_response input in
      Printf.printf "syntax-id=%s recovered-id=%s\n"
        (Option.value ~default:"missing" (response_request_id malformed_syntax))
        (Option.value ~default:"missing" (response_request_id after_syntax));

      let parameter name value : Sexp.alarm_parameter = { name; value } in
      let alarm : Sexp.alarm_input =
        {
          action = Audio;
          trigger = Relative { seconds = -60; related = Start };
          trigger_parameters = [];
          repeat = None;
          duration_seconds = None;
          duration_parameters = [];
          repeat_parameters = [];
          summary = None;
          summary_parameters = [];
          description = None;
          description_parameters = [];
          attendees = [];
          attendee_values = [];
          attachment = Some (Binary "AAEC/w==");
          attachment_parameters =
            [
              parameter "ENCODING" "BASE64";
              parameter "VALUE" "BINARY";
              parameter "FMTTYPE" "application/octet-stream";
              parameter "X-LABEL" "\"secure\"";
            ];
          other =
            [
              X
                {
                  namespace = "TRACE";
                  name = "ID";
                  value = "alarm-42";
                  parameters = [ parameter "X-SCOPE" "\"private\"" ];
                };
            ];
        }
      in
      let create_request : Sexp.create_event_request =
        let absolute value =
          {
            alarm with
            trigger = Absolute value;
            attachment = None;
            attachment_parameters = [];
            other = [];
          }
        in
        {
          calendar = "personal";
          summary = "Complex";
          start = { kind = Utc; value = "2026-08-03T09:30:45" };
          end_ = None;
          location = None;
          description = None;
          categories = [ "alpha"; "beta" ];
          recurrence = Some { rrule = "FREQ=WEEKLY;BYDAY=MO,WE;COUNT=4" };
          alarms =
            [
              alarm;
              absolute "2026-08-03T11:30:45+02:00";
              absolute "2026-08-03T04:30:45-05:00";
            ];
        }
      in
      let create_payload =
        Sexp.sexp_of_request (CreateEvent create_request)
        |> Sexplib.Sexp.to_string
      in
      let lossy_alarm_request =
        {
          create_request with
          alarms = [ { alarm with description = Some "not representable" } ];
        }
      in
      let lossy_alarm_payload =
        Sexp.sexp_of_request (CreateEvent lossy_alarm_request)
        |> Sexplib.Sexp.to_string
      in
      send output (request ~id:"lossy-alarm" lossy_alarm_payload);
      let lossy_alarm_response = read_response input in
      let lossy_alarm_error = Option.get (error_record lossy_alarm_response) in
      Printf.printf "lossy-alarm-code=%s\n"
        (atom_field "code" lossy_alarm_error);
      send output (request ~id:"create-complex" create_payload);
      let created = read_response input in
      let created_event =
        match first_event created with
        | Some event -> event
        | None ->
            failwith ("create response: " ^ Sexplib.Sexp.to_string_hum created)
      in
      let categories =
        match field "categories_value" created_event with
        | Some (Sexplib.Sexp.List values) ->
            List.filter_map atom values |> String.concat ","
        | _ -> "missing"
      in
      let recurrence =
        Option.bind (field "recurrence_value" created_event) (field "rrule")
        |> fun value ->
        Option.bind value atom |> Option.value ~default:"missing"
      in
      Printf.printf "categories=%s recurrence=%s\n" categories recurrence;
      let alarm =
        match field "alarms_value" created_event with
        | Some (Sexplib.Sexp.List (alarm :: _)) -> alarm
        | _ -> failwith "created event response lost alarm"
      in
      let attachment = Option.get (field "attachment" alarm) in
      let attachment_value =
        Option.bind (field "value" attachment) atom
        |> Option.value ~default:"missing"
      in
      let attachment_parameters =
        match field "parameters" attachment with
        | Some (Sexplib.Sexp.List values) ->
            List.filter_map
              (fun value -> Option.bind (field "name" value) atom)
              values
        | _ -> []
      in
      let other_parameters =
        match field "other" alarm with
        | Some (Sexplib.Sexp.List (property :: _)) -> (
            match field "parameters" property with
            | Some (Sexplib.Sexp.List values) ->
                List.filter_map
                  (fun value -> Option.bind (field "name" value) atom)
                  values
            | _ -> [])
        | _ -> []
      in
      Printf.printf "binary=%b attachment-params=%b other-params=%b\n"
        (String.equal attachment_value "AAEC/w==")
        (List.for_all
           (fun name -> List.mem name attachment_parameters)
           [ "ENCODING"; "VALUE"; "FMTTYPE"; "X-LABEL" ])
        (List.mem "X-SCOPE" other_parameters);
      let absolute_instants =
        match field "alarms_value" created_event with
        | Some (Sexplib.Sexp.List (_binary :: absolutes)) ->
            List.filter_map
              (fun alarm ->
                let value =
                  Option.bind (field "trigger" alarm) (field "value")
                  |> fun value -> Option.bind value atom
                in
                Option.bind value (fun value ->
                    match Ptime.of_rfc3339 value with
                    | Ok (timestamp, _, _) -> Some timestamp
                    | Error _ -> None))
              absolutes
        | _ -> []
      in
      let expected_instant =
        match Ptime.of_rfc3339 "2026-08-03T09:30:45Z" with
        | Ok (timestamp, _, _) -> timestamp
        | Error _ -> assert false
      in
      Printf.printf "offset-instants-normalized=%b seconds-preserved=%b\n"
        (List.length absolute_instants = 2
        && List.for_all (Ptime.equal expected_instant) absolute_instants)
        (List.for_all
           (fun timestamp ->
             let _, ((_, _, seconds), _) = Ptime.to_date_time timestamp in
             seconds = 45)
           absolute_instants);

      send output
        (edit_request ~request_id:"edit-one" ~file
           ~fingerprint:initial_fingerprint ~summary:"(Set \"First revised\")"
           ~description:"Keep");
      let edited_once = read_response input in
      let first_fingerprint =
        Option.bind (first_event edited_once) (field "source_fingerprint")
        |> fun value -> Option.bind value atom |> Option.get
      in

      send output
        (edit_request ~request_id:"edit-two" ~file
           ~fingerprint:first_fingerprint ~summary:"Keep"
           ~description:"(Set \"Second revision\")");
      let edited_twice = read_response input in
      let second_fingerprint =
        Option.bind (first_event edited_twice) (field "source_fingerprint")
        |> fun value -> Option.bind value atom |> Option.get
      in
      let twice_source = In_channel.with_open_bin file In_channel.input_all in
      Printf.printf "override-reedited=%b/%b\n"
        (has_line ~prefix:"SUMMARY:First revised" twice_source)
        (has_line ~prefix:"DESCRIPTION:Second revision" twice_source);

      send output
        (edit_request ~request_id:"stale-edit" ~file
           ~fingerprint:initial_fingerprint ~summary:"Keep"
           ~description:"(Set stale)");
      let stale = read_response input in
      let stale_error = Option.get (error_record stale) in
      Printf.printf "stale-code=%s retryable=%s\n"
        (Option.bind (field "code" stale_error) atom |> Option.get)
        (Option.bind (field "retryable" stale_error) atom |> Option.get);

      send output
        (request ~id:"delete-override"
           (Printf.sprintf
              "(DeleteEvent ((id series-e2e) (calendar_key personal) (file %S) \
               (source_fingerprint %S) (occurrence_start 2026-07-20T10:00:00Z) \
               (occurrence_timezone UTC)))"
              file second_fingerprint));
      let deleted = read_response input in
      let deleted_ok =
        match response_value deleted with
        | Some
            (Sexplib.Sexp.List
               [ Sexplib.Sexp.Atom "Ok"; Sexplib.Sexp.Atom "Empty" ]) ->
            true
        | _ -> false
      in
      let deleted_source = In_channel.with_open_bin file In_channel.input_all in
      Printf.printf
        "override-deleted=%b recurrence-id-gone=%b exdate-added=%b\n" deleted_ok
        (not (has_line ~prefix:"RECURRENCE-ID:" deleted_source))
        (has_line ~prefix:"EXDATE:20260720T100000Z" deleted_source);
      close_out output;
      let diagnostics = In_channel.input_all errors in
      let status = Unix.close_process_full (input, output, errors) in
      Printf.printf "server-exit=%b diagnostics-empty=%b\n"
        (status = Unix.WEXITED 0)
        (String.trim diagnostics = ""));
  [%expect
    {|
    handshake-id=hello-42
    malformed-id=malformed-9 code=invalid_request
    syntax-id=syntax-7 recovered-id=after-syntax
    lossy-alarm-code=invalid_request
    categories=alpha,beta recurrence=FREQ=WEEKLY;COUNT=4;BYDAY=MO,WE
    binary=true attachment-params=true other-params=true
    offset-instants-normalized=true seconds-preserved=true
    override-reedited=true/true
    stale-code=conflict retryable=true
    override-deleted=true recurrence-id-gone=true exdate-added=true
    server-exit=true diagnostics-empty=true
    |}]

let%expect_test
    "real server DeleteEvent removes a complete recurrence series atomically" =
  let root = Filename.temp_file "caledonia-server-series-delete-" "" in
  Sys.remove root;
  Sys.mkdir root 0o700;
  Fun.protect
    ~finally:(fun () -> remove_tree root)
    (fun () ->
      let calendar = Filename.concat root "personal" in
      Sys.mkdir calendar 0o700;
      let file = Filename.concat calendar "series-delete.ics" in
      Out_channel.with_open_bin file (fun channel ->
          output_string channel series_deletion_calendar);
      let fingerprint =
        Digest.to_hex (Digest.string series_deletion_calendar)
      in
      let environment =
        Array.append
          [| "CALENDAR_DIR=" ^ root; "TZ=UTC" |]
          (Unix.environment () |> Array.to_list
          |> List.filter (fun value ->
              not
                (String.starts_with ~prefix:"CALENDAR_DIR=" value
                || String.starts_with ~prefix:"TZ=" value))
          |> Array.of_list)
      in
      let executable = server_executable () in
      let input, output, errors =
        Unix.open_process_args_full executable [| executable; "server" |]
          environment
      in
      send output (request ~id:"delete-series-hello" "Handshake");
      ignore (read_response input);
      send output
        (request ~id:"delete-series"
           (Printf.sprintf
              "(DeleteEvent ((id delete-series-e2e) (calendar_key personal) \
               (file %S) (source_fingerprint %S)))"
              file fingerprint));
      let deleted = read_response input in
      let deleted_ok =
        match response_value deleted with
        | Some
            (Sexplib.Sexp.List
               [ Sexplib.Sexp.Atom "Ok"; Sexplib.Sexp.Atom "Empty" ]) ->
            true
        | _ -> false
      in
      let written = In_channel.with_open_bin file In_channel.input_all in
      Printf.printf "delete-id=%s response=%b\n"
        (Option.value ~default:"missing" (response_request_id deleted))
        deleted_ok;
      Printf.printf "series/override gone=%b/%b\n"
        (not (has_line ~prefix:"UID:delete-series-e2e" written))
        (not (has_line ~prefix:"RECURRENCE-ID:" written));
      Printf.printf "siblings/opaque kept=%b/%b/%b\n"
        (has_line ~prefix:"UID:delete-sibling-e2e" written)
        (has_line ~prefix:"UID:delete-todo-sibling-e2e" written)
        (has_line ~prefix:"UID:delete-opaque-sibling-e2e" written);
      send output (request ~id:"after-series-delete" "ListCalendars");
      let recovered = read_response input in
      close_out output;
      let diagnostics = In_channel.input_all errors in
      let status = Unix.close_process_full (input, output, errors) in
      Printf.printf "recovered-id=%s server-exit=%b diagnostics-empty=%b\n"
        (Option.value ~default:"missing" (response_request_id recovered))
        (status = Unix.WEXITED 0)
        (String.trim diagnostics = ""));
  [%expect
    {|
    delete-id=delete-series response=true
    series/override gone=true/true
    siblings/opaque kept=true/true/true
    recovered-id=after-series-delete server-exit=true diagnostics-empty=true |}]

let%expect_test
    "server rejects partial occurrence identity and malformed transport input \
     without mutating data" =
  let root = Filename.temp_file "caledonia-server-robustness-" "" in
  Sys.remove root;
  Sys.mkdir root 0o700;
  Fun.protect
    ~finally:(fun () -> remove_tree root)
    (fun () ->
      let calendar = Filename.concat root "personal" in
      Sys.mkdir calendar 0o700;
      let file = Filename.concat calendar "series.ics" in
      Out_channel.with_open_bin file (fun channel ->
          output_string channel initial_calendar);
      let fingerprint = Digest.to_hex (Digest.string initial_calendar) in
      let environment =
        Array.append
          [| "CALENDAR_DIR=" ^ root; "TZ=Europe/London" |]
          (Unix.environment () |> Array.to_list
          |> List.filter (fun value ->
              not
                (String.starts_with ~prefix:"CALENDAR_DIR=" value
                || String.starts_with ~prefix:"TZ=" value))
          |> Array.of_list)
      in
      let executable = server_executable () in
      let input, output, errors =
        Unix.open_process_args_full executable [| executable; "server" |]
          environment
      in
      let utf8_responses = ref [] in
      let receive () =
        let line, response = read_response_line input in
        utf8_responses := valid_utf8 line :: !utf8_responses;
        response
      in
      let error_code response =
        Option.bind (error_record response) (field "code") |> fun value ->
        Option.bind value atom |> Option.value ~default:"missing"
      in
      let error_message response =
        Option.bind (error_record response) (field "message") |> fun value ->
        Option.bind value atom |> Option.value ~default:"missing"
      in
      send output (request ~id:"hello-robustness" "Handshake");
      ignore (receive ());
      let requests =
        [
          ( "edit-zone-only",
            Printf.sprintf
              "(EditEvent ((id series-e2e) (calendar_key personal) (file %S) \
               (source_fingerprint %S) (summary (Set corrupted)) \
               (occurrence_timezone UTC)))"
              file fingerprint );
          ( "edit-start-only",
            Printf.sprintf
              "(EditEvent ((id series-e2e) (calendar_key personal) (file %S) \
               (source_fingerprint %S) (summary (Set corrupted)) \
               (occurrence_start 2026-07-20T10:00:00Z)))"
              file fingerprint );
          ( "delete-zone-only",
            Printf.sprintf
              "(DeleteEvent ((id series-e2e) (calendar_key personal) (file %S) \
               (source_fingerprint %S) (occurrence_timezone UTC)))"
              file fingerprint );
          ( "delete-start-only",
            Printf.sprintf
              "(DeleteEvent ((id series-e2e) (calendar_key personal) (file %S) \
               (source_fingerprint %S) (occurrence_start \
               2026-07-20T10:00:00Z)))"
              file fingerprint );
          ( "edit-date-relative",
            Printf.sprintf
              "(EditEvent ((id series-e2e) (calendar_key personal) (file %S) \
               (source_fingerprint %S) (start (Set ((kind Date) (value \
               today))))))"
              file fingerprint );
          ( "edit-utc-no-seconds",
            Printf.sprintf
              "(EditEvent ((id series-e2e) (calendar_key personal) (file %S) \
               (source_fingerprint %S) (start (Set ((kind Utc) (value \
               2026-07-20T10:00))))))"
              file fingerprint );
          ( "edit-floating-relative",
            Printf.sprintf
              "(EditEvent ((id series-e2e) (calendar_key personal) (file %S) \
               (source_fingerprint %S) (start (Set ((kind Floating) (value \
               tomorrowT10:00:00))))))"
              file fingerprint );
          ( "edit-tzid-sentinel",
            Printf.sprintf
              "(EditEvent ((id series-e2e) (calendar_key personal) (file %S) \
               (source_fingerprint %S) (start (Set ((kind (Tzid FLOATING)) \
               (value 2026-07-20T10:00:00))))))"
              file fingerprint );
        ]
      in
      let alarm_edit alarm =
        Printf.sprintf
          "(EditEvent ((id series-e2e) (calendar_key personal) (file %S) \
           (source_fingerprint %S) (alarms (Set (%s)))))"
          file fingerprint alarm
      in
      let invalid_alarm_requests =
        [
          ( "alarm-binary",
            alarm_edit
              "((action Audio) (trigger (Relative ((seconds -60) (related \
               Start)))) (attachment (Binary \"AA=!\")))" );
          ( "alarm-uri",
            alarm_edit
              "((action Audio) (trigger (Relative ((seconds -60) (related \
               Start)))) (attachment (Uri \"relative/path\")))" );
          ( "alarm-uri-value-kind",
            alarm_edit
              "((action Audio) (trigger (Relative ((seconds -60) (related \
               Start)))) (attachment (Uri \"https://example.test/a\")) \
               (attachment_parameters (((name VALUE) (value TEXT)))))" );
          ( "alarm-uri-encoding",
            alarm_edit
              "((action Audio) (trigger (Relative ((seconds -60) (related \
               Start)))) (attachment (Uri \"https://example.test/a\")) \
               (attachment_parameters (((name ENCODING) (value BASE64)))))" );
          ( "alarm-binary-encoding",
            alarm_edit
              "((action Audio) (trigger (Relative ((seconds -60) (related \
               Start)))) (attachment (Binary \"TQ==\")) (attachment_parameters \
               (((name ENCODING) (value 8BIT)))))" );
          ( "alarm-parameter-smuggling",
            alarm_edit
              (Printf.sprintf
                 "((action Audio) (trigger (Relative ((seconds -60) (related \
                  Start)))) (attachment (Binary \"TQ==\")) \
                  (attachment_parameters (((name VALUE) (value %S)))))"
                 "BINARY;ENCODING=BASE64") );
          ( "alarm-parameter-control",
            alarm_edit
              (Printf.sprintf
                 "((action Audio) (trigger (Relative ((seconds -60) (related \
                  Start)))) (trigger_parameters (((name X-LABEL) (value \
                  %S)))))"
                 "safe\001bad") );
          ( "alarm-audio-summary",
            alarm_edit
              "((action Audio) (trigger (Relative ((seconds -60) (related \
               Start)))) (summary forbidden))" );
          ( "alarm-display-summary",
            alarm_edit
              "((action Display) (trigger (Relative ((seconds -60) (related \
               Start)))) (summary forbidden) (description Reminder))" );
          ( "alarm-none-summary",
            alarm_edit
              "((action None_action) (trigger (Relative ((seconds -60) \
               (related Start)))) (summary forbidden))" );
          ( "alarm-attendee-aliases",
            alarm_edit
              "((action Email) (trigger (Relative ((seconds -60) (related \
               Start)))) (summary Invite) (description Body) (attendees \
               (\"mailto:legacy@example.test\")) (attendee_values (((uri \
               \"mailto:structured@example.test\"))))))" );
          ( "alarm-trigger-encoding",
            alarm_edit
              "((action Audio) (trigger (Relative ((seconds -60) (related \
               Start)))) (trigger_parameters (((name ENCODING) (value \
               BASE64)))))" );
          ( "alarm-description-related",
            alarm_edit
              "((action Display) (trigger (Relative ((seconds -60) (related \
               Start)))) (description Reminder) (description_parameters \
               (((name RELATED) (value END)))))" );
          ( "alarm-summary-encoding",
            alarm_edit
              "((action Email) (trigger (Relative ((seconds -60) (related \
               Start)))) (summary Invite) (summary_parameters (((name \
               ENCODING) (value BASE64)))) (description Body) (attendees \
               (\"mailto:user@example.test\")))" );
          ( "alarm-attendee-related",
            alarm_edit
              "((action Email) (trigger (Relative ((seconds -60) (related \
               Start)))) (summary Invite) (description Body) (attendee_values \
               (((uri \"mailto:user@example.test\") (parameters (((name \
               RELATED) (value END))))))))" );
          ( "alarm-fractional-absolute",
            alarm_edit
              "((action Audio) (trigger (Absolute \"2026-08-03T09:30:45.5Z\")))"
          );
          ( "alarm-attendee",
            alarm_edit
              "((action Email) (trigger (Relative ((seconds -60) (related \
               Start)))) (summary Invite) (description Body) (attendees \
               (\"user@example.com\")))" );
          ( "alarm-iana-name",
            alarm_edit
              "((action Audio) (trigger (Relative ((seconds -60) (related \
               Start)))) (other ((Iana ((name \"BAD:NAME\") (value safe))))))"
          );
          ( "alarm-iana-collision",
            alarm_edit
              "((action Audio) (trigger (Relative ((seconds -60) (related \
               Start)))) (other ((Iana ((name ACTION) (value shadow))))))" );
          ( "alarm-x-namespace",
            alarm_edit
              "((action Audio) (trigger (Relative ((seconds -60) (related \
               Start)))) (other ((X ((namespace \"BAD SPACE\") (name ID) \
               (value safe))))))" );
          ( "alarm-x-value",
            alarm_edit
              (Printf.sprintf
                 "((action Audio) (trigger (Relative ((seconds -60) (related \
                  Start)))) (other ((X ((namespace TRACE) (name ID) (value \
                  %S))))))"
                 "one\r\ntwo") );
        ]
      in
      let mutation_results =
        List.map
          (fun (id, payload) ->
            send output (request ~id payload);
            let response = receive () in
            let code = error_code response in
            Printf.sprintf "%s:%s:%s%s" id
              (Option.value ~default:"missing" (response_request_id response))
              code
              (if code = "internal_error" then
                 "[" ^ error_message response ^ "]"
               else ""))
          (requests @ invalid_alarm_requests)
      in
      let after_rejected_mutations =
        In_channel.with_open_bin file In_channel.input_all
      in
      send output
        (request ~id:"tzid-utc"
           "(CreateEvent ((calendar personal) (summary tzid-utc) (start ((kind \
            (Tzid UTC)) (value 2026-07-20T10:00:00)))))");
      let tzid_utc = receive () |> first_event |> Option.get in
      let tzid_utc_preserved =
        Option.bind (field "start_value" tzid_utc) (field "kind")
        = Some (Sexplib.Sexp.Atom "tzid")
        && Option.bind (field "start_value" tzid_utc) (field "tzid")
           = Some (Sexplib.Sexp.Atom "UTC")
      in
      send output
        (request ~id:"floating-gap"
           "(CreateEvent ((calendar personal) (summary floating-gap) (start \
            ((kind Floating) (value 2026-03-29T01:30:00)))))");
      let floating_gap = receive () |> first_event |> Option.get in
      let floating_gap_preserved =
        Option.bind (field "start_value" floating_gap) (field "kind")
        = Some (Sexplib.Sexp.Atom "floating")
      in
      output_string output
        "(Request ((version 1) (request_id utf8-safe) (request List";
      output_char output (Char.chr 0xff);
      output_string output "Calendars)))\n";
      flush output;
      let invalid_utf8 = receive () in
      send output (request ~id:"extra-close" "ListCalendars" ^ ")");
      let malformed_sexp = receive () in
      send output (String.make 1_000_001 'x');
      let overlong_unknown = receive () in
      let correlated_prefix =
        "(Request ((version 1) (request_id overlong-safe) (request "
      in
      send output
        (correlated_prefix
        ^ String.make (1_000_001 - String.length correlated_prefix) 'x');
      let overlong_correlated = receive () in
      send output
        (request ~id:(String.make 257 'h') "(CreateEvent ((calendar personal)))");
      let oversized_id = receive () in
      send output (request ~id:"control\001id" "ListCalendars");
      let control_id = receive () in
      send output (request ~id:"after-invalid-input" "ListCalendars");
      let recovered = receive () in
      Printf.printf "rejected-mutations=%s\n"
        (String.concat "," mutation_results);
      Printf.printf "mutation-bytes-preserved=%b series-still-present=%b\n"
        (String.equal initial_calendar after_rejected_mutations)
        (Sys.file_exists file);
      Printf.printf "strict-time-kinds=tzid-utc:%b floating-gap:%b\n"
        tzid_utc_preserved floating_gap_preserved;
      Printf.printf
        "invalid-utf8=%s:%s malformed=%s:%s overlong=%s:%s correlated=%s:%s \
         recovered=%s\n"
        (Option.value ~default:"missing" (response_request_id invalid_utf8))
        (error_code invalid_utf8)
        (Option.value ~default:"missing" (response_request_id malformed_sexp))
        (error_code malformed_sexp)
        (Option.value ~default:"missing" (response_request_id overlong_unknown))
        (error_code overlong_unknown)
        (Option.value ~default:"missing"
           (response_request_id overlong_correlated))
        (error_code overlong_correlated)
        (Option.value ~default:"missing" (response_request_id recovered));
      Printf.printf "invalid-ids=oversized:%s:%s control:%s:%s\n"
        (Option.value ~default:"missing" (response_request_id oversized_id))
        (error_code oversized_id)
        (Option.value ~default:"missing" (response_request_id control_id))
        (error_code control_id);
      Printf.printf "stdout-strict-utf8=%b\n"
        (List.for_all Fun.id !utf8_responses);
      close_out output;
      let diagnostics = In_channel.input_all errors in
      let status = Unix.close_process_full (input, output, errors) in
      Printf.printf "server-exit=%b diagnostics-empty=%b\n"
        (status = Unix.WEXITED 0)
        (String.trim diagnostics = ""));
  [%expect
    {|
    rejected-mutations=edit-zone-only:edit-zone-only:invalid_request,edit-start-only:edit-start-only:invalid_request,delete-zone-only:delete-zone-only:invalid_request,delete-start-only:delete-start-only:invalid_request,edit-date-relative:edit-date-relative:invalid_request,edit-utc-no-seconds:edit-utc-no-seconds:invalid_request,edit-floating-relative:edit-floating-relative:invalid_request,edit-tzid-sentinel:edit-tzid-sentinel:invalid_request,alarm-binary:alarm-binary:invalid_request,alarm-uri:alarm-uri:invalid_request,alarm-uri-value-kind:alarm-uri-value-kind:invalid_request,alarm-uri-encoding:alarm-uri-encoding:invalid_request,alarm-binary-encoding:alarm-binary-encoding:invalid_request,alarm-parameter-smuggling:alarm-parameter-smuggling:invalid_request,alarm-parameter-control:alarm-parameter-control:invalid_request,alarm-audio-summary:alarm-audio-summary:invalid_request,alarm-display-summary:alarm-display-summary:invalid_request,alarm-none-summary:alarm-none-summary:invalid_request,alarm-attendee-aliases:alarm-attendee-aliases:invalid_request,alarm-trigger-encoding:alarm-trigger-encoding:invalid_request,alarm-description-related:alarm-description-related:invalid_request,alarm-summary-encoding:alarm-summary-encoding:invalid_request,alarm-attendee-related:alarm-attendee-related:invalid_request,alarm-fractional-absolute:alarm-fractional-absolute:invalid_request,alarm-attendee:alarm-attendee:invalid_request,alarm-iana-name:alarm-iana-name:invalid_request,alarm-iana-collision:alarm-iana-collision:invalid_request,alarm-x-namespace:alarm-x-namespace:invalid_request,alarm-x-value:alarm-x-value:invalid_request
    mutation-bytes-preserved=true series-still-present=true
    strict-time-kinds=tzid-utc:true floating-gap:true
    invalid-utf8=utf8-safe:invalid_request malformed=extra-close:invalid_request overlong=unknown:invalid_request correlated=overlong-safe:invalid_request recovered=after-invalid-input
    invalid-ids=oversized:unknown:invalid_request control:unknown:invalid_request
    stdout-strict-utf8=true
    server-exit=true diagnostics-empty=true
    |}]

let%expect_test
    "query occurrence identity controls floating and DATE mutations \
     independent of process TZ" =
  let root = Filename.temp_file "caledonia-occurrence-e2e-" "" in
  Sys.remove root;
  Sys.mkdir root 0o700;
  Fun.protect
    ~finally:(fun () -> remove_tree root)
    (fun () ->
      let calendar = Filename.concat root "personal" in
      Sys.mkdir calendar 0o700;
      let file = Filename.concat calendar "occurrences.ics" in
      Out_channel.with_open_bin file (fun channel ->
          output_string channel occurrence_calendar);
      let environment =
        Array.append
          [| "CALENDAR_DIR=" ^ root; "TZ=UTC" |]
          (Unix.environment () |> Array.to_list
          |> List.filter (fun value ->
              not
                (String.starts_with ~prefix:"CALENDAR_DIR=" value
                || String.starts_with ~prefix:"TZ=" value))
          |> Array.of_list)
      in
      let executable = server_executable () in
      let input, output, errors =
        Unix.open_process_args_full executable [| executable; "server" |]
          environment
      in
      send output (request ~id:"hello-occurrences" "Handshake");
      ignore (read_response input);
      let query id =
        let value : Sexp.query_request =
          {
            from = Some "2026-07-16";
            to_ = "2026-07-16";
            timezone = Some "Europe/London";
            calendars = [ "personal" ];
            text = None;
            search_in = [];
            categories = [];
            id = Some id;
            statuses = [];
            overdue = None;
            has_alarm = None;
            recurring = None;
            limit = None;
          }
        in
        let payload =
          Sexp.sexp_of_request (Query value) |> Sexplib.Sexp.to_string
        in
        send output (request ~id:("query-" ^ id) payload);
        read_response input
      in
      let find_occurrence id =
        let response = query id in
        match event_with_id id response with
        | Some event -> event
        | None ->
            failwith
              (Printf.sprintf "query %s response: %s" id
                 (Sexplib.Sexp.to_string_hum response))
      in
      let floating = find_occurrence "floating-e2e" in
      let date = find_occurrence "date-e2e" in
      let master_start event =
        Option.bind (field "series_master" event) (field "start_value")
        |> fun value ->
        Option.bind value (field "value") |> fun value ->
        Option.bind value atom |> Option.value ~default:"missing"
      in
      Printf.printf "floating local=%s occurrence=%s zone=%s rid=%s master=%s\n"
        (atom_field "start_local" floating)
        (atom_field "occurrence_start" floating)
        (atom_field "occurrence_timezone" floating)
        ( Option.bind (field "recurrence_id_value" floating) (field "value")
        |> fun value ->
          Option.bind value atom |> Option.value ~default:"missing" )
        (master_start floating);
      Printf.printf "date local=%s occurrence=%s zone=%s rid=%s master=%s\n"
        (atom_field "start_local" date)
        (atom_field "occurrence_start" date)
        (atom_field "occurrence_timezone" date)
        ( Option.bind (field "recurrence_id_value" date) (field "value")
        |> fun value ->
          Option.bind value atom |> Option.value ~default:"missing" )
        (master_start date);
      let floating_occurrence = atom_field "occurrence_start" floating in
      let fingerprint = atom_field "source_fingerprint" floating in
      send output
        (request ~id:"edit-floating"
           (Printf.sprintf
              "(EditEvent ((id floating-e2e) (calendar_key personal) (file %S) \
               (source_fingerprint %S) (summary (Set \"Edited floating\")) \
               (occurrence_start %S) (occurrence_timezone Europe/London)))"
              file fingerprint floating_occurrence));
      let edited = read_response input in
      let edited_fingerprint =
        Option.get (first_event edited) |> atom_field "source_fingerprint"
      in
      let edited_source = In_channel.with_open_bin file In_channel.input_all in
      Printf.printf "floating edit-local=%b summary=%b\n"
        (has_line ~prefix:"RECURRENCE-ID:20260716T090000" edited_source)
        (has_line ~prefix:"SUMMARY:Edited floating" edited_source);
      send output
        (request ~id:"delete-floating"
           (Printf.sprintf
              "(DeleteEvent ((id floating-e2e) (calendar_key personal) (file \
               %S) (source_fingerprint %S) (occurrence_start %S) \
               (occurrence_timezone Europe/London)))"
              file edited_fingerprint floating_occurrence));
      ignore (read_response input);
      let after_floating = In_channel.with_open_bin file In_channel.input_all in
      Printf.printf "floating delete-local=%b override-gone=%b\n"
        (has_line ~prefix:"EXDATE:20260716T090000" after_floating)
        (not (has_line ~prefix:"RECURRENCE-ID:20260716T090000" after_floating));
      let date = find_occurrence "date-e2e" in
      let date_occurrence = atom_field "occurrence_start" date in
      let fingerprint = atom_field "source_fingerprint" date in
      send output
        (request ~id:"delete-date"
           (Printf.sprintf
              "(DeleteEvent ((id date-e2e) (calendar_key personal) (file %S) \
               (source_fingerprint %S) (occurrence_start %S) \
               (occurrence_timezone Europe/London)))"
              file fingerprint date_occurrence));
      ignore (read_response input);
      let after_date = In_channel.with_open_bin file In_channel.input_all in
      Printf.printf "date delete-local=%b\n"
        (has_line ~prefix:"EXDATE;VALUE=DATE:20260716" after_date);
      close_out output;
      let diagnostics = In_channel.input_all errors in
      let status = Unix.close_process_full (input, output, errors) in
      Printf.printf "server-exit=%b diagnostics-empty=%b\n"
        (status = Unix.WEXITED 0)
        (String.trim diagnostics = ""));
  [%expect
    {|
    floating local=2026-07-16T09:00:00 occurrence=2026-07-16T08:00:00Z zone=Europe/London rid=2026-07-16T09:00:00 master=2026-07-15T09:00:00
    date local=2026-07-16T00:00:00 occurrence=2026-07-15T23:00:00Z zone=Europe/London rid=2026-07-16 master=2026-07-15
    floating edit-local=true summary=true
    floating delete-local=true override-gone=true
    date delete-local=true
    server-exit=true diagnostics-empty=true
    |}]

let%expect_test
    "server bounds oversized responses and keeps the connection usable" =
  let root = Filename.temp_file "caledonia-response-bound-e2e-" "" in
  Sys.remove root;
  Sys.mkdir root 0o700;
  Fun.protect
    ~finally:(fun () -> remove_tree root)
    (fun () ->
      let calendar = Filename.concat root "personal" in
      Sys.mkdir calendar 0o700;
      let file = Filename.concat calendar "large.ics" in
      let source =
        String.concat "\r\n"
          [
            "BEGIN:VCALENDAR";
            "VERSION:2.0";
            "PRODID:-//Caledonia response bound test//EN";
            "BEGIN:VEVENT";
            "UID:oversized-response";
            "DTSTAMP:20260701T000000Z";
            "DTSTART:20260715T090000Z";
            "SUMMARY:Large response";
            "DESCRIPTION:" ^ String.make 1_100_000 'x';
            "END:VEVENT";
            "END:VCALENDAR";
            "";
          ]
      in
      Out_channel.with_open_bin file (fun channel ->
          output_string channel source);
      let environment =
        Array.append
          [| "CALENDAR_DIR=" ^ root; "TZ=UTC" |]
          (Unix.environment () |> Array.to_list
          |> List.filter (fun value ->
              not
                (String.starts_with ~prefix:"CALENDAR_DIR=" value
                || String.starts_with ~prefix:"TZ=" value))
          |> Array.of_list)
      in
      let executable = server_executable () in
      let input, output, errors =
        Unix.open_process_args_full executable [| executable; "server" |]
          environment
      in
      send output (request ~id:"hello-response-bound" "Handshake");
      ignore (read_response input);
      let query : Sexp.query_request =
        {
          from = Some "2026-07-15";
          to_ = "2026-07-15";
          timezone = Some "UTC";
          calendars = [ "personal" ];
          text = None;
          search_in = [];
          categories = [];
          id = Some "oversized-response";
          statuses = [];
          overdue = None;
          has_alarm = None;
          recurring = None;
          limit = None;
        }
      in
      let payload =
        Sexp.sexp_of_request (Query query) |> Sexplib.Sexp.to_string
      in
      send output (request ~id:"oversized-query" payload);
      let line, oversized = read_response_line input in
      Sys.remove file;
      send output (request ~id:"after-oversized-response" "ListCalendars");
      let recovered = read_response input in
      Printf.printf "bounded=%b id=%s code=%s recovered=%s\n"
        (String.length line <= 1_000_000)
        (Option.value ~default:"missing" (response_request_id oversized))
        ( error_record oversized |> fun value ->
          Option.bind value (field "code") |> fun value ->
          Option.bind value atom |> Option.value ~default:"missing" )
        (Option.value ~default:"missing" (response_request_id recovered));
      close_out output;
      let diagnostics = In_channel.input_all errors in
      let status = Unix.close_process_full (input, output, errors) in
      Printf.printf "server-exit=%b diagnostics-empty=%b\n"
        (status = Unix.WEXITED 0)
        (String.trim diagnostics = ""));
  [%expect
    {|
    bounded=true id=oversized-query code=response_too_large recovered=after-oversized-response
    server-exit=true diagnostics-empty=true
    |}]

let%expect_test "event query rejects unavailable and unknown filters" =
  let root = Filename.temp_file "caledonia-server-query-validation-" "" in
  Sys.remove root;
  Sys.mkdir root 0o700;
  Fun.protect
    ~finally:(fun () -> remove_tree root)
    (fun () ->
      Sys.mkdir (Filename.concat root "personal") 0o700;
      let environment =
        Array.append
          [| "CALENDAR_DIR=" ^ root; "TZ=UTC" |]
          (Unix.environment () |> Array.to_list
          |> List.filter (fun value ->
              not
                (String.starts_with ~prefix:"CALENDAR_DIR=" value
                || String.starts_with ~prefix:"TZ=" value))
          |> Array.of_list)
      in
      let executable = server_executable () in
      let input, output, errors =
        Unix.open_process_args_full executable [| executable; "server" |]
          environment
      in
      send output (request ~id:"query-validation-hello" "Handshake");
      ignore (read_response input);
      let query ~id ~statuses ~overdue =
        let value : Sexp.query_request =
          {
            from = Some "2026-07-15";
            to_ = "2026-07-15";
            timezone = Some "UTC";
            calendars = [ "personal" ];
            text = None;
            search_in = [];
            categories = [];
            id = None;
            statuses;
            overdue;
            has_alarm = None;
            recurring = None;
            limit = None;
          }
        in
        let payload =
          Sexp.sexp_of_request (Query value) |> Sexplib.Sexp.to_string
        in
        send output (request ~id payload);
        read_response input
      in
      let overdue =
        query ~id:"overdue-filter" ~statuses:[] ~overdue:(Some true)
      in
      let bogus =
        query ~id:"bogus-status" ~statuses:[ "BOGUS" ] ~overdue:None
      in
      let code response =
        let record = error_record response in
        let value = Option.bind record (field "code") in
        Option.bind value atom |> Option.value ~default:"missing"
      in
      Printf.printf "overdue=%s bogus=%s ids=%s/%s\n" (code overdue)
        (code bogus)
        (Option.value ~default:"missing" (response_request_id overdue))
        (Option.value ~default:"missing" (response_request_id bogus));
      close_out output;
      let diagnostics = In_channel.input_all errors in
      let status = Unix.close_process_full (input, output, errors) in
      Printf.printf "server-exit=%b diagnostics-empty=%b\n"
        (status = Unix.WEXITED 0)
        (String.trim diagnostics = ""));
  [%expect
    {|
    overdue=unsupported_capability bogus=invalid_request ids=overdue-filter/bogus-status
    server-exit=true diagnostics-empty=true |}]
