open Caledonia_lib

let calendar_dir_path = Filename.concat (Sys.getcwd ()) "calendar"

module Event = struct
  include Event

  let create = create ~now:Ptime.epoch
  let edit_patch = edit_patch ~now:Ptime.epoch

  let events_of_icalendar_result _calendar_key ~file calendar =
    let _ = file in
    List.filter_map
      (function `Event event -> Some event | _ -> None)
      (snd calendar)
    |> of_events_result

  let compute_alarm_fires ~floating_tz ~from ~to_ event =
    compute_alarm_fires_result ~floating_tz ~from ~to_ event |> Result.get_ok
end

module Todo = struct
  include Todo

  let create = create ~now:Ptime.epoch
  let edit = edit ~now:Ptime.epoch

  let compute_alarm_fires ~floating_tz ~from ~to_ todo =
    compute_alarm_fires_result ~floating_tz ~from ~to_ todo |> Result.get_ok
end

module Alarm_query = struct
  include Alarm_query

  let run ~floating_tz ~from ~to_ components =
    run_result ~floating_tz ~from ~to_ components |> Result.get_ok
end

let stored_component ~fs ~calendar_key body =
  let file = Eio.Path.(fs / (calendar_key ^ "-alarm-test.ics")) in
  let source =
    Component_source.of_decoded_document ~calendar_key ~file
      ~fingerprint:(Digest.string calendar_key |> Digest.to_hex)
      ()
  in
  Component.stored_views_of_decoded_components ~source
    (Component.ical_components_of_body body)
  |> Result.get_ok |> List.hd

let event_alarm_summary (fire : Event.alarm_owner Alarm.fire) =
  match fire.owner with
  | Event.Series series -> Event.get_summary series
  | Event.Occurrence occurrence -> Event.Occurrence.get_summary occurrence

let make_display_alarm seconds =
  let span = Ptime.Span.of_int_s (-seconds) in
  let open Icalendar in
  `Display
    {
      trigger = (Params.empty, `Duration span);
      duration_repeat = None;
      summary = None;
      other = [];
      special = { description = Some (Params.empty, "Reminder") };
    }

let make_none_alarm () =
  let open Icalendar in
  `None
    {
      trigger = (Params.empty, `Duration (Ptime.Span.of_int_s (-900)));
      duration_repeat = None;
      summary = None;
      other = [];
      special = ();
    }

let make_none_absolute_alarm instant =
  let open Icalendar in
  let params = Params.empty |> Params.add Valuetype `Datetime in
  `None
    {
      trigger = (params, `Datetime instant);
      duration_repeat = None;
      summary = None;
      other = [];
      special = ();
    }

let make_relative_alarm ?(related = `Start) seconds =
  let open Icalendar in
  let params = Params.empty |> Params.add Related related in
  `Display
    {
      trigger = (params, `Duration (Ptime.Span.of_int_s seconds));
      duration_repeat = None;
      summary = None;
      other = [];
      special = { description = Some (Params.empty, "Reminder") };
    }

let make_absolute_alarm instant =
  let open Icalendar in
  let params = Params.empty |> Params.add Valuetype `Datetime in
  `Display
    {
      trigger = (params, `Datetime instant);
      duration_repeat = None;
      summary = None;
      other = [];
      special = { description = Some (Params.empty, "Reminder") };
    }

let with_repeat ~every_seconds ~repeat = function
  | `Display (alarm : Icalendar.display_struct Icalendar.alarm_struct) ->
      `Display
        {
          alarm with
          duration_repeat =
            Some
              ( (Icalendar.Params.empty, Ptime.Span.of_int_s every_seconds),
                (Icalendar.Params.empty, repeat) );
        }
  | `None (alarm : unit Icalendar.alarm_struct) ->
      `None
        {
          alarm with
          duration_repeat =
            Some
              ( (Icalendar.Params.empty, Ptime.Span.of_int_s every_seconds),
                (Icalendar.Params.empty, repeat) );
        }
  | alarm -> alarm

let ptime_of ymd hms = Option.get @@ Ptime.of_date_time (ymd, (hms, 0))

let format_ptime t =
  let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" y m d hh mm ss

let validation_result = function
  | Ok () -> "accepted"
  | Error (`Msg _) -> "rejected"

let%expect_test "alarm protocol binary attachment validation is strict" =
  [
    ("one-byte", "TQ==");
    ("two-byte", "TWE=");
    ("three-byte", "TWFu");
    ("binary", "AAEC/w==");
    ("empty", "");
    ("bad-alphabet", "AAE!Bw==");
    ("embedded-padding", "AA=CBw==");
    ("all-padding", "====");
    ("missing-padding", "TQ");
    ("too-much-padding", "TQ===");
    ("noncanonical-tail", "TR==");
  ]
  |> List.iter (fun (label, value) ->
      Printf.printf "%s: %s\n" label
        (validation_result (Alarm.validate_binary_attachment value)));
  let oversized = String.make (Alarm.max_binary_attachment_bytes + 4) 'A' in
  Printf.printf "oversized: %s\n"
    (validation_result (Alarm.validate_binary_attachment oversized));
  [%expect
    {|
    one-byte: accepted
    two-byte: accepted
    three-byte: accepted
    binary: accepted
    empty: rejected
    bad-alphabet: rejected
    embedded-padding: rejected
    all-padding: rejected
    missing-padding: rejected
    too-much-padding: rejected
    noncanonical-tail: rejected
    oversized: rejected
    |}]

let%expect_test "alarm protocol URI and raw extension validation" =
  let oversized_uri = "x:" ^ String.make Alarm.max_uri_bytes 'a' in
  [
    ("mailto", "mailto:user@example.com");
    ("https", "https://example.test/a%20b?x=1");
    ("relative", "/local/attachment");
    ("space", "https://example.test/a b");
    ("bad-percent", "https://example.test/%zz");
    ("newline", "mailto:user@example.com\nATTACK");
    ("bad-scheme", "1http://example.test");
    ("oversized", oversized_uri);
  ]
  |> List.iter (fun (label, value) ->
      Printf.printf "uri-%s: %s\n" label
        (validation_result (Alarm.validate_uri ~field:"test URI" value)));
  [
    ("valid", "alarm-42");
    ("tab", "alarm\t42");
    ("crlf", "one\r\ntwo");
    ("nul", "one\000two");
    ("escape", "one\027two");
    ("invalid-utf8", String.make 1 (Char.chr 0xff));
  ]
  |> List.iter (fun (label, value) ->
      Printf.printf "raw-%s: %s\n" label
        (validation_result
           (Alarm.validate_content_line_value ~field:"test property" value)));
  Printf.printf "raw-oversized: %s\n"
    (validation_result
       (Alarm.validate_content_line_value ~field:"test property"
          (String.make (Alarm.max_content_line_value_bytes + 1) 'a')));
  [
    ("valid", "TRACE-ID");
    ("empty", "");
    ("colon", "TRACE:ID");
    ("oversized", String.make 129 'A');
  ]
  |> List.iter (fun (label, value) ->
      Printf.printf "token-%s: %s\n" label
        (validation_result (Alarm.validate_token ~field:"test token" value)));
  [%expect
    {|
    uri-mailto: accepted
    uri-https: accepted
    uri-relative: rejected
    uri-space: rejected
    uri-bad-percent: rejected
    uri-newline: rejected
    uri-bad-scheme: rejected
    uri-oversized: rejected
    raw-valid: accepted
    raw-tab: accepted
    raw-crlf: rejected
    raw-nul: rejected
    raw-escape: rejected
    raw-invalid-utf8: rejected
    raw-oversized: rejected
    token-valid: accepted
    token-empty: rejected
    token-colon: rejected
    token-oversized: rejected
    |}]

let%expect_test "typed alarm validation cannot bypass protocol constraints" =
  let open Icalendar in
  let trigger =
    ( Params.empty |> Params.add Valuetype `Duration,
      `Duration (Ptime.Span.of_int_s (-60)) )
  in
  let attachment_params =
    Params.empty |> Params.add Valuetype `Binary |> Params.add Encoding `Base64
  in
  let audio ?summary ?attach () : Icalendar.alarm =
    `Audio
      {
        trigger;
        duration_repeat = None;
        summary;
        other = [];
        special = { attach };
      }
  in
  let display_with_summary : Icalendar.alarm =
    `Display
      {
        trigger;
        duration_repeat = None;
        summary = Some (Params.empty, "forbidden");
        other = [];
        special = { description = Some (Params.empty, "Reminder") };
      }
  in
  let email_with_relative_attendee : Icalendar.alarm =
    `Email
      {
        trigger;
        duration_repeat = None;
        summary = Some (Params.empty, "Required summary");
        other = [];
        special =
          {
            description = (Params.empty, "Body");
            attendees = [ (Params.empty, Uri.of_string "relative-address") ];
            attach = None;
          };
      }
  in
  let with_trigger_params params : Icalendar.alarm =
    `Audio
      {
        trigger = (params, `Duration (Ptime.Span.of_int_s (-60)));
        duration_repeat = None;
        summary = None;
        other = [];
        special = { attach = None };
      }
  in
  let with_trigger_span span : Icalendar.alarm =
    `Audio
      {
        trigger = (Params.empty, `Duration span);
        duration_repeat = None;
        summary = None;
        other = [];
        special = { attach = None };
      }
  in
  let fractional_span = Option.get (Ptime.Span.of_float_s 0.5) in
  let overflow_span = Option.get (Ptime.Span.of_d_ps (max_int, 0L)) in
  let fractional_absolute : Icalendar.alarm =
    let timestamp =
      match Ptime.of_rfc3339 "2026-08-03T09:30:45.5Z" with
      | Ok (timestamp, _, _) -> timestamp
      | Error _ -> assert false
    in
    `Audio
      {
        trigger =
          (Params.empty |> Params.add Valuetype `Datetime, `Datetime timestamp);
        duration_repeat = None;
        summary = None;
        other = [];
        special = { attach = None };
      }
  in
  let display_with_description_params params : Icalendar.alarm =
    `Display
      {
        trigger;
        duration_repeat = None;
        summary = None;
        other = [];
        special = { description = Some (params, "Reminder") };
      }
  in
  let email_with_params ~summary_params ~attendee_params : Icalendar.alarm =
    `Email
      {
        trigger;
        duration_repeat = None;
        summary = Some (summary_params, "Required summary");
        other = [];
        special =
          {
            description = (Params.empty, "Body");
            attendees =
              [ (attendee_params, Uri.of_string "mailto:user@example.test") ];
            attach = None;
          };
      }
  in
  let bad_other =
    audio () |> function
    | `Audio alarm ->
        `Audio
          {
            alarm with
            other = [ `Xprop (("BAD SPACE", "ID"), Params.empty, "safe") ];
          }
    | _ -> assert false
  in
  let injected_params =
    Params.empty
    |> Params.add (Iana_param "X-SAFE") [ `String "safe;ENCODING=BASE64" ]
  in
  [
    ("valid-binary", audio ~attach:(attachment_params, `Binary "TQ==") ());
    ("bad-binary", audio ~attach:(attachment_params, `Binary "not base64") ());
    ("binary-missing-params", audio ~attach:(Params.empty, `Binary "TQ==") ());
    ( "relative-uri",
      audio ~attach:(Params.empty, `Uri (Uri.of_string "relative/path")) () );
    ("audio-summary", audio ~summary:(Params.empty, "forbidden") ());
    ("display-summary", display_with_summary);
    ("relative-attendee", email_with_relative_attendee);
    ("bad-extension", bad_other);
    ( "trigger-unrelated-encoding",
      with_trigger_params (Params.empty |> Params.add Encoding `Base64) );
    ( "description-unrelated-related",
      display_with_description_params (Params.empty |> Params.add Related `End)
    );
    ( "summary-unrelated-encoding",
      email_with_params
        ~summary_params:(Params.empty |> Params.add Encoding `Base64)
        ~attendee_params:Params.empty );
    ( "attendee-unrelated-related",
      email_with_params ~summary_params:Params.empty
        ~attendee_params:(Params.empty |> Params.add Related `End) );
    ( "duration-standard-parameter",
      `Audio
        {
          trigger;
          duration_repeat =
            Some
              ( ( Params.empty |> Params.add Language "en",
                  Ptime.Span.of_int_s 30 ),
                (Params.empty, 1) );
          summary = None;
          other = [];
          special = { attach = None };
        } );
    ( "duration-extensions-allowed",
      `Audio
        {
          trigger;
          duration_repeat =
            Some
              ( ( Params.empty
                  |> Params.add (Xparam ("CAL", "DURATION")) [ `String "safe" ],
                  Ptime.Span.of_int_s 30 ),
                ( Params.empty
                  |> Params.add (Xparam ("CAL", "REPEAT")) [ `String "safe" ],
                  1 ) );
          summary = None;
          other = [];
          special = { attach = None };
        } );
    ( "trigger-extension-allowed",
      with_trigger_params
        (Params.empty |> Params.add (Xparam ("CAL", "TRACE")) [ `String "safe" ])
    );
    ("fractional-absolute", fractional_absolute);
    ("fractional-relative", with_trigger_span fractional_span);
    ("overflow-relative", with_trigger_span overflow_span);
    ( "fractional-repeat-duration",
      `Audio
        {
          trigger;
          duration_repeat =
            Some ((Params.empty, fractional_span), (Params.empty, 1));
          summary = None;
          other = [];
          special = { attach = None };
        } );
  ]
  |> List.iter (fun (label, alarm) ->
      Printf.printf "%s: %s\n" label (validation_result (Alarm.validate alarm)));
  Printf.printf "typed-parameter-injection: %s\n"
    (validation_result (Alarm.validate_params injected_params));
  [%expect
    {|
    valid-binary: accepted
    bad-binary: rejected
    binary-missing-params: rejected
    relative-uri: rejected
    audio-summary: rejected
    display-summary: rejected
    relative-attendee: rejected
    bad-extension: rejected
    trigger-unrelated-encoding: rejected
    description-unrelated-related: rejected
    summary-unrelated-encoding: rejected
    attendee-unrelated-related: rejected
    duration-standard-parameter: rejected
    duration-extensions-allowed: accepted
    trigger-extension-allowed: accepted
    fractional-absolute: rejected
    fractional-relative: rejected
    overflow-relative: rejected
    fractional-repeat-duration: rejected
    typed-parameter-injection: rejected
    |}]

let%expect_test "event create and load apply typed alarm validation" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let start = ptime_of (2026, 7, 15) (10, 0, 0) in
  let trigger =
    (Icalendar.Params.empty, `Duration (Ptime.Span.of_int_s (-60)))
  in
  let invalid_audio : Icalendar.alarm =
    `Audio
      {
        trigger;
        duration_repeat = None;
        summary = Some (Icalendar.Params.empty, "forbidden");
        other = [];
        special = { attach = None };
      }
  in
  let created =
    Event.create ~summary:"Direct invalid alarm"
      ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
      ~alarms:[ invalid_audio ] ()
  in
  let fractional_alarm : Icalendar.alarm =
    `Audio
      {
        trigger =
          ( Icalendar.Params.empty,
            `Duration (Option.get (Ptime.Span.of_float_s (-0.5))) );
        duration_repeat = None;
        summary = None;
        other = [];
        special = { attach = None };
      }
  in
  let fractional_created =
    Event.create ~summary:"Fractional alarm"
      ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
      ~alarms:[ fractional_alarm ] ()
  in
  let source alarm_lines =
    String.concat "\r\n"
      ([
         "BEGIN:VCALENDAR";
         "VERSION:2.0";
         "PRODID:-//Caledonia alarm validation test//EN";
         "BEGIN:VEVENT";
         "UID:invalid-audio";
         "DTSTAMP:20260715T090000Z";
         "DTSTART:20260715T100000Z";
         "BEGIN:VALARM";
       ]
      @ alarm_lines
      @ [ "END:VALARM"; "END:VEVENT"; "END:VCALENDAR"; "" ])
  in
  let load source =
    match Calendar_codec.Legacy.parse source with
    | Error _ -> (false, Error (`Msg "parser rejected malformed alarm"))
    | Ok calendar ->
        ( true,
          Event.events_of_icalendar_result "alarm"
            ~file:Eio.Path.(fs / "invalid-alarm.ics")
            calendar )
  in
  let parser_retained_summary, loaded_summary =
    load (source [ "ACTION:AUDIO"; "TRIGGER:-PT1M"; "SUMMARY:forbidden" ])
  in
  let parser_retained_binary, loaded_binary =
    load
      (source
         [
           "ACTION:AUDIO";
           "TRIGGER:-PT1M";
           "ATTACH;VALUE=BINARY;ENCODING=BASE64:TR==";
         ])
  in
  let parser_retained_wrong_param, loaded_wrong_param =
    load
      (source
         [
           "ACTION:DISPLAY"; "TRIGGER:-PT1M"; "DESCRIPTION;RELATED=END:Reminder";
         ])
  in
  Printf.printf "direct create rejected: %b\n" (Result.is_error created);
  Printf.printf "fractional direct create rejected: %b\n"
    (Result.is_error fractional_created);
  Printf.printf "parser retained malformed summary/binary/parameter: %b/%b/%b\n"
    parser_retained_summary parser_retained_binary parser_retained_wrong_param;
  Printf.printf "loaded summary/binary/parameter rejected: %b/%b/%b\n"
    (Result.is_error loaded_summary)
    (Result.is_error loaded_binary)
    (Result.is_error loaded_wrong_param);
  [%expect
    {|
    direct create rejected: true
    fractional direct create rejected: true
    parser retained malformed summary/binary/parameter: true/true/true
    loaded summary/binary/parameter rejected: true/true/true
    |}]

(* --- Format function tests --- *)

let%expect_test "format_alarm_trigger" =
  let test span_secs =
    let span = Ptime.Span.of_int_s span_secs in
    Printf.printf "%d -> %s\n" span_secs
      (Format_utils.format_alarm_trigger span)
  in
  test 0;
  test (-900);
  (* 15 min *)
  test (-3600);
  (* 1 hour *)
  test (-86400);
  (* 1 day *)
  test (-95400);
  (* 1 day 2 hours 30 min = 86400 + 7200 + 1800 *)
  test (-90);
  test 900;
  (* 15 min after *)
  [%expect
    {|
    0 -> at start
    -900 -> 15 minutes before
    -3600 -> 1 hour before
    -86400 -> 1 day before
    -95400 -> 1 day 2 hours 30 minutes before
    -90 -> 1 minute 30 seconds before
    900 -> 15 minutes after |}]

let%expect_test "format_alarm_short" =
  let test span_secs =
    let span = Ptime.Span.of_int_s span_secs in
    Printf.printf "%d -> %s\n" span_secs (Format_utils.format_alarm_short span)
  in
  test 0;
  test (-900);
  test (-3600);
  test (-86400);
  test (-95400);
  test (-90);
  [%expect
    {|
    0 -> 0m
    -900 -> 15m
    -3600 -> 1h
    -86400 -> 1d
    -95400 -> 1d2h30m
    -90 -> 1m30s |}]

let%expect_test "format_alarms filters None alarms" =
  let alarms =
    [ make_display_alarm 900; make_none_alarm (); make_display_alarm 3600 ]
  in
  Printf.printf "long: %s\n" (Format_utils.format_alarms alarms);
  Printf.printf "short: %s\n" (Format_utils.format_alarms_short alarms);
  [%expect {|
    long: 15 minutes before, 1 hour before
    short: 15m,1h |}]

let%expect_test "ACTION:NONE retains its semantic trigger" =
  let relative = make_none_alarm () in
  let absolute_time = ptime_of (2025, 4, 15) (12, 0, 0) in
  let absolute = make_none_absolute_alarm absolute_time in
  let print_trigger = function
    | _, `Duration span ->
        Printf.printf "relative: %s\n" (Format_utils.format_alarm_trigger span)
    | _, `Datetime instant ->
        Printf.printf "absolute: %s\n" (format_ptime instant)
  in
  print_trigger (Alarm.trigger relative);
  print_trigger (Alarm.trigger absolute);
  [%expect
    {|
    relative: 15 minutes before
    absolute: 2025-04-15T12:00:00Z |}]

let%expect_test "format_alarms empty list" =
  Printf.printf "long: '%s'\n" (Format_utils.format_alarms []);
  Printf.printf "short: '%s'\n" (Format_utils.format_alarms_short []);
  [%expect {|
    long: ''
    short: '' |}]

(* --- Event alarm fire computation tests --- *)

let%expect_test "event alarm fires from ics file" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let components =
    Result.get_ok
    @@ Calendar_dir.get_calendar_components ~fs calendar_dir "alarm"
  in
  let events = List.filter_map Component.to_event components in
  let event =
    List.find (fun e -> Event.get_id e = "alarm-event@caledonia.test") events
  in
  let from = Some (ptime_of (2025, 4, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 4, 30) (23, 59, 59) in
  let fires =
    Event.compute_alarm_fires ~floating_tz:Timedesc.Time_zone.utc ~from ~to_
      event
  in
  Printf.printf "Number of alarm fires: %d\n" (List.length fires);
  List.iter
    (fun (af : Event.alarm_owner Alarm.fire) ->
      Printf.printf "  %s\n" (format_ptime af.fire_time))
    fires;
  [%expect
    {|
    Number of alarm fires: 2
      2025-04-15T09:00:00Z
      2025-04-15T09:45:00Z |}]

let%expect_test "event with no alarms returns empty fires" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let components =
    Result.get_ok
    @@ Calendar_dir.get_calendar_components ~fs calendar_dir "example"
  in
  let events = List.filter_map Component.to_event components in
  let event =
    List.find (fun e -> Event.get_id e = "test-event@caledonia.test") events
  in
  let from = Some (ptime_of (2025, 3, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 5, 1) (0, 0, 0) in
  let fires =
    Event.compute_alarm_fires ~floating_tz:Timedesc.Time_zone.utc ~from ~to_
      event
  in
  Printf.printf "Number of alarm fires: %d\n" (List.length fires);
  [%expect {| Number of alarm fires: 0 |}]

let%expect_test
    "ACTION:NONE participates in event, todo, and unified fire queries" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let event_start = ptime_of (2025, 4, 15) (10, 0, 0) in
  let todo_alarm_time = ptime_of (2025, 4, 15) (12, 0, 0) in
  let event =
    Result.get_ok
      (Event.create ~summary:"No-action event"
         ~start:(Icalendar.Params.empty, `Datetime (`Utc event_start))
         ~alarms:[ make_none_alarm () ]
         ())
  in
  let todo =
    Result.get_ok
      (Todo.create ~summary:"No-action todo"
         ~alarms:
           [
             make_none_absolute_alarm todo_alarm_time
             |> with_repeat ~every_seconds:20 ~repeat:2;
           ]
         ())
  in
  let from = Some (ptime_of (2025, 4, 15) (9, 0, 0)) in
  let to_ = ptime_of (2025, 4, 15) (12, 1, 0) in
  let event_fires =
    Event.compute_alarm_fires ~floating_tz:Timedesc.Time_zone.utc ~from ~to_
      event
  in
  let todo_fires =
    Todo.compute_alarm_fires ~floating_tz:Timedesc.Time_zone.utc ~from ~to_ todo
  in
  let unified =
    Alarm_query.run ~floating_tz:Timedesc.Time_zone.utc ~from ~to_
      [
        stored_component ~fs ~calendar_key:"event" (Component.event_body event);
        stored_component ~fs ~calendar_key:"todo" (Component.todo_body todo);
      ]
  in
  Printf.printf "event=%d todo=%d unified=%d\n" (List.length event_fires)
    (List.length todo_fires) (List.length unified);
  List.iter
    (fun (fire : Alarm_query.fire) ->
      match fire.alarm with
      | `None _ -> Printf.printf "none %s\n" (format_ptime fire.fire_time)
      | `Audio _ | `Display _ | `Email _ -> print_endline "unexpected action")
    unified;
  [%expect
    {|
    event=1 todo=3 unified=4
    none 2025-04-15T09:45:00Z
    none 2025-04-15T12:00:00Z
    none 2025-04-15T12:00:20Z
    none 2025-04-15T12:00:40Z |}]

let%expect_test "recurring event alarm fires" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let components =
    Result.get_ok
    @@ Calendar_dir.get_calendar_components ~fs calendar_dir "alarm"
  in
  let events = List.filter_map Component.to_event components in
  let event =
    List.find
      (fun e -> Event.get_id e = "alarm-recurring@caledonia.test")
      events
  in
  (* Query 4 weeks starting from the event's first occurrence *)
  let from = Some (ptime_of (2025, 3, 27) (0, 0, 0)) in
  let to_ = ptime_of (2025, 4, 24) (0, 0, 0) in
  let fires =
    Event.compute_alarm_fires ~floating_tz:Timedesc.Time_zone.utc ~from ~to_
      event
  in
  Printf.printf "Number of alarm fires: %d\n" (List.length fires);
  List.iter
    (fun (af : Event.alarm_owner Alarm.fire) ->
      Printf.printf "  %s\n" (format_ptime af.fire_time))
    fires;
  [%expect
    {|
    Number of alarm fires: 4
      2025-03-27T11:00:00Z
      2025-04-03T11:00:00Z
      2025-04-10T11:00:00Z
      2025-04-17T11:00:00Z |}]

let%expect_test "END-relative alarm uses event end and preserves seconds" =
  Eio_main.run @@ fun env ->
  let _ = env in
  let start = ptime_of (2025, 4, 15) (10, 0, 30) in
  let end_ = ptime_of (2025, 4, 15) (11, 0, 30) in
  let event =
    Result.get_ok
      (Event.create ~summary:"END alarm"
         ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
         ~end_:(`Dtend (Icalendar.Params.empty, `Datetime (`Utc end_)))
         ~alarms:[ make_relative_alarm ~related:`End (-90) ]
         ())
  in
  let from = Some (ptime_of (2025, 4, 15) (10, 58, 0)) in
  let to_ = ptime_of (2025, 4, 15) (11, 0, 0) in
  let fires =
    Event.compute_alarm_fires ~floating_tz:Timedesc.Time_zone.utc ~from ~to_
      event
  in
  List.iter
    (fun (fire : Event.alarm_owner Alarm.fire) ->
      print_endline (format_ptime fire.fire_time))
    fires;
  [%expect {| 2025-04-15T10:59:00Z |}]

let%expect_test "long-lead recurring alarm derives its expansion horizon" =
  Eio_main.run @@ fun env ->
  let _ = env in
  let start = ptime_of (2025, 4, 15) (12, 0, 0) in
  let recurrence = (`Weekly, Some (`Count 2), None, []) in
  let event =
    Result.get_ok
      (Event.create ~summary:"Long lead"
         ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
         ~recurrence
         ~alarms:[ make_relative_alarm (-10 * 86400) ]
         ())
  in
  let from = Some (ptime_of (2025, 4, 5) (0, 0, 0)) in
  let to_ = ptime_of (2025, 4, 6) (0, 0, 0) in
  let fires =
    Event.compute_alarm_fires ~floating_tz:Timedesc.Time_zone.utc ~from ~to_
      event
  in
  List.iter
    (fun (fire : Event.alarm_owner Alarm.fire) ->
      print_endline (format_ptime fire.fire_time))
    fires;
  [%expect {| 2025-04-05T12:00:00Z |}]

let%expect_test "sparse override end and inherited alarm extend the horizon" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Caledonia sparse alarm horizon test//EN";
        "BEGIN:VEVENT";
        "UID:sparse-horizon";
        "DTSTAMP:20250401T000000Z";
        "DTSTART:20250415T120000Z";
        "DTEND:20250415T130000Z";
        "RRULE:FREQ=WEEKLY;COUNT=2";
        "SUMMARY:Sparse horizon";
        "BEGIN:VALARM";
        "ACTION:DISPLAY";
        "TRIGGER;RELATED=END:-PT1H";
        "DESCRIPTION:Inherited end reminder";
        "END:VALARM";
        "END:VEVENT";
        "BEGIN:VEVENT";
        "UID:sparse-horizon";
        "RECURRENCE-ID:20250422T120000Z";
        "DTSTAMP:20250401T000000Z";
        "DTSTART:20250422T120000Z";
        "DTEND:20250430T120000Z";
        "END:VEVENT";
        "END:VCALENDAR";
        "";
      ]
  in
  let calendar = Result.get_ok (Calendar_codec.Legacy.parse source) in
  let file =
    Eio.Path.(fs / Filename.get_temp_dir_name () / "sparse-horizon.ics")
  in
  let event =
    Result.get_ok (Event.events_of_icalendar_result "alarm" ~file calendar)
    |> List.hd
  in
  let from = Some (ptime_of (2025, 4, 30) (10, 0, 0)) in
  let to_ = ptime_of (2025, 4, 30) (12, 0, 0) in
  let fires =
    Event.compute_alarm_fires_result ~max_instances:100
      ~floating_tz:Timedesc.Time_zone.utc ~from ~to_ event
    |> Result.get_ok
  in
  List.iter
    (fun (fire : Event.alarm_owner Alarm.fire) ->
      Printf.printf "%s:%s\n"
        (format_ptime fire.fire_time)
        (Option.value ~default:"" (event_alarm_summary fire)))
    fires;
  [%expect {| 2025-04-30T11:00:00Z:Sparse horizon |}]

let%expect_test "absolute alarm on recurring event fires once" =
  Eio_main.run @@ fun env ->
  let _ = env in
  let start = ptime_of (2025, 4, 1) (12, 0, 0) in
  let absolute = ptime_of (2025, 4, 2) (9, 0, 15) in
  let recurrence = (`Daily, Some (`Count 3), None, []) in
  let event =
    Result.get_ok
      (Event.create ~summary:"Absolute"
         ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
         ~recurrence
         ~alarms:[ make_absolute_alarm absolute ]
         ())
  in
  let from = Some (ptime_of (2025, 4, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 4, 5) (0, 0, 0) in
  let fires =
    Event.compute_alarm_fires ~floating_tz:Timedesc.Time_zone.utc ~from ~to_
      event
  in
  Printf.printf "count: %d\n" (List.length fires);
  List.iter
    (fun (fire : Event.alarm_owner Alarm.fire) ->
      print_endline (format_ptime fire.fire_time))
    fires;
  [%expect {|
    count: 1
    2025-04-02T09:00:15Z |}]

let%expect_test "event relative and absolute alarms include every repeat" =
  Eio_main.run @@ fun env ->
  let _ = env in
  let start = ptime_of (2025, 4, 15) (10, 0, 0) in
  let absolute = ptime_of (2025, 4, 15) (12, 0, 0) in
  let event =
    Result.get_ok
      (Event.create ~summary:"Repeating alarms"
         ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
         ~alarms:
           [
             make_relative_alarm (-60)
             |> with_repeat ~every_seconds:30 ~repeat:2;
             make_absolute_alarm absolute
             |> with_repeat ~every_seconds:45 ~repeat:2;
           ]
         ())
  in
  let from = Some (ptime_of (2025, 4, 15) (9, 58, 0)) in
  let to_ = ptime_of (2025, 4, 15) (12, 2, 0) in
  Event.compute_alarm_fires ~floating_tz:Timedesc.Time_zone.utc ~from ~to_ event
  |> List.iter (fun (fire : Event.alarm_owner Alarm.fire) ->
      print_endline (format_ptime fire.fire_time));
  [%expect
    {|
    2025-04-15T09:59:00Z
    2025-04-15T09:59:30Z
    2025-04-15T10:00:00Z
    2025-04-15T12:00:00Z
    2025-04-15T12:00:45Z
    2025-04-15T12:01:30Z |}]

let%expect_test "relative alarm references require component boundaries" =
  Eio_main.run @@ fun env ->
  let _ = env in
  let start = ptime_of (2025, 4, 15) (10, 0, 0) in
  let event_result =
    Event.create ~summary:"Missing end"
      ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
      ~alarms:[ make_relative_alarm ~related:`End (-60) ]
      ()
  in
  let todo_result =
    Todo.create ~alarms:[ make_relative_alarm ~related:`Start (-60) ] ()
  in
  Printf.printf "event end rejected: %b\n" (Result.is_error event_result);
  Printf.printf "todo start rejected: %b\n" (Result.is_error todo_result);
  [%expect {|
    event end rejected: true
    todo start rejected: true |}]

(* --- Todo alarm fire computation tests --- *)

let%expect_test "todo alarm fires from ics file" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let components =
    Result.get_ok
    @@ Calendar_dir.get_calendar_components ~fs calendar_dir "alarm"
  in
  let todos = List.filter_map Component.to_todo components in
  let todo =
    List.find (fun t -> Todo.get_id t = "alarm-todo@caledonia.test") todos
  in
  let from = Some (ptime_of (2025, 4, 1) (0, 0, 0)) in
  let to_ = ptime_of (2025, 4, 30) (23, 59, 59) in
  let fires =
    Todo.compute_alarm_fires ~floating_tz:Timedesc.Time_zone.utc ~from ~to_ todo
  in
  Printf.printf "Number of alarm fires: %d\n" (List.length fires);
  List.iter
    (fun (af : Todo.t Alarm.fire) ->
      Printf.printf "  %s\n" (format_ptime af.fire_time))
    fires;
  [%expect {|
    Number of alarm fires: 1
      2025-04-20T13:30:00Z |}]

let%expect_test "todo absolute alarm repeats are bounded and scheduled" =
  Eio_main.run @@ fun env ->
  let _ = env in
  let absolute = ptime_of (2025, 4, 20) (9, 0, 0) in
  let alarm =
    make_absolute_alarm absolute |> with_repeat ~every_seconds:20 ~repeat:2
  in
  let todo = Result.get_ok (Todo.create ~alarms:[ alarm ] ()) in
  let from = Some absolute in
  let to_ = ptime_of (2025, 4, 20) (9, 1, 0) in
  Todo.compute_alarm_fires ~floating_tz:Timedesc.Time_zone.utc ~from ~to_ todo
  |> List.iter (fun (fire : Todo.t Alarm.fire) ->
      print_endline (format_ptime fire.fire_time));
  Printf.printf "limit rejected: %b\n"
    (Result.is_error
       (Alarm.repeated_instants ~max_repetitions:1 absolute alarm));
  [%expect
    {|
    2025-04-20T09:00:00Z
    2025-04-20T09:00:20Z
    2025-04-20T09:00:40Z
    limit rejected: true |}]

(* --- Component unified alarm query tests --- *)

let%expect_test "component query_alarm_fires combines events and todos" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let components =
    Result.get_ok
    @@ Calendar_dir.get_calendar_components ~fs calendar_dir "alarm"
  in
  (* Range that covers both the event (Apr 15) and todo (Apr 20) alarm fires *)
  let from = Some (ptime_of (2025, 4, 14) (0, 0, 0)) in
  let to_ = ptime_of (2025, 4, 21) (0, 0, 0) in
  let fires =
    Alarm_query.run ~floating_tz:Timedesc.Time_zone.utc ~from ~to_ components
  in
  Printf.printf "Number of alarm fires: %d\n" (List.length fires);
  List.iter
    (fun (af : Alarm_query.fire) ->
      let summary =
        match Component_query.get_summary af.owner with
        | Some s -> s
        | None -> "(none)"
      in
      Printf.printf "  %s  %s\n" (format_ptime af.fire_time) summary)
    fires;
  [%expect
    {|
    Number of alarm fires: 4
      2025-04-15T09:00:00Z  Alarm Test Event
      2025-04-15T09:45:00Z  Alarm Test Event
      2025-04-17T11:00:00Z  Recurring Alarm Event
      2025-04-20T13:30:00Z  Alarm Test Todo |}]

let%expect_test "component query_alarm_fires sorted by fire_time" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let components =
    Result.get_ok
    @@ Calendar_dir.get_calendar_components ~fs calendar_dir "alarm"
  in
  let from = Some (ptime_of (2025, 3, 27) (0, 0, 0)) in
  let to_ = ptime_of (2025, 4, 21) (0, 0, 0) in
  let fires =
    Alarm_query.run ~floating_tz:Timedesc.Time_zone.utc ~from ~to_ components
  in
  (* Verify sorted order *)
  let times = List.map (fun (af : Alarm_query.fire) -> af.fire_time) fires in
  let sorted =
    List.for_all2
      (fun a b -> Ptime.compare a b <= 0)
      (List.filteri (fun i _ -> i < List.length times - 1) times)
      (List.filteri (fun i _ -> i > 0) times)
  in
  Printf.printf "Results sorted by fire_time: %b\n" sorted;
  Printf.printf "Total fires: %d\n" (List.length fires);
  [%expect {|
    Results sorted by fire_time: true
    Total fires: 7 |}]
