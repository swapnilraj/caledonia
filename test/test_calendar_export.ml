open Caledonia_lib

let contains_substring ~needle haystack =
  let needle_length = String.length needle in
  let rec search offset =
    if offset + needle_length > String.length haystack then false
    else if String.sub haystack offset needle_length = needle then true
    else search (offset + 1)
  in
  needle = "" || search 0

let with_fs run = Eio_main.run @@ fun env -> run (Eio.Stdenv.fs env)

let source ~fs ~calendar_key ~file contents =
  Component_source.of_decoded_document ~calendar_key
    ~file:Eio.Path.(fs / file)
    ~fingerprint:(Digest.string contents |> Digest.to_hex)
    ()

let document ~fs ~calendar_key ~file contents =
  Calendar_document.parse
    ~source:(source ~fs ~calendar_key ~file contents)
    contents
  |> Result.get_ok

let timezone ?(offset = "+0000") tzid =
  [
    "BEGIN:VTIMEZONE";
    "TZID:" ^ tzid;
    "BEGIN:STANDARD";
    "DTSTART:19700101T000000";
    "TZOFFSETFROM:" ^ offset;
    "TZOFFSETTO:" ^ offset;
    "END:STANDARD";
    "END:VTIMEZONE";
  ]

let calendar ?(offset = "+0000") ?(referenced = true) ~uid () =
  String.concat "\r\n"
    ([ "BEGIN:VCALENDAR"; "VERSION:2.0"; "PRODID:-//Export tests//EN" ]
    @ timezone ~offset "Europe/London"
    @ [
        "BEGIN:VEVENT";
        "UID:" ^ uid;
        "DTSTAMP:20260715T080000Z";
        (if referenced then "DTSTART;TZID=Europe/London:20260715T090000"
         else "DTSTART:20260715T080000Z");
        "SUMMARY:" ^ uid;
        "END:VEVENT";
        "END:VCALENDAR";
        "";
      ])

let series_calendar =
  String.concat "\r\n"
    ([ "BEGIN:VCALENDAR"; "VERSION:2.0"; "PRODID:-//Export tests//EN" ]
    @ timezone "Europe/London" @ timezone "Example/Unused"
    @ [
        "BEGIN:VEVENT";
        "UID:series";
        "DTSTAMP:20260715T080000Z";
        "DTSTART;TZID=Europe/London:20260715T090000";
        "RRULE:FREQ=DAILY;COUNT=2";
        "SUMMARY:master";
        "END:VEVENT";
        "BEGIN:VEVENT";
        "UID:series";
        "DTSTAMP:20260715T080000Z";
        "RECURRENCE-ID;TZID=Europe/London:20260716T090000";
        "DTSTART;TZID=Europe/London:20260716T100000";
        "SUMMARY:override";
        "END:VEVENT";
        "BEGIN:VTODO";
        "UID:unselected-todo";
        "DTSTAMP:20260715T080000Z";
        "SUMMARY:unselected";
        "END:VTODO";
        "END:VCALENDAR";
        "";
      ])

let parsed_components exported =
  Calendar_codec.Legacy.parse exported |> Result.get_ok |> snd

let component_counts components =
  List.fold_left
    (fun (timezones, events, todos) -> function
      | `Timezone _ -> (timezones + 1, events, todos)
      | `Event _ -> (timezones, events + 1, todos)
      | `Todo _ -> (timezones, events, todos + 1)
      | `Journal _ | `Freebusy _ -> (timezones, events, todos))
    (0, 0, 0) components

let stored_event document =
  Calendar_document.components document
  |> List.find (fun component ->
      Component.component_type component = Component_kind.Event)

let%expect_test
    "stored selected export preserves its series and exact timezone context" =
  with_fs @@ fun fs ->
  let document =
    document ~fs ~calendar_key:"one" ~file:"one/series.ics" series_calendar
  in
  let selected = stored_event document in
  let exported =
    Calendar_export.stored_to_ics ~documents:[ document ] [ selected ]
    |> Result.get_ok
  in
  let timezone_count, event_count, todo_count =
    parsed_components exported |> component_counts
  in
  Printf.printf "timezones=%d events=%d todos=%d unused-zone=%b\n"
    timezone_count event_count todo_count
    (String.split_on_char '\n' exported
    |> List.exists (String.starts_with ~prefix:"TZID:Example/Unused"));
  Printf.printf "empty=%S missing-context=%b\n"
    (Result.get_ok (Calendar_export.stored_to_ics ~documents:[] []))
    (match Calendar_export.stored_to_ics ~documents:[] [ selected ] with
    | Error (`Msg message) ->
        String.equal message
          "Selected component has no matching immutable document export context"
    | Ok _ -> false);
  [%expect
    {|
      timezones=1 events=2 todos=0 unused-zone=false
      empty="" missing-context=true |}]

let%expect_test "stored DATE UNTIL export preserves its semantic DATE limit" =
  with_fs @@ fun fs ->
  let source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Export DATE UNTIL tests//EN";
        "BEGIN:VEVENT";
        "UID:date-until-export";
        "DTSTAMP:20260715T080000Z";
        "DTSTART;VALUE=DATE:20260715";
        "RRULE:FREQ=DAILY;UNTIL=20260716";
        "SUMMARY:DATE UNTIL";
        "END:VEVENT";
        "END:VCALENDAR";
        "";
      ]
  in
  let original =
    document ~fs ~calendar_key:"one" ~file:"one/date-until.ics" source
  in
  let selected = stored_event original in
  let exported =
    Calendar_export.stored_to_ics ~documents:[ original ] [ selected ]
    |> Result.get_ok
  in
  let round_trip =
    document ~fs ~calendar_key:"export" ~file:"export/date-until.ics" exported
    |> stored_event |> Component.to_event |> Option.get
  in
  Printf.printf "date-wire=%b utc-midnight=%b private-marker=%b metadata=%b\n"
    (String.split_on_char '\n' exported
    |> List.exists
         (String.starts_with ~prefix:"RRULE:FREQ=DAILY;UNTIL=20260716"))
    (contains_substring ~needle:"UNTIL=20260716T000000Z" exported)
    (contains_substring ~needle:"X-CALEDONIA" exported)
    (Event.date_until round_trip = Some (2026, 7, 16));
  [%expect
    {| date-wire=true utc-midnight=false private-marker=false metadata=true |}]

let%expect_test "finite occurrence export emits only its effective event" =
  with_fs @@ fun fs ->
  let document =
    document ~fs ~calendar_key:"one" ~file:"one/series.ics" series_calendar
  in
  let stored_series = stored_event document in
  let series = Component.to_event stored_series |> Option.get in
  let occurrences =
    Event.Recurrence.expand
      ~floating_tz:(Timedesc.Time_zone.make_exn "Europe/London")
      ~from:None
      ~to_:(Ptime.of_date (2026, 7, 18) |> Option.get)
      series
    |> Result.get_ok
  in
  let occurrence = List.hd occurrences in
  let exported =
    Calendar_export.to_ics ~documents:[ document ]
      [ Component_query.Occurrence { stored_series; occurrence } ]
    |> Result.get_ok
  in
  let timezone_count, event_count, todo_count =
    parsed_components exported |> component_counts
  in
  Printf.printf "timezones=%d events=%d todos=%d finite=%b\n" timezone_count
    event_count todo_count
    (not
       (String.split_on_char '\n' exported
       |> List.exists (String.starts_with ~prefix:"RRULE:")));
  [%expect {| timezones=1 events=1 todos=0 finite=true |}]

let%expect_test
    "timezone definitions dedupe, reject conflicts, and omit unreferenced data"
    =
  with_fs @@ fun fs ->
  let one_source = calendar ~uid:"one" () in
  let two_source = calendar ~uid:"two" () in
  let conflict_source = calendar ~offset:"+0100" ~uid:"two" () in
  let unused_one = calendar ~referenced:false ~uid:"unused-one" () in
  let unused_two =
    calendar ~offset:"+0100" ~referenced:false ~uid:"unused-two" ()
  in
  let one = document ~fs ~calendar_key:"one" ~file:"one.ics" one_source in
  let two = document ~fs ~calendar_key:"two" ~file:"two.ics" two_source in
  let conflict =
    document ~fs ~calendar_key:"two" ~file:"conflict.ics" conflict_source
  in
  let unused_one_document =
    document ~fs ~calendar_key:"unused-one" ~file:"unused-one.ics" unused_one
  in
  let unused_two_document =
    document ~fs ~calendar_key:"unused-two" ~file:"unused-two.ics" unused_two
  in
  let identical =
    Calendar_export.stored_to_ics ~documents:[ one; two ]
      [ stored_event one; stored_event two ]
    |> Result.get_ok |> parsed_components |> component_counts
  in
  let conflict_rejected =
    match
      Calendar_export.stored_to_ics ~documents:[ one; conflict ]
        [ stored_event one; stored_event conflict ]
    with
    | Error (`Msg message) ->
        String.equal message
          "Conflicting VTIMEZONE definitions for TZID Europe/London"
    | Ok _ -> false
  in
  let unused =
    Calendar_export.stored_to_ics
      ~documents:[ unused_one_document; unused_two_document ]
      [ stored_event unused_one_document; stored_event unused_two_document ]
    |> Result.get_ok |> parsed_components |> component_counts
  in
  let identical_timezones, identical_events, _ = identical in
  let unused_timezones, _, _ = unused in
  Printf.printf
    "identical-timezones=%d events=%d conflict=%b unused-timezones=%d\n"
    identical_timezones identical_events conflict_rejected unused_timezones;
  [%expect
    {|
      identical-timezones=1 events=2 conflict=true unused-timezones=0 |}]

let%expect_test "selected timezone export retains nested opaque source context"
    =
  with_fs @@ fun fs ->
  let source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Export nested timezone tests//EN";
        "BEGIN:VTIMEZONE";
        "TZID:Europe/London";
        "BEGIN:X-TIMEZONE-METADATA";
        "X-PRIVATE-VALUE:preserve-me";
        "END:X-TIMEZONE-METADATA";
        "BEGIN:STANDARD";
        "DTSTART:19700101T000000";
        "TZOFFSETFROM:+0000";
        "TZOFFSETTO:+0000";
        "END:STANDARD";
        "END:VTIMEZONE";
        "BEGIN:VEVENT";
        "UID:nested-timezone-export";
        "DTSTAMP:20260715T080000Z";
        "DTSTART;TZID=Europe/London:20260715T090000";
        "SUMMARY:Nested timezone context";
        "END:VEVENT";
        "END:VCALENDAR";
        "";
      ]
  in
  let original =
    document ~fs ~calendar_key:"nested" ~file:"nested/timezone.ics" source
  in
  let exported =
    Calendar_export.stored_to_ics ~documents:[ original ]
      [ stored_event original ]
    |> Result.get_ok
  in
  let timezone_begin =
    Str.search_forward (Str.regexp_string "BEGIN:VTIMEZONE") exported 0
  in
  let nested_begin =
    Str.search_forward
      (Str.regexp_string "BEGIN:X-TIMEZONE-METADATA")
      exported timezone_begin
  in
  let timezone_end =
    Str.search_forward (Str.regexp_string "END:VTIMEZONE") exported nested_begin
  in
  let round_trip =
    document ~fs ~calendar_key:"round-trip" ~file:"round-trip/timezone.ics"
      exported
  in
  let round_trip_serialized = Calendar_document.serialize round_trip in
  let timezone_count, event_count, _ =
    parsed_components exported |> component_counts
  in
  Printf.printf
    "nested=%b inside-timezone=%b roundtrip=%b timezones=%d events=%d\n"
    (contains_substring ~needle:"X-PRIVATE-VALUE:preserve-me" exported)
    (timezone_begin < nested_begin && nested_begin < timezone_end)
    (contains_substring ~needle:"X-PRIVATE-VALUE:preserve-me"
       round_trip_serialized)
    timezone_count event_count;
  [%expect
    {|
    nested=true inside-timezone=true roundtrip=true timezones=1 events=1
    |}]
