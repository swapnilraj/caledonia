open Caledonia_lib

module Event = struct
  include Event

  let create = create ~now:Ptime.epoch
  let edit_patch = edit_patch ~now:Ptime.epoch

  let authored_event event =
    let known = Calendar_codec.make_known (`Event event) |> Result.get_ok in
    let event =
      match Calendar_codec.component known with
      | `Event event -> event
      | _ -> assert false
    in
    let date_until =
      match (event.Icalendar.rrule, Calendar_codec.rrule_date_untils known) with
      | None, [] -> None
      | Some _, [ date_until ] -> date_until
      | _ -> failwith "invalid VEVENT RRULE metadata shape"
    in
    (event, date_until)

  let events_of_icalendar_result ?display_name ?source_fingerprint _calendar_key
      ~file calendar =
    let _ = (display_name, source_fingerprint, file) in
    List.filter_map
      (function `Event event -> Some event | _ -> None)
      (snd calendar)
    |> List.map authored_event |> of_authored_events_result

  let events_of_icalendar ?display_name ?source_fingerprint calendar_key ~file
      calendar =
    events_of_icalendar_result ?display_name ?source_fingerprint calendar_key
      ~file calendar
    |> Result.get_ok

  let to_ical_calendar series =
    ( [
        `Prodid (Icalendar.Params.empty, "-//Caledonia event test//EN");
        `Version (Icalendar.Params.empty, "2.0");
      ],
      List.map (fun event -> `Event event) (authored_events series) )
end

let fixed_date = Option.get @@ Ptime.of_date_time ((2025, 3, 27), ((0, 0, 0), 0))
let setup_fixed_date () = fixed_date
let calendar_dir_path = Filename.concat (Sys.getcwd ()) "calendar"
let ptime_of ymd hms = Option.get (Ptime.of_date_time (ymd, (hms, 0)))

let get_events ~fs calendar_dir =
  Calendar_dir.get_components ~fs calendar_dir
  |> Result.map (List.filter_map Component.to_event)

let make_event ~fs ?end_ summary start =
  let _ = fs in
  Result.get_ok
    (Event.create ~summary
       ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
       ?end_ ())

let result_is_error = function Error _ -> true | Ok _ -> false

let%expect_test "domain mutation timestamps are explicit and no-op stable" =
  let created_at = ptime_of (2026, 7, 16) (9, 0, 0) in
  let edited_at = ptime_of (2026, 7, 16) (10, 0, 0) in
  let start = ptime_of (2026, 7, 17) (12, 0, 0) in
  let event =
    Caledonia_lib.Event.create ~now:created_at ~summary:"Clocked"
      ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
      ()
    |> Result.get_ok
  in
  let edited =
    Caledonia_lib.Event.edit_patch ~now:edited_at ~summary:(Patch.Set "Edited")
      event
    |> Result.get_ok
  in
  let unchanged =
    Caledonia_lib.Event.edit_patch
      ~now:(ptime_of (2026, 7, 16) (11, 0, 0))
      edited
    |> Result.get_ok
  in
  let stamp value = snd (Caledonia_lib.Event.master value).Icalendar.dtstamp in
  Printf.printf "create=%b edit=%b noop=%b\n"
    (Ptime.equal (stamp event) created_at)
    (Ptime.equal (stamp edited) edited_at)
    (Ptime.equal (stamp unchanged) edited_at);
  [%expect {| create=true edit=true noop=true |}]

module Query_item = struct
  type t = Stored of Event.t | Occurrence of Event.Occurrence.t

  let summary = function
    | Stored event -> Event.get_summary event
    | Occurrence occurrence -> Event.Occurrence.get_summary occurrence

  let location = function
    | Stored event -> Event.get_location event
    | Occurrence occurrence -> Event.Occurrence.get_location occurrence

  let description = function
    | Stored event -> Event.get_description event
    | Occurrence occurrence -> Event.Occurrence.get_description occurrence

  let id = function
    | Stored event -> Event.get_id event
    | Occurrence occurrence ->
        Event.Occurrence.reference occurrence |> Event.Occurrence.Reference.uid

  let start_result = function
    | Stored event ->
        Event.get_start_result ~floating_tz:Timedesc.Time_zone.utc event
    | Occurrence occurrence -> Event.Occurrence.get_start_result occurrence
end

let item_contains needle value =
  match value with
  | None -> false
  | Some value ->
      let rex = Re.Pcre.regexp ~flags:[ `CASELESS ] (Re.Pcre.quote needle) in
      Re.Pcre.pmatch ~rex value

let query_items ?matches events ~from ~to_ =
  let overlaps event =
    let start =
      Event.get_start_result ~floating_tz:Timedesc.Time_zone.utc event
      |> Result.get_ok
    in
    let end_ =
      Event.get_end_result ~floating_tz:Timedesc.Time_zone.utc event
      |> Result.get_ok
      |> Option.value ~default:start
    in
    Ptime.compare start to_ < 0
    &&
    match from with
    | None -> true
    | Some lower when Ptime.equal start end_ -> Ptime.compare start lower >= 0
    | Some lower -> Ptime.compare end_ lower > 0
  in
  let items =
    List.concat_map
      (fun event ->
        if Event.has_recurrence_set event then
          Event.Recurrence.expand ~floating_tz:Timedesc.Time_zone.utc ~from ~to_
            event
          |> Result.get_ok
          |> List.map (fun occurrence -> Query_item.Occurrence occurrence)
        else if overlaps event then [ Query_item.Stored event ]
        else [])
      events
  in
  let items =
    match matches with
    | None -> items
    | Some predicate -> List.filter predicate items
  in
  List.stable_sort
    (fun left right ->
      Ptime.compare
        (Query_item.start_result left |> Result.get_ok)
        (Query_item.start_result right |> Result.get_ok))
    items

let%expect_test "event temporal validation rejects invalid end and duration" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _ = fs in
  let start = ptime_of (2025, 4, 1) (10, 0, 0) in
  let before = ptime_of (2025, 4, 1) (9, 0, 0) in
  let create end_ =
    Event.create ~summary:"invalid"
      ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
      ~end_ ()
  in
  let backwards =
    create (`Dtend (Icalendar.Params.empty, `Datetime (`Utc before)))
  in
  let zero_duration =
    create (`Duration (Icalendar.Params.empty, Ptime.Span.zero))
  in
  let unknown_tz =
    Event.create ~summary:"invalid timezone"
      ~start:
        ( Icalendar.Params.empty,
          `Datetime (`With_tzid (start, (false, "Mars/Olympus"))) )
      ~end_:
        (`Dtend
           ( Icalendar.Params.empty,
             `Datetime (`With_tzid (before, (false, "UTC"))) ))
      ()
  in
  Printf.printf "backwards=%b zero=%b unknown-tz=%b\n"
    (result_is_error backwards)
    (result_is_error zero_duration)
    (result_is_error unknown_tz);
  [%expect {| backwards=true zero=true unknown-tz=true |}]

let custom_timezone_calendar ?(embedded = false) ~event_end ~todo_due () =
  String.concat "\r\n"
    ([
       "BEGIN:VCALENDAR";
       "VERSION:2.0";
       "PRODID:-//Caledonia custom timezone validation//EN";
     ]
    @ (if embedded then
         [
           "BEGIN:VTIMEZONE";
           "TZID:Mars/Olympus";
           "BEGIN:STANDARD";
           "DTSTART:19700101T000000";
           "TZOFFSETFROM:+0000";
           "TZOFFSETTO:+0000";
           "END:STANDARD";
           "END:VTIMEZONE";
         ]
       else [])
    @ [
        "BEGIN:VEVENT";
        "UID:custom-zone-event";
        "DTSTAMP:20260701T000000Z";
        "DTSTART;TZID=Mars/Olympus:20260715T090000";
        "DTEND;TZID=Mars/Olympus:" ^ event_end;
        "SUMMARY:Custom zone event";
        "END:VEVENT";
        "BEGIN:VTODO";
        "UID:custom-zone-todo";
        "DTSTAMP:20260701T000000Z";
        "DTSTART;TZID=Mars/Olympus:20260715T110000";
        "DUE;TZID=Mars/Olympus:" ^ todo_due;
        "SUMMARY:Custom zone todo";
        "END:VTODO";
        "END:VCALENDAR";
        "";
      ])

let%expect_test "same custom TZID ranges load by authored wall-clock order" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let file = Eio.Path.(fs / "custom-timezone.ics") in
  let load ?embedded ~event_end ~todo_due () =
    let encoded = custom_timezone_calendar ?embedded ~event_end ~todo_due () in
    let source =
      Component_source.of_decoded_document ~calendar_key:"work" ~file
        ~fingerprint:(Digest.string encoded |> Digest.to_hex)
        ()
    in
    encoded |> Calendar_codec.Legacy.parse |> Result.get_ok |> snd
    |> Component.stored_views_of_decoded_components ~source
  in
  let inspect result =
    match result with
    | Error _ -> (false, false, false)
    | Ok components ->
        let event =
          List.find_map Component.to_event components
          |> Option.get |> Event.master
        in
        let todo =
          List.find_map Component.to_todo components
          |> Option.get |> Todo.to_ical_todo
        in
        let event_range_preserved =
          match (snd event.dtstart, event.dtend_or_duration) with
          | ( `Datetime (`With_tzid (_, (_, start_tzid))),
              Some (`Dtend (_, `Datetime (`With_tzid (_, (_, end_tzid))))) ) ->
              start_tzid = "Mars/Olympus" && end_tzid = "Mars/Olympus"
          | _ -> false
        in
        let todo_range_preserved =
          let timezone property =
            match property with
            | `Dtstart (_, `Datetime (`With_tzid (_, (_, tzid))))
            | `Due (_, `Datetime (`With_tzid (_, (_, tzid)))) ->
                Some tzid
            | _ -> None
          in
          List.filter_map timezone todo = [ "Mars/Olympus"; "Mars/Olympus" ]
        in
        (List.length components = 2, event_range_preserved, todo_range_preserved)
  in
  let without_definition =
    load ~event_end:"20260715T100000" ~todo_due:"20260715T120000" () |> inspect
  in
  let with_definition =
    load ~embedded:true ~event_end:"20260715T100000" ~todo_due:"20260715T120000"
      ()
    |> inspect
  in
  let invalid_event =
    load ~event_end:"20260715T080000" ~todo_due:"20260715T120000" ()
  in
  let invalid_todo =
    load ~event_end:"20260715T100000" ~todo_due:"20260715T100000" ()
  in
  let print_result label (count, event, todo) =
    Printf.printf "%s=count:%b event:%b todo:%b\n" label count event todo
  in
  print_result "without" without_definition;
  print_result "embedded" with_definition;
  Printf.printf "invalid-event=%b invalid-todo=%b\n"
    (result_is_error invalid_event)
    (result_is_error invalid_todo);
  [%expect
    {|
    without=count:true event:true todo:true
    embedded=count:true event:true todo:true
    invalid-event=true invalid-todo=true |}]

let%expect_test "event alarm validation enforces RFC action constraints" =
  Eio_main.run @@ fun env ->
  let _ = env in
  let start = ptime_of (2025, 4, 1) (10, 0, 0) in
  let trigger =
    (Icalendar.Params.empty, `Duration (Ptime.Span.of_int_s (-300)))
  in
  let invalid_display : Icalendar.alarm =
    `Display
      {
        trigger;
        duration_repeat = None;
        summary = None;
        other = [];
        special = { description = None };
      }
  in
  let invalid_email : Icalendar.alarm =
    `Email
      {
        trigger;
        duration_repeat = None;
        summary = Some (Icalendar.Params.empty, "Reminder");
        other = [];
        special =
          {
            description = (Icalendar.Params.empty, "Reminder");
            attendees = [];
            attach = None;
          };
      }
  in
  let create alarm =
    Event.create ~summary:"alarm validation"
      ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
      ~alarms:[ alarm ] ()
  in
  Printf.printf "display=%b email=%b\n"
    (result_is_error (create invalid_display))
    (result_is_error (create invalid_email));
  [%expect {| display=true email=true |}]

let%expect_test "loaded VEVENTs reject invalid domain and singleton states" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let load extra =
    let source =
      String.concat "\r\n"
        [
          "BEGIN:VCALENDAR";
          "VERSION:2.0";
          "PRODID:-//Event validation//EN";
          "BEGIN:VEVENT";
          "UID:validation";
          "DTSTAMP:20260701T000000Z";
          "DTSTART:20260715T100000Z";
          extra;
          "END:VEVENT";
          "END:VCALENDAR";
          "";
        ]
    in
    match Icalendar.parse source with
    | Error _ -> true
    | Ok calendar ->
        Result.is_error
          (Event.events_of_icalendar_result "test"
             ~file:Eio.Path.(fs / "validation.ics")
             calendar)
  in
  let invalid_priority =
    let source =
      String.concat "\r\n"
        [
          "BEGIN:VCALENDAR";
          "VERSION:2.0";
          "PRODID:-//Event validation//EN";
          "BEGIN:VEVENT";
          "UID:validation";
          "DTSTAMP:20260701T000000Z";
          "DTSTART:20260715T100000Z";
          "END:VEVENT";
          "END:VCALENDAR";
          "";
        ]
    in
    match Icalendar.parse source with
    | Error _ -> false
    | Ok (properties, [ `Event event ]) ->
        let event =
          {
            event with
            props = `Priority (Icalendar.Params.empty, 10) :: event.props;
          }
        in
        Result.is_error
          (Event.events_of_icalendar_result "test"
             ~file:Eio.Path.(fs / "validation.ics")
             (properties, [ `Event event ]))
    | Ok _ -> false
  in
  Printf.printf
    "status=%b priority=%b duplicate-status=%b end=%b alarm-reference=%b\n"
    (load "STATUS:COMPLETED") invalid_priority
    (load "STATUS:CONFIRMED\r\nSTATUS:TENTATIVE")
    (load "DTEND:20260715T090000Z")
    (load
       "BEGIN:VALARM\r\n\
        ACTION:DISPLAY\r\n\
        TRIGGER;RELATED=END:-PT5M\r\n\
        DESCRIPTION:Reminder\r\n\
        END:VALARM");
  [%expect
    {|
    status=true priority=true duplicate-status=true end=true alarm-reference=true
    |}]

let%expect_test "query ranges are half-open including point events" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let lower = ptime_of (2025, 4, 1) (10, 0, 0) in
  let upper = ptime_of (2025, 4, 1) (11, 0, 0) in
  let ended_at_lower =
    make_event ~fs "ended"
      (ptime_of (2025, 4, 1) (9, 0, 0))
      ~end_:
        (`Dtend
           ( Icalendar.Params.empty,
             `Datetime (`Utc (ptime_of (2025, 4, 1) (10, 0, 0))) ))
  in
  let at_lower = make_event ~fs "at lower" lower in
  let at_upper = make_event ~fs "at upper" upper in
  let results =
    query_items
      [ ended_at_lower; at_lower; at_upper ]
      ~from:(Some lower) ~to_:upper
  in
  List.iter
    (fun event ->
      print_endline (Option.value (Query_item.summary event) ~default:""))
    results;
  [%expect {| at lower |}]

let%expect_test "edit_patch clears optional event fields" =
  Eio_main.run @@ fun env ->
  let _ = env in
  let start = ptime_of (2025, 4, 1) (10, 0, 0) in
  let end_ = ptime_of (2025, 4, 1) (11, 0, 0) in
  let alarm =
    let open Icalendar in
    `Display
      {
        trigger = (Params.empty, `Duration (Ptime.Span.of_int_s (-90)));
        duration_repeat = None;
        summary = None;
        other = [];
        special = { description = Some (Params.empty, "Reminder") };
      }
  in
  let event =
    Result.get_ok
      (Event.create ~summary:"Patch"
         ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
         ~end_:(`Dtend (Icalendar.Params.empty, `Datetime (`Utc end_)))
         ~location:"Room" ~description:"Description" ~categories:[ "one" ]
         ~recurrence:(`Daily, Some (`Count 2), None, [])
         ~alarms:[ alarm ] ())
  in
  let edited =
    Result.get_ok
      (Event.edit_patch ~end_:Patch.Clear ~location:Patch.Clear
         ~description:Patch.Clear ~categories:Patch.Clear
         ~recurrence:Patch.Clear ~alarms:Patch.Clear event)
  in
  Printf.printf
    "end=%b location=%b description=%b categories=%d recurrence=%b alarms=%d\n"
    (Event.get_end_result ~floating_tz:Timedesc.Time_zone.utc edited
    |> Result.get_ok = None)
    (Event.get_location edited = None)
    (Event.get_description edited = None)
    (List.length (Event.get_categories edited))
    (Event.get_recurrence edited = None)
    (List.length (Event.get_alarms edited));
  [%expect
    {|
    end=true location=true description=true categories=0 recurrence=true alarms=0 |}]

let%expect_test "event body edits are isolated from sibling series" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar =
    Result.get_ok
      (Icalendar.parse
         "BEGIN:VCALENDAR\r\n\
          VERSION:2.0\r\n\
          PRODID:-//Test//EN\r\n\
          BEGIN:VEVENT\r\n\
          UID:first@example.test\r\n\
          DTSTAMP:20250301T000000Z\r\n\
          DTSTART:20250401T100000Z\r\n\
          SUMMARY:First\r\n\
          END:VEVENT\r\n\
          BEGIN:VEVENT\r\n\
          UID:second@example.test\r\n\
          DTSTAMP:20250301T000000Z\r\n\
          DTSTART:20250401T120000Z\r\n\
          SUMMARY:Second\r\n\
          END:VEVENT\r\n\
          END:VCALENDAR\r\n")
  in
  let events =
    Event.events_of_icalendar "test"
      ~file:Eio.Path.(fs / "siblings.ics")
      calendar
  in
  let first =
    List.find (fun event -> Event.get_id event = "first@example.test") events
  in
  let edited =
    Result.get_ok (Event.edit_patch ~summary:(Patch.Set "Edited") first)
  in
  let second =
    List.find (fun event -> Event.get_id event = "second@example.test") events
  in
  Printf.printf "series=%d edited=%b original=%b sibling=%b\n"
    (List.length events)
    (Event.get_summary edited = Some "Edited")
    (Event.get_summary first = Some "First")
    (Event.get_summary second = Some "Second");
  [%expect {| series=2 edited=true original=true sibling=true |}]

let%expect_test "query all events" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _ = setup_fixed_date () in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let from =
    Some (Option.get @@ Ptime.of_date_time ((2025, 01, 01), ((0, 0, 0), 0)))
  in
  let to_ = Option.get @@ Ptime.of_date_time ((2026, 01, 01), ((0, 0, 0), 0)) in
  let events = Result.get_ok @@ get_events ~fs calendar_dir in
  let events = query_items events ~from ~to_ in
  Printf.printf "Number of events: %d\n" (List.length events);
  let test_event =
    List.find_opt
      (fun event -> Option.get @@ Query_item.summary event = "Test Event")
      events
  in
  Printf.printf "Found Test Event: %b\n" (test_event <> None);
  [%expect {|
    Number of events: 832
    Found Test Event: true
    |}]

let%expect_test "recurrence expansion" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _ = setup_fixed_date () in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let from =
    Some (Option.get @@ Ptime.of_date_time ((2025, 3, 1), ((0, 0, 0), 0)))
  in
  let to_ =
    Option.get @@ Ptime.of_date_time ((2025, 5, 31), ((23, 59, 59), 0))
  in
  let events = Result.get_ok @@ get_events ~fs calendar_dir in
  let events = query_items events ~from ~to_ in
  let recurring_events =
    List.filter
      (fun event -> Option.get @@ Query_item.summary event = "Recurring Event")
      events
  in
  Printf.printf "Found multiple recurring events: %b\n"
    (List.length recurring_events > 1);
  Printf.printf "Number of recurring events: %d\n"
    (List.length recurring_events);
  [%expect
    {|
    Found multiple recurring events: true
    Number of recurring events: 10 |}]

let%expect_test "multiple EXDATE properties are all applied" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let from =
    Some (Option.get @@ Ptime.of_date_time ((2025, 5, 1), ((0, 0, 0), 0)))
  in
  let to_ = Option.get @@ Ptime.of_date_time ((2025, 6, 6), ((0, 0, 0), 0)) in
  let events = Result.get_ok @@ get_events ~fs calendar_dir in
  let events =
    query_items events ~from ~to_ ~matches:(fun item ->
        Query_item.id item = "with-exdate-recur@caledonia.test")
  in
  List.iter
    (fun event ->
      let year, month, day =
        Query_item.start_result event |> Result.get_ok |> Ptime.to_date
      in
      Printf.printf "%04d-%02d-%02d\n" year month day)
    events;
  [%expect
    {|
    2025-05-01
    2025-05-08
    2025-05-22
    2025-06-05
    |}]

let%expect_test "text search" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _ = setup_fixed_date () in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let from =
    Some (Option.get @@ Ptime.of_date_time ((2025, 01, 01), ((0, 0, 0), 0)))
  in
  let to_ = Option.get @@ Ptime.of_date_time ((2026, 01, 01), ((0, 0, 0), 0)) in
  let events = Result.get_ok @@ get_events ~fs calendar_dir in

  let filtered =
    query_items events ~from ~to_ ~matches:(fun item ->
        item_contains "Test" (Query_item.summary item))
  in
  Printf.printf "Events with 'Test' in summary: %d\n" (List.length filtered);

  let filtered =
    query_items events ~from ~to_ ~matches:(fun item ->
        item_contains "Weekly" (Query_item.location item))
  in
  Printf.printf "Events with 'Weekly' in location: %d\n" (List.length filtered);

  let filtered =
    query_items events ~from ~to_ ~matches:(fun item ->
        item_contains "Test" (Query_item.summary item)
        && item_contains "test" (Query_item.description item))
  in
  Printf.printf "Events matching AND criteria: %d\n" (List.length filtered);

  let filtered =
    query_items events ~from ~to_ ~matches:(fun item ->
        item_contains "Test" (Query_item.summary item)
        || item_contains "Weekly" (Query_item.location item))
  in
  Printf.printf "Events matching OR criteria: %d\n" (List.length filtered);

  [%expect
    {|
    Events with 'Test' in summary: 4
    Events with 'Weekly' in location: 10
    Events matching AND criteria: 3
    Events matching OR criteria: 14
    |}]
