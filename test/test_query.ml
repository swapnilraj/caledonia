open Caledonia_lib

let instant date time = Option.get (Ptime.of_date_time (date, (time, 0)))

let components_of_ics_key fs key ics =
  let calendar = Result.get_ok (Calendar_codec.Legacy.parse ics) in
  let source =
    Component_source.of_decoded_document ~calendar_key:key
      ~file:Eio.Path.(fs / (key ^ "-query.ics"))
      ~fingerprint:(Digest.string ics |> Digest.to_hex)
      ()
  in
  Component.stored_views_of_decoded_components ~source (snd calendar)
  |> Result.get_ok

let components_of_ics fs ics = components_of_ics_key fs "work" ics

let query ?(criteria = Component_query.no_criteria) ?(from = None)
    ?(include_undated_todos = false) ?(include_undated_journals = false)
    ?(include_todo_ancestors = false) ~to_ ?(sort = []) ~timezone components =
  Component_query.run ~timezone
    ~now:(instant (2026, 1, 10) (12, 0, 0))
    ~from ~to_ ~include_undated_todos ~include_undated_journals
    ~include_todo_ancestors ~criteria ~sort components

let unbounded_query ?(criteria = Component_query.no_criteria)
    ?(include_todo_ancestors = false) ?(sort = []) ~timezone components =
  Component_query.run_unbounded ~timezone
    ~now:(instant (2026, 1, 10) (12, 0, 0))
    ~include_todo_ancestors ~criteria ~sort components

let summaries items =
  items
  |> List.map (fun item ->
      Option.value ~default:"<none>" (Component_query.get_summary item))
  |> String.concat ", "

let mixed_ics =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia query tests//EN";
      "BEGIN:VEVENT";
      "UID:recurring";
      "DTSTAMP:20260101T000000Z";
      "DTSTART:20260101T090000Z";
      "SUMMARY:Recurring needle";
      "RRULE:FREQ=DAILY;COUNT=3";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:upper-bound";
      "DTSTAMP:20260101T000000Z";
      "DTSTART:20260103T000000Z";
      "SUMMARY:At exclusive upper bound";
      "END:VEVENT";
      "BEGIN:VTODO";
      "UID:due-only";
      "DTSTAMP:20260101T000000Z";
      "DUE;VALUE=DATE:20260102";
      "SUMMARY:Due only";
      "CATEGORIES:Work,Urgent";
      "END:VTODO";
      "BEGIN:VTODO";
      "UID:undated";
      "DTSTAMP:20260101T000000Z";
      "SUMMARY:Undated";
      "END:VTODO";
      "END:VCALENDAR";
      "";
    ]

let%expect_test
    "shared query expands recurring search and handles due-only todos" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let components = components_of_ics fs mixed_ics in
  let from = instant (2026, 1, 2) (0, 0, 0) in
  let to_ = instant (2026, 1, 3) (0, 0, 0) in
  let event_criteria =
    Component_query.
      {
        no_criteria with
        component_types = [ Component_kind.Event ];
        text = Some "needle";
        text_fields = [ Summary ];
      }
  in
  let events =
    query ~criteria:event_criteria ~from:(Some from) ~to_
      ~timezone:Timedesc.Time_zone.utc components
    |> Result.get_ok
  in
  let todo_criteria =
    Component_query.
      {
        no_criteria with
        component_types = [ Component_kind.Todo ];
        categories = [ "urgent" ];
      }
  in
  let dated_todos =
    query ~criteria:todo_criteria ~from:(Some from) ~to_
      ~timezone:Timedesc.Time_zone.utc components
    |> Result.get_ok
  in
  let all_todos =
    query
      ~criteria:
        Component_query.
          { no_criteria with component_types = [ Component_kind.Todo ] }
      ~from:(Some from) ~to_ ~include_undated_todos:true
      ~timezone:Timedesc.Time_zone.utc components
    |> Result.get_ok
  in
  Printf.printf "recurring=%s\n" (summaries events);
  Printf.printf "due-only=%s\n" (summaries dated_todos);
  Printf.printf "with-undated=%s\n" (summaries all_todos);
  Printf.printf "nominal-items=occurrence:%b stored:%b\n"
    (List.for_all
       (fun item -> Option.is_some (Component_query.occurrence item))
       events)
    (List.for_all
       (fun item -> Option.is_some (Component_query.stored item))
       all_todos);
  let occurrence_context =
    match events with
    | [ item ] ->
        Component_query.get_calendar_key item = "work"
        && Option.is_some (Component_query.get_recurrence_id_property item)
        && Option.is_some
             (Component_query.get_identity item)
               .Component_identity.recurrence_id
    | _ -> false
  in
  Printf.printf "occurrence-context=%b\n" occurrence_context;
  [%expect
    {|
      recurring=Recurring needle
      due-only=Due only
      with-undated=Due only, Undated
      nominal-items=occurrence:true stored:true
      occurrence-context=true |}]

let%expect_test "unbounded query returns far-future component masters once" =
  Eio_main.run @@ fun env ->
  let components =
    components_of_ics (Eio.Stdenv.fs env)
      (String.concat "\r\n"
         [
           "BEGIN:VCALENDAR";
           "VERSION:2.0";
           "PRODID:-//Caledonia unbounded query test//EN";
           "BEGIN:VEVENT";
           "UID:far-event";
           "DTSTAMP:20260101T000000Z";
           "DTSTART:21260101T090000Z";
           "SUMMARY:Far event";
           "END:VEVENT";
           "BEGIN:VEVENT";
           "UID:far-recurring";
           "DTSTAMP:20260101T000000Z";
           "DTSTART:21260101T100000Z";
           "SUMMARY:Far recurring";
           "RRULE:FREQ=DAILY;COUNT=3";
           "END:VEVENT";
           "BEGIN:VEVENT";
           "UID:far-recurring";
           "RECURRENCE-ID:21260102T100000Z";
           "DTSTAMP:20260101T000000Z";
           "DTSTART:21260102T110000Z";
           "SUMMARY:Moved far recurrence";
           "END:VEVENT";
           "BEGIN:VTODO";
           "UID:far-todo";
           "DTSTAMP:20260101T000000Z";
           "DUE:21260101T170000Z";
           "SUMMARY:Far todo";
           "END:VTODO";
           "BEGIN:VJOURNAL";
           "UID:far-journal";
           "DTSTAMP:20260101T000000Z";
           "DTSTART;VALUE=DATE:21260101";
           "SUMMARY:Far journal";
           "END:VJOURNAL";
           "END:VCALENDAR";
           "";
         ])
  in
  let criteria = Component_query.{ no_criteria with text = Some "Far" } in
  let selected =
    unbounded_query ~criteria ~timezone:Timedesc.Time_zone.utc components
    |> Result.get_ok
  in
  Printf.printf "ids=%s\n"
    (selected
    |> List.map Component_query.get_id
    |> List.sort String.compare |> String.concat ",");
  let recurring =
    List.find
      (fun item -> Component_query.get_id item = "far-recurring")
      selected
    |> Component_query.stored |> Option.get |> Component.to_event |> Option.get
  in
  Printf.printf "masters=%d recurrence=%b\n"
    (List.length
       (List.filter
          (fun item -> Component_query.get_id item = "far-recurring")
          selected))
    (Option.is_some (Event.get_recurrence recurring));
  Printf.printf "all-stored=%b\n"
    (List.for_all
       (fun item -> Option.is_some (Component_query.stored item))
       selected);
  [%expect
    {|
      ids=far-event,far-journal,far-recurring,far-todo
      masters=1 recurrence=true
      all-stored=true |}]

let london = Option.get (Timedesc.Time_zone.make "Europe/London")

let local_london date time =
  Date.ptime_of_ical_result ~floating_tz:london
    (`Datetime (`With_tzid (instant date time, (false, "Europe/London"))))
  |> Result.get_ok

let dst_ics =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia query tests//EN";
      "BEGIN:VEVENT";
      "UID:inside-dst-day";
      "DTSTAMP:20260101T000000Z";
      "DTSTART:20260329T225959Z";
      "SUMMARY:Inside 23 hour day";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:at-upper-dst";
      "DTSTAMP:20260101T000000Z";
      "DTSTART:20260329T230000Z";
      "SUMMARY:At next local midnight";
      "END:VEVENT";
      "END:VCALENDAR";
      "";
    ]

let%expect_test "non-UTC DST day remains a half-open local-day query" =
  Eio_main.run @@ fun env ->
  let components = components_of_ics (Eio.Stdenv.fs env) dst_ics in
  let from = local_london (2026, 3, 29) (0, 0, 0) in
  let to_ = local_london (2026, 3, 30) (0, 0, 0) in
  let selected =
    query ~from:(Some from) ~to_ ~timezone:london components |> Result.get_ok
  in
  Printf.printf "span-hours=%.0f selected=%s\n"
    (Ptime.Span.to_float_s (Ptime.diff to_ from) /. 3600.)
    (summaries selected);
  [%expect {| span-hours=23 selected=Inside 23 hour day |}]

let%expect_test "server query boundary uses the same explicit half-open model" =
  let request : Sexp.query_request =
    {
      from = Some "2026-03-29";
      to_ = "2026-03-29";
      timezone = Some "Europe/London";
      calendars = [ "work" ];
      text = Some "planning";
      search_in = [ Sexp.Summary; Sexp.Categories ];
      categories = [ "urgent" ];
      id = None;
      statuses = [];
      overdue = None;
      has_alarm = Some true;
      recurring = Some true;
      limit = Some 5;
    }
  in
  let criteria, from, to_, limit, timezone =
    Sexp.generate_query_params ~now:(instant (2026, 3, 29) (0, 0, 0)) request
    |> Result.get_ok
  in
  let from = Option.get from in
  Printf.printf "span-hours=%.0f zone=%s limit=%d fields=%d alarms=%b\n"
    (Ptime.Span.to_float_s (Ptime.diff to_ from) /. 3600.)
    (Timedesc.Time_zone.name timezone)
    (Option.get limit)
    (List.length criteria.Component_query.text_fields)
    (criteria.Component_query.has_alarm = Some true);
  [%expect {| span-hours=23 zone=Europe/London limit=5 fields=2 alarms=true |}]

let%expect_test "public query criteria reject unknown or inapplicable statuses"
    =
  let event_criteria =
    Component_query.
      {
        no_criteria with
        component_types = [ Component_kind.Event ];
        statuses = [ `Completed ];
      }
  in
  let unknown_status = Component_query.status_of_string "BOGUS" in
  Printf.printf "inapplicable=%b unknown=%b\n"
    (Result.is_error (Component_query.validate_criteria event_criteria))
    (Result.is_error unknown_status);
  [%expect {| inapplicable=true unknown=true |}]

let invalid_time_ics =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia query tests//EN";
      "BEGIN:VEVENT";
      "UID:unknown-zone";
      "DTSTAMP:20260101T000000Z";
      "DTSTART;TZID=Mars/Olympus:20260329T003000";
      "SUMMARY:Unknown";
      "END:VEVENT";
      "END:VCALENDAR";
      "";
    ]

let%expect_test "query surfaces typed calendar-time conversion errors" =
  Eio_main.run @@ fun env ->
  let components = components_of_ics (Eio.Stdenv.fs env) invalid_time_ics in
  let result =
    query
      ~from:(Some (instant (2026, 3, 1) (0, 0, 0)))
      ~to_:(instant (2026, 4, 1) (0, 0, 0))
      ~timezone:london components
  in
  (match result with
  | Error (`Msg message) -> print_endline message
  | Ok _ -> print_endline "unexpected success");
  (match Component_query.timezone_of_name "Mars/Olympus" with
  | Error (`Msg message) -> print_endline message
  | Ok _ -> print_endline "unexpected timezone success");
  [%expect
    {|
    unknown timezone Mars/Olympus
    Unknown timezone "Mars/Olympus"
    |}]

let%expect_test
    "unbounded non-temporal sorts do not resolve unrelated calendar times" =
  Eio_main.run @@ fun env ->
  let components = components_of_ics (Eio.Stdenv.fs env) invalid_time_ics in
  let by_summary =
    unbounded_query
      ~sort:[ Component_query.{ field = Summary_sort; descending = false } ]
      ~timezone:london components
  in
  let by_start =
    unbounded_query
      ~sort:[ Component_query.{ field = Start; descending = false } ]
      ~timezone:london components
  in
  (match by_summary with
  | Ok selected -> Printf.printf "summary=%s\n" (summaries selected)
  | Error (`Msg message) -> Printf.printf "summary-error=%s\n" message);
  (match by_start with
  | Ok _ -> print_endline "unexpected start-sort success"
  | Error (`Msg message) -> Printf.printf "start-error=%s\n" message);
  [%expect
    {|
      summary=Unknown
      start-error=unknown timezone Mars/Olympus |}]

let%expect_test "unsupported recurring todo and journal fail explicitly" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let check kind =
    let filename = kind ^ ".ics" in
    let text =
      String.concat "\r\n"
        [
          "BEGIN:VCALENDAR";
          "VERSION:2.0";
          "PRODID:-//Caledonia unsupported recurrence test//EN";
          "BEGIN:" ^ kind;
          "UID:recurring-component";
          "DTSTAMP:20260101T000000Z";
          "DTSTART:20260101T090000Z";
          "RRULE:FREQ=DAILY;COUNT=2";
          "SUMMARY:Recurring";
          "END:" ^ kind;
          "END:VCALENDAR";
          "";
        ]
    in
    let calendar = Calendar_codec.Legacy.parse text |> Result.get_ok in
    let source =
      Component_source.of_decoded_document ~calendar_key:"work"
        ~file:Eio.Path.(fs / filename)
        ~fingerprint:(Digest.string text |> Digest.to_hex)
        ()
    in
    match
      Component.stored_views_of_decoded_components ~source (snd calendar)
    with
    | Ok _ -> print_endline "unexpected success"
    | Error (`Msg message) -> print_endline message
  in
  check "VTODO";
  check "VJOURNAL";
  [%expect
    {|
    Recurring VTODO components are not supported; refusing to return an incomplete schedule
    Recurring VJOURNAL components are not supported; refusing to return an incomplete schedule |}]

let corrupt_todo_ics =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia query tests//EN";
      "BEGIN:VTODO";
      "UID:child";
      "DTSTAMP:20260101T000000Z";
      "DUE:20260102T090000Z";
      "RELATED-TO;RELTYPE=PARENT:missing";
      "SUMMARY:Child";
      "END:VTODO";
      "END:VCALENDAR";
      "";
    ]

let%expect_test "todo queries use checked parent graphs" =
  Eio_main.run @@ fun env ->
  let components = components_of_ics (Eio.Stdenv.fs env) corrupt_todo_ics in
  let criteria =
    Component_query.
      { no_criteria with component_types = [ Component_kind.Todo ] }
  in
  let result =
    query ~criteria
      ~to_:(instant (2026, 2, 1) (0, 0, 0))
      ~timezone:Timedesc.Time_zone.utc components
  in
  (match result with
  | Error (`Msg message) -> print_endline message
  | Ok _ -> print_endline "unexpected success");
  [%expect {| Todo "child" refers to missing parent "missing" |}]

let%expect_test "todo ancestor expansion preserves source order for stable ties"
    =
  Eio_main.run @@ fun env ->
  let components =
    components_of_ics (Eio.Stdenv.fs env)
      (String.concat "\r\n"
         [
           "BEGIN:VCALENDAR";
           "VERSION:2.0";
           "PRODID:-//Caledonia ancestor order test//EN";
           "BEGIN:VTODO";
           "UID:child-first";
           "DTSTAMP:20260101T000000Z";
           "SUMMARY:Needle child";
           "RELATED-TO;RELTYPE=PARENT:parent-second";
           "END:VTODO";
           "BEGIN:VTODO";
           "UID:parent-second";
           "DTSTAMP:20260101T000000Z";
           "SUMMARY:Parent";
           "END:VTODO";
           "END:VCALENDAR";
           "";
         ])
  in
  let criteria =
    Component_query.
      {
        no_criteria with
        component_types = [ Component_kind.Todo ];
        text = Some "Needle";
        text_fields = [ Summary ];
      }
  in
  let selected =
    query ~criteria ~include_undated_todos:true ~include_todo_ancestors:true
      ~to_:(instant (2026, 2, 1) (0, 0, 0))
      ~timezone:Timedesc.Time_zone.utc components
    |> Result.get_ok
  in
  print_endline (String.concat "," (List.map Component_query.get_id selected));
  [%expect {| child-first,parent-second |}]

let%expect_test "undated journals use an explicit search inclusion policy" =
  Eio_main.run @@ fun env ->
  let components =
    components_of_ics (Eio.Stdenv.fs env)
      (String.concat "\r\n"
         [
           "BEGIN:VCALENDAR";
           "VERSION:2.0";
           "PRODID:-//Caledonia undated journal test//EN";
           "BEGIN:VJOURNAL";
           "UID:undated-journal";
           "DTSTAMP:20260101T000000Z";
           "SUMMARY:Needle";
           "END:VJOURNAL";
           "END:VCALENDAR";
           "";
         ])
  in
  let criteria =
    Component_query.
      {
        no_criteria with
        component_types = [ Component_kind.Journal ];
        id = Some "undated-journal";
      }
  in
  let run include_undated_journals =
    query ~criteria ~include_undated_journals
      ~to_:(instant (2026, 2, 1) (0, 0, 0))
      ~timezone:Timedesc.Time_zone.utc components
    |> Result.get_ok |> List.length
  in
  Printf.printf "excluded=%d included=%d\n" (run false) (run true);
  [%expect {| excluded=0 included=1 |}]

let%expect_test "todo ancestor mode preserves cross-type stable ties" =
  Eio_main.run @@ fun env ->
  let components =
    components_of_ics (Eio.Stdenv.fs env)
      (String.concat "\r\n"
         [
           "BEGIN:VCALENDAR";
           "VERSION:2.0";
           "PRODID:-//Caledonia cross type order test//EN";
           "BEGIN:VEVENT";
           "UID:event-first";
           "DTSTAMP:20260101T000000Z";
           "DTSTART:20260102T090000Z";
           "SUMMARY:Tie";
           "END:VEVENT";
           "BEGIN:VTODO";
           "UID:todo-second";
           "DTSTAMP:20260101T000000Z";
           "DTSTART:20260102T090000Z";
           "SUMMARY:Tie";
           "END:VTODO";
           "END:VCALENDAR";
           "";
         ])
  in
  let selected =
    query ~include_todo_ancestors:true
      ~from:(Some (instant (2026, 1, 2) (0, 0, 0)))
      ~to_:(instant (2026, 1, 3) (0, 0, 0))
      ~timezone:Timedesc.Time_zone.utc components
    |> Result.get_ok
  in
  print_endline (String.concat "," (List.map Component_query.get_id selected));
  [%expect {| event-first,todo-second |}]

let duration_sort_ics =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia query tests//EN";
      "BEGIN:VTODO";
      "UID:duration-long";
      "DTSTAMP:20260101T000000Z";
      "DTSTART:20260102T090000Z";
      "DURATION:PT4H";
      "SUMMARY:Duration ends at 13:00";
      "END:VTODO";
      "BEGIN:VTODO";
      "UID:due-short";
      "DTSTAMP:20260101T000000Z";
      "DTSTART:20260102T100000Z";
      "DUE:20260102T110000Z";
      "SUMMARY:Due ends at 11:00";
      "END:VTODO";
      "END:VCALENDAR";
      "";
    ]

let%expect_test "end sort computes VTODO DTSTART plus DURATION" =
  Eio_main.run @@ fun env ->
  let components = components_of_ics (Eio.Stdenv.fs env) duration_sort_ics in
  let criteria =
    Component_query.
      { no_criteria with component_types = [ Component_kind.Todo ] }
  in
  let selected =
    query ~criteria
      ~from:(Some (instant (2026, 1, 2) (0, 0, 0)))
      ~to_:(instant (2026, 1, 3) (0, 0, 0))
      ~sort:[ Component_query.{ field = End; descending = false } ]
      ~timezone:Timedesc.Time_zone.utc components
    |> Result.get_ok
  in
  print_endline (summaries selected);
  [%expect {| Due ends at 11:00, Duration ends at 13:00 |}]

let sortable_document ~kind ~uid ~start ~end_property ~summary ?location () =
  String.concat "\r\n"
    ([
       "BEGIN:VCALENDAR";
       "VERSION:2.0";
       "PRODID:-//Caledonia comparator tests//EN";
       "BEGIN:" ^ kind;
       "UID:" ^ uid;
       "DTSTAMP:20260101T000000Z";
       "DTSTART:" ^ start;
       end_property;
       "SUMMARY:" ^ summary;
     ]
    @ Option.fold ~none:[] ~some:(fun value -> [ "LOCATION:" ^ value ]) location
    @ [ "END:" ^ kind; "END:VCALENDAR"; "" ])

let%expect_test
    "every advertised comparator reverses cleanly and ties remain stable" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let event =
    sortable_document ~kind:"VEVENT" ~uid:"z-event" ~start:"20260102T100000Z"
      ~end_property:"DTEND:20260102T110000Z" ~summary:"Zebra" ~location:"Zoo" ()
    |> components_of_ics_key fs "z-calendar"
    |> List.hd
  in
  let todo =
    sortable_document ~kind:"VTODO" ~uid:"a-todo" ~start:"20260102T090000Z"
      ~end_property:"DUE:20260102T100000Z" ~summary:"Alpha" ()
    |> components_of_ics_key fs "a-calendar"
    |> List.hd
  in
  let journal =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Caledonia comparator tests//EN";
        "BEGIN:VJOURNAL";
        "UID:m-journal";
        "DTSTAMP:20260101T000000Z";
        "DTSTART:20260102T093000Z";
        "SUMMARY:Middle";
        "END:VJOURNAL";
        "END:VCALENDAR";
        "";
      ]
    |> components_of_ics_key fs "m-calendar"
    |> List.hd
  in
  let to_ = instant (2026, 1, 3) (0, 0, 0) in
  let fields =
    Component_query.[ Start; End; Summary_sort; Location_sort; Calendar; Type ]
  in
  let item_ids values = List.map Component_query.get_id values in
  let component_ids values = List.map Component.get_id values in
  let reverses =
    List.for_all
      (fun field ->
        let run descending =
          query ~to_
            ~sort:[ Component_query.{ field; descending } ]
            ~timezone:Timedesc.Time_zone.utc [ event; todo ]
          |> Result.get_ok |> item_ids
        in
        run true = List.rev (run false))
      fields
  in
  let end_order =
    query ~to_
      ~sort:[ Component_query.{ field = End; descending = false } ]
      ~timezone:Timedesc.Time_zone.utc [ journal; event; todo ]
    |> Result.get_ok |> item_ids
  in
  let ties_source = [ todo; event; journal ] in
  let tied =
    query ~to_ ~sort:[] ~timezone:Timedesc.Time_zone.utc ties_source
    |> Result.get_ok |> item_ids
  in
  Printf.printf
    "all-six-asc-desc=%b journal-end-null-last=%b stable-input-tie=%b\n"
    reverses
    (end_order = [ "a-todo"; "z-event"; "m-journal" ])
    (tied = component_ids ties_source);
  [%expect
    {|
    all-six-asc-desc=true journal-end-null-last=true stable-input-tie=true |}]

let%expect_test "multi-key comparator respects declared precedence" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let make uid start summary =
    sortable_document ~kind:"VEVENT" ~uid ~start ~end_property:"DURATION:PT1H"
      ~summary ()
    |> components_of_ics_key fs "work"
    |> List.hd
  in
  let components =
    [
      make "a-late" "20260102T100000Z" "Alpha";
      make "beta-early" "20260102T080000Z" "Beta";
      make "a-early" "20260102T090000Z" "Alpha";
    ]
  in
  let selected =
    query
      ~to_:(instant (2026, 1, 3) (0, 0, 0))
      ~sort:
        Component_query.
          [
            { field = Summary_sort; descending = false };
            { field = Start; descending = false };
          ]
      ~timezone:Timedesc.Time_zone.utc components
    |> Result.get_ok
  in
  print_endline (String.concat "," (List.map Component_query.get_id selected));
  [%expect {| a-early,a-late,beta-early |}]

let rec permutations = function
  | [] -> [ [] ]
  | values ->
      List.concat_map
        (fun selected ->
          let rec remove_first accumulated = function
            | [] -> []
            | value :: rest when value == selected ->
                List.rev_append accumulated rest
            | value :: rest -> remove_first (value :: accumulated) rest
          in
          permutations (remove_first [] values)
          |> List.map (fun rest -> selected :: rest))
        values

let%expect_test
    "comparator permutation properties hold for every advertised key" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let make_event key uid hour summary location =
    sortable_document ~kind:"VEVENT" ~uid
      ~start:(Printf.sprintf "20260102T%02d0000Z" hour)
      ~end_property:(Printf.sprintf "DTEND:20260102T%02d0000Z" (hour + 1))
      ~summary ~location ()
    |> components_of_ics_key fs key
    |> List.hd
  in
  let ordered =
    [
      make_event "a" "a" 8 "Alpha" "A";
      make_event "b" "b" 9 "Beta" "B";
      make_event "c" "c" 10 "Gamma" "C";
    ]
  in
  let type_ordered =
    let event = List.hd ordered in
    let todo =
      sortable_document ~kind:"VTODO" ~uid:"todo" ~start:"20260102T090000Z"
        ~end_property:"DUE:20260102T100000Z" ~summary:"Todo" ()
      |> components_of_ics_key fs "work"
      |> List.hd
    in
    let journal =
      String.concat "\r\n"
        [
          "BEGIN:VCALENDAR";
          "VERSION:2.0";
          "PRODID:-//Caledonia comparator property tests//EN";
          "BEGIN:VJOURNAL";
          "UID:journal";
          "DTSTAMP:20260101T000000Z";
          "DTSTART:20260102T090000Z";
          "SUMMARY:Journal";
          "END:VJOURNAL";
          "END:VCALENDAR";
          "";
        ]
      |> components_of_ics_key fs "work"
      |> List.hd
    in
    [ event; todo; journal ]
  in
  let to_ = instant (2026, 1, 3) (0, 0, 0) in
  let sorted field descending values =
    query ~to_
      ~sort:[ Component_query.{ field; descending } ]
      ~timezone:Timedesc.Time_zone.utc values
    |> Result.get_ok
    |> List.map Component_query.get_id
  in
  let unique_properties =
    Component_query.[ Start; End; Summary_sort; Location_sort; Calendar ]
    |> List.for_all (fun field ->
        let expected = List.map Component.get_id ordered in
        permutations ordered
        |> List.for_all (fun input ->
            sorted field false input = expected
            && sorted field true input = List.rev expected))
  in
  let type_property =
    let expected = List.map Component.get_id type_ordered in
    permutations type_ordered
    |> List.for_all (fun input ->
        sorted Component_query.Type false input = expected
        && sorted Component_query.Type true input = List.rev expected)
  in
  let stable_tie_property =
    permutations ordered
    |> List.for_all (fun input ->
        sorted Component_query.Type false input
        = List.map Component.get_id input)
  in
  Printf.printf "unique/type/stable properties: %b/%b/%b\n" unique_properties
    type_property stable_tie_property;
  [%expect {| unique/type/stable properties: true/true/true |}]
