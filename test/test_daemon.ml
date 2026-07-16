open Caledonia_lib

let instant date time = Option.get (Ptime.of_date_time (date, (time, 0)))

let components_of_ics fs source =
  let calendar = Result.get_ok (Calendar_codec.Legacy.parse source) in
  let component_source =
    Component_source.of_decoded_document ~calendar_key:"daemon"
      ~file:Eio.Path.(fs / "daemon.ics")
      ~fingerprint:(Digest.string source |> Digest.to_hex)
      ()
  in
  Component.stored_views_of_decoded_components ~source:component_source
    (snd calendar)
  |> Result.get_ok

let duplicate_alarm_ics =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia daemon tests//EN";
      "BEGIN:VEVENT";
      "UID:duplicate-alarms";
      "DTSTAMP:20260715T080000Z";
      "DTSTART:20260715T100000Z";
      "SUMMARY:Duplicate alarms";
      "BEGIN:VALARM";
      "ACTION:DISPLAY";
      "TRIGGER:-PT1M";
      "DESCRIPTION:Reminder";
      "END:VALARM";
      "BEGIN:VALARM";
      "ACTION:DISPLAY";
      "TRIGGER:-PT1M";
      "DESCRIPTION:Reminder";
      "END:VALARM";
      "END:VEVENT";
      "END:VCALENDAR";
      "";
    ]

let recurring_alarm_ics =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia daemon tests//EN";
      "BEGIN:VEVENT";
      "UID:recurring-alarms";
      "DTSTAMP:20260715T080000Z";
      "DTSTART:20260715T100000Z";
      "RRULE:FREQ=DAILY;COUNT=2";
      "SUMMARY:Recurring alarm";
      "BEGIN:VALARM";
      "ACTION:DISPLAY";
      "TRIGGER:-PT1M";
      "DESCRIPTION:Reminder";
      "END:VALARM";
      "END:VEVENT";
      "END:VCALENDAR";
      "";
    ]

let none_and_display_alarm_ics =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia daemon tests//EN";
      "BEGIN:VEVENT";
      "UID:none-and-display";
      "DTSTAMP:20260715T080000Z";
      "DTSTART:20260715T100000Z";
      "SUMMARY:No-op policy";
      "BEGIN:VALARM";
      "ACTION:NONE";
      "TRIGGER:-PT1M";
      "END:VALARM";
      "BEGIN:VALARM";
      "ACTION:DISPLAY";
      "TRIGGER:-PT1M";
      "DESCRIPTION:Reminder";
      "END:VALARM";
      "END:VEVENT";
      "END:VCALENDAR";
      "";
    ]

let fires_in_range components ~from ~to_ =
  Alarm_query.run_result ~floating_tz:Timedesc.Time_zone.utc ~from:(Some from)
    ~to_ components
  |> Result.get_ok

let%expect_test "stdout notification fields replace terminal controls" =
  print_endline
    (Format_utils.sanitize_terminal
       "Hostile \027ESC \194\155CSI \226\128\174BIDI \007BELL");
  [%expect {| Hostile ?ESC ?CSI ?BIDI ?BELL |}]

let%expect_test "terminal sanitization separates trusted rows from fields" =
  let zwj_emoji = "👩‍👩‍👧‍👦" in
  print_endline
    (Format_utils.sanitize_terminal
       ("trusted\nrow\t\r\027\226\128\174 " ^ zwj_emoji));
  print_endline
    (Format_utils.sanitize_terminal_line
       ("field\n\ttail\226\128\174 " ^ zwj_emoji));
  Printf.printf "widths combining=%d family=%d flag=%d keycap=%d\n"
    (Format_utils.display_width "é")
    (Format_utils.display_width zwj_emoji)
    (Format_utils.display_width "🇬🇧")
    (Format_utils.display_width "1️⃣");
  [%expect
    {|
    trusted
    row???? 👩‍👩‍👧‍👦
    field??tail? 👩‍👩‍👧‍👦
    widths combining=1 family=2 flag=2 keycap=2
    |}]

let driver ~now ~initial ~fires ~notify ~saved ~wait ~sleeps ~reports =
  Alarm_daemon_core.
    {
      now = (fun () -> now);
      wait;
      sleep = (fun seconds -> sleeps := seconds :: !sleeps);
      load_state = (fun () -> Some initial);
      save_state =
        (fun state ->
          saved := state :: !saved;
          Ok ());
      load_fires =
        (fun ~from ~to_ ->
          Ok
            (List.filter
               (fun (fire : Alarm_query.fire) ->
                 Ptime.compare fire.fire_time from >= 0
                 && Ptime.compare fire.fire_time to_ < 0)
               fires));
      notify;
      report = (fun message -> reports := message :: !reports);
    }

let persisted_schema_v2_with_duplicate_alarm_fires =
  {|{
    "schema_version": 2,
    "watermark": "2026-07-15T09:58:00Z",
    "fired": [
      [
        "daemon",
        "daemon.ics",
        "duplicate-alarms",
        "master",
        0,
        "374ec4007495cc6225a9e035188598fa",
        "2026-07-15T09:59:00Z"
      ],
      [
        "daemon",
        "daemon.ics",
        "duplicate-alarms",
        "master",
        1,
        "374ec4007495cc6225a9e035188598fa",
        "2026-07-15T09:59:00Z"
      ]
    ],
    "pending": []
  }|}

let%expect_test
    "hand-authored persisted schema-v2 fired keys do not replay after restart" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let components = components_of_ics fs duplicate_alarm_ics in
  let from = instant (2026, 7, 15) (9, 58, 0) in
  let now = instant (2026, 7, 15) (10, 0, 0) in
  let fires = fires_in_range components ~from ~to_:now in
  let persisted =
    persisted_schema_v2_with_duplicate_alarm_fires |> Yojson.Safe.from_string
    |> Alarm_daemon_core.state_of_json |> Option.get
  in
  let attempts = ref 0 in
  let saved = ref [] in
  let final =
    Alarm_daemon_core.run ~max_cycles:1 ~grace:600 ~rescan_interval:60.
      (driver ~now ~initial:persisted ~fires ~saved ~sleeps:(ref [])
         ~reports:(ref [])
         ~wait:(fun ~timeout:_ -> Alarm_daemon_core.Timer)
         ~notify:(fun _ ->
           incr attempts;
           Ok ()))
    |> Result.get_ok
  in
  Printf.printf
    "loaded=%d matching-fires=%d replay-attempts=%d fired=%d pending=%d \
     watermark-advanced=%b saved=%d\n"
    (Alarm_daemon_core.fired_count persisted)
    (List.length fires) !attempts
    (Alarm_daemon_core.fired_count final)
    (Alarm_daemon_core.pending_count final)
    (Ptime.equal (Alarm_daemon_core.watermark final) now)
    (List.length !saved);
  [%expect
    {|
    loaded=2 matching-fires=2 replay-attempts=0 fired=2 pending=0 watermark-advanced=true saved=1
    |}]

let%expect_test
    "daemon persists mixed successes, retries only failures, and advances \
     watermark" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let components = components_of_ics fs duplicate_alarm_ics in
  let from = instant (2026, 7, 15) (9, 58, 0) in
  let now = instant (2026, 7, 15) (10, 0, 0) in
  let fires = fires_in_range components ~from ~to_:now in
  if List.length fires <> 2 then failwith "fixture did not preserve two alarms";
  let initial = Alarm_daemon_core.empty_state ~watermark:from in
  let saved = ref [] in
  let sleeps = ref [] in
  let reports = ref [] in
  let attempts = ref 0 in
  let first_driver =
    driver ~now ~initial ~fires ~saved ~sleeps ~reports
      ~wait:(fun ~timeout:_ -> Alarm_daemon_core.Timer)
      ~notify:(fun _ ->
        incr attempts;
        if !attempts = 1 then Ok () else Error (`Msg "temporary failure"))
  in
  let first =
    Result.get_ok
      (Alarm_daemon_core.run ~max_cycles:1 ~grace:600 ~rescan_interval:60.
         first_driver)
  in
  let persisted = List.hd !saved in
  let oldest_failed =
    List.fold_left
      (fun earliest (fire : Alarm_query.fire) ->
        match earliest with
        | None -> Some fire.fire_time
        | Some value ->
            Some
              (if Ptime.compare fire.fire_time value < 0 then fire.fire_time
               else value))
      None fires
    |> Option.get
  in
  Printf.printf
    "fires=%d attempts=%d fired=%d persisted=%d pending=%d \
     watermark-at-oldest-failure=%b\n"
    (List.length fires) !attempts
    (Alarm_daemon_core.fired_count first)
    (Alarm_daemon_core.fired_count persisted)
    (Alarm_daemon_core.pending_count persisted)
    (Ptime.equal (Alarm_daemon_core.watermark first) oldest_failed);
  let retry_attempts = ref 0 in
  let retry_saved = ref [] in
  let retry_driver =
    driver ~now ~initial:persisted ~fires ~saved:retry_saved ~sleeps ~reports
      ~wait:(fun ~timeout:_ -> Alarm_daemon_core.Timer)
      ~notify:(fun _ ->
        incr retry_attempts;
        Ok ())
  in
  let retried =
    Result.get_ok
      (Alarm_daemon_core.run ~max_cycles:1 ~grace:600 ~rescan_interval:60.
         retry_driver)
  in
  Printf.printf
    "retry-attempts=%d fired=%d pending=%d watermark-advanced=%b reports=%d\n"
    !retry_attempts
    (Alarm_daemon_core.fired_count retried)
    (Alarm_daemon_core.pending_count retried)
    (Ptime.equal (Alarm_daemon_core.watermark retried) now)
    (List.length !reports);
  [%expect
    {|
    fires=2 attempts=2 fired=1 persisted=1 pending=1 watermark-at-oldest-failure=true
    retry-attempts=1 fired=2 pending=0 watermark-advanced=true reports=1
    |}]

let%expect_test
    "permanent notifier failure is bounded across restart and stops replay" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let components = components_of_ics fs duplicate_alarm_ics in
  let from = instant (2026, 7, 15) (9, 58, 0) in
  let now = instant (2026, 7, 15) (10, 0, 0) in
  let fires = fires_in_range components ~from ~to_:now in
  let reports = ref [] in
  let attempts_before_restart = ref 0 in
  let before_restart =
    Result.get_ok
      (Alarm_daemon_core.run ~max_cycles:2 ~grace:600 ~rescan_interval:1.
         (driver ~now
            ~initial:(Alarm_daemon_core.empty_state ~watermark:from)
            ~fires ~saved:(ref []) ~sleeps:(ref []) ~reports
            ~wait:(fun ~timeout:_ -> Alarm_daemon_core.Timer)
            ~notify:(fun _ ->
              incr attempts_before_restart;
              Error (`Msg "backend unavailable"))))
  in
  let persisted =
    Alarm_daemon_core.state_to_json before_restart
    |> Alarm_daemon_core.state_of_json |> Option.get
  in
  let attempts_after_restart = ref 0 in
  let after_final_attempt =
    Result.get_ok
      (Alarm_daemon_core.run ~max_cycles:1 ~grace:600 ~rescan_interval:1.
         (driver ~now ~initial:persisted ~fires ~saved:(ref []) ~sleeps:(ref [])
            ~reports
            ~wait:(fun ~timeout:_ -> Alarm_daemon_core.Timer)
            ~notify:(fun _ ->
              incr attempts_after_restart;
              Error (`Msg "backend unavailable"))))
  in
  let attempts_after_drop = ref 0 in
  let after_rescan =
    Result.get_ok
      (Alarm_daemon_core.run ~max_cycles:1 ~grace:600 ~rescan_interval:1.
         (driver ~now ~initial:after_final_attempt ~fires ~saved:(ref [])
            ~sleeps:(ref []) ~reports
            ~wait:(fun ~timeout:_ -> Alarm_daemon_core.Timer)
            ~notify:(fun _ ->
              incr attempts_after_drop;
              Error (`Msg "must not be called"))))
  in
  let dropped_reports =
    List.filter (String.starts_with ~prefix:"Notification dropped") !reports
    |> List.length
  in
  Printf.printf
    "limit=%d before-restart-attempts=%d persisted-pending=%d \
     after-restart-attempts=%d final-pending=%d watermark-advanced=%b \
     dropped=%d later-attempts=%d stable-watermark=%b\n"
    Alarm_daemon_core.max_delivery_attempts !attempts_before_restart
    (Alarm_daemon_core.pending_count persisted)
    !attempts_after_restart
    (Alarm_daemon_core.pending_count after_final_attempt)
    (Ptime.equal (Alarm_daemon_core.watermark after_final_attempt) now)
    dropped_reports !attempts_after_drop
    (Ptime.equal
       (Alarm_daemon_core.watermark after_rescan)
       (Alarm_daemon_core.watermark after_final_attempt));
  [%expect
    {|
    limit=3 before-restart-attempts=4 persisted-pending=2 after-restart-attempts=2 final-pending=0 watermark-advanced=true dropped=2 later-attempts=0 stable-watermark=true
    |}]

let%expect_test "notifier deadlines use an injected monotonic clock" =
  let monotonic = ref 10. in
  let wall_clock = ref 1_000_000. in
  let now () = !monotonic in
  let deadline = Monotonic_deadline.after ~now 10. in
  wall_clock := -1_000_000.;
  let wall_clock_jump_ignored =
    not (Monotonic_deadline.reached ~now deadline)
  in
  monotonic := 19.999;
  let before = Monotonic_deadline.reached ~now deadline in
  monotonic := 20.;
  let at = Monotonic_deadline.reached ~now deadline in
  Printf.printf "wall-jump-ignored=%b before=%b at=%b\n" wall_clock_jump_ignored
    before at;
  [%expect {| wall-jump-ignored=true before=false at=true |}]

let%expect_test "reconciliation releases pending alarms removed from source" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let components = components_of_ics fs duplicate_alarm_ics in
  let from = instant (2026, 7, 15) (9, 58, 0) in
  let now = instant (2026, 7, 15) (10, 0, 0) in
  let fires = fires_in_range components ~from ~to_:now in
  let failed =
    Result.get_ok
      (Alarm_daemon_core.run ~max_cycles:1 ~grace:600 ~rescan_interval:1.
         (driver ~now
            ~initial:(Alarm_daemon_core.empty_state ~watermark:from)
            ~fires ~saved:(ref []) ~sleeps:(ref []) ~reports:(ref [])
            ~wait:(fun ~timeout:_ -> Alarm_daemon_core.Timer)
            ~notify:(fun _ -> Error (`Msg "backend unavailable"))))
  in
  let notify_after_removal = ref 0 in
  let reconciled =
    Result.get_ok
      (Alarm_daemon_core.run ~max_cycles:1 ~grace:600 ~rescan_interval:1.
         (driver ~now ~initial:failed ~fires:[] ~saved:(ref []) ~sleeps:(ref [])
            ~reports:(ref [])
            ~wait:(fun ~timeout:_ -> Alarm_daemon_core.Timer)
            ~notify:(fun _ ->
              incr notify_after_removal;
              Ok ())))
  in
  Printf.printf
    "pending-before=%d pending-after=%d attempts-after=%d advanced=%b\n"
    (Alarm_daemon_core.pending_count failed)
    (Alarm_daemon_core.pending_count reconciled)
    !notify_after_removal
    (Ptime.equal (Alarm_daemon_core.watermark reconciled) now);
  [%expect
    {| pending-before=2 pending-after=0 attempts-after=0 advanced=true |}]

let%expect_test "daemon records ACTION:NONE without invoking the notifier" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let components = components_of_ics fs none_and_display_alarm_ics in
  let from = instant (2026, 7, 15) (9, 58, 0) in
  let now = instant (2026, 7, 15) (10, 0, 0) in
  let fires = fires_in_range components ~from ~to_:now in
  let none_count =
    List.fold_left
      (fun count (fire : Alarm_query.fire) ->
        match fire.alarm with `None _ -> count + 1 | _ -> count)
      0 fires
  in
  let none_fires =
    List.filter
      (fun (fire : Alarm_query.fire) ->
        match fire.alarm with `None _ -> true | _ -> false)
      fires
  in
  let none_only_attempts = ref 0 in
  let initial = Alarm_daemon_core.empty_state ~watermark:from in
  let none_only =
    Result.get_ok
      (Alarm_daemon_core.run ~max_cycles:1 ~grace:600 ~rescan_interval:60.
         (driver ~now ~initial ~fires:none_fires ~saved:(ref [])
            ~sleeps:(ref []) ~reports:(ref [])
            ~wait:(fun ~timeout:_ -> Alarm_daemon_core.Timer)
            ~notify:(fun _ ->
              incr none_only_attempts;
              Ok ())))
  in
  let attempts = ref 0 in
  let final =
    Result.get_ok
      (Alarm_daemon_core.run ~max_cycles:1 ~grace:600 ~rescan_interval:60.
         (driver ~now ~initial ~fires ~saved:(ref []) ~sleeps:(ref [])
            ~reports:(ref [])
            ~wait:(fun ~timeout:_ -> Alarm_daemon_core.Timer)
            ~notify:(fun _ ->
              incr attempts;
              Ok ())))
  in
  Printf.printf
    "fires=%d none=%d none-only-attempts=%d none-only-recorded=%d \
     mixed-attempts=%d recorded=%d watermark-advanced=%b\n"
    (List.length fires) none_count !none_only_attempts
    (Alarm_daemon_core.fired_count none_only)
    !attempts
    (Alarm_daemon_core.fired_count final)
    (Ptime.equal (Alarm_daemon_core.watermark final) now);
  [%expect
    {|
    fires=2 none=1 none-only-attempts=0 none-only-recorded=1 mixed-attempts=1 recorded=2 watermark-advanced=true
    |}]

let%expect_test "throwing notifier does not stop later alarms and is retried" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let components = components_of_ics fs duplicate_alarm_ics in
  let from = instant (2026, 7, 15) (9, 58, 0) in
  let now = instant (2026, 7, 15) (10, 0, 0) in
  let fires = fires_in_range components ~from ~to_:now in
  let initial = Alarm_daemon_core.empty_state ~watermark:from in
  let attempts = ref 0 in
  let reports = ref [] in
  let first =
    Result.get_ok
      (Alarm_daemon_core.run ~max_cycles:1 ~grace:600 ~rescan_interval:60.
         (driver ~now ~initial ~fires ~saved:(ref []) ~sleeps:(ref []) ~reports
            ~wait:(fun ~timeout:_ -> Alarm_daemon_core.Timer)
            ~notify:(fun _ ->
              incr attempts;
              if !attempts = 1 then failwith "spawn failed" else Ok ())))
  in
  let retry_attempts = ref 0 in
  let final =
    Result.get_ok
      (Alarm_daemon_core.run ~max_cycles:1 ~grace:600 ~rescan_interval:60.
         (driver ~now ~initial:first ~fires ~saved:(ref []) ~sleeps:(ref [])
            ~reports
            ~wait:(fun ~timeout:_ -> Alarm_daemon_core.Timer)
            ~notify:(fun _ ->
              incr retry_attempts;
              Ok ())))
  in
  Printf.printf
    "first-attempts=%d later-delivered=%d retry-attempts=%d final=%d reported=%b\n"
    !attempts
    (Alarm_daemon_core.fired_count first)
    !retry_attempts
    (Alarm_daemon_core.fired_count final)
    (List.exists (String.starts_with ~prefix:"Notification failed") !reports);
  [%expect
    {|
    first-attempts=2 later-delivered=1 retry-attempts=1 final=2 reported=true
    |}]

let%expect_test
    "identical VALARMs and recurrence instances have distinct persisted \
     identities" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let duplicate_components = components_of_ics fs duplicate_alarm_ics in
  let duplicate_fires =
    fires_in_range duplicate_components
      ~from:(instant (2026, 7, 15) (9, 58, 0))
      ~to_:(instant (2026, 7, 15) (10, 0, 0))
  in
  let duplicate_distinct =
    match duplicate_fires with
    | [ left; right ] ->
        left.alarm_index <> right.alarm_index
        && Alarm_daemon_core.distinct_fire_identity left right
    | _ -> false
  in
  let recurring_components = components_of_ics fs recurring_alarm_ics in
  let recurring_fires =
    fires_in_range recurring_components
      ~from:(instant (2026, 7, 15) (9, 58, 0))
      ~to_:(instant (2026, 7, 16) (10, 0, 0))
  in
  let recurrence_distinct =
    match recurring_fires with
    | [ left; right ] -> Alarm_daemon_core.distinct_fire_identity left right
    | _ -> false
  in
  Printf.printf "duplicate-count=%d ordinal-and-key-distinct=%b\n"
    (List.length duplicate_fires)
    duplicate_distinct;
  Printf.printf "recurrence-count=%d recurrence-key-distinct=%b\n"
    (List.length recurring_fires)
    recurrence_distinct;
  [%expect
    {|
    duplicate-count=2 ordinal-and-key-distinct=true
    recurrence-count=2 recurrence-key-distinct=true
    |}]

let%expect_test
    "replay grace longer than one day retains successful partial deliveries" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let components = components_of_ics fs duplicate_alarm_ics in
  let replay_start = instant (2026, 7, 14) (0, 0, 0) in
  let now = instant (2026, 7, 18) (10, 0, 0) in
  let fires = fires_in_range components ~from:replay_start ~to_:now in
  let initial = Alarm_daemon_core.empty_state ~watermark:replay_start in
  let saved = ref [] in
  let sleeps = ref [] in
  let reports = ref [] in
  let attempts = ref 0 in
  let first_driver =
    driver ~now ~initial ~fires ~saved ~sleeps ~reports
      ~wait:(fun ~timeout:_ -> Alarm_daemon_core.Timer)
      ~notify:(fun _ ->
        incr attempts;
        if !attempts = 1 then Ok () else Error (`Msg "retry later"))
  in
  let first =
    Result.get_ok
      (Alarm_daemon_core.run ~max_cycles:1
         ~grace:(4 * 24 * 60 * 60)
         ~rescan_interval:60. first_driver)
  in
  let retry_attempts = ref 0 in
  let retry_driver =
    driver ~now ~initial:first ~fires ~saved:(ref []) ~sleeps ~reports
      ~wait:(fun ~timeout:_ -> Alarm_daemon_core.Timer)
      ~notify:(fun _ ->
        incr retry_attempts;
        Ok ())
  in
  let final =
    Result.get_ok
      (Alarm_daemon_core.run ~max_cycles:1
         ~grace:(4 * 24 * 60 * 60)
         ~rescan_interval:60. retry_driver)
  in
  Printf.printf
    "old-fire-age-hours=%d retained-after-partial=%d retry-attempts=%d final=%d\n"
    72
    (Alarm_daemon_core.fired_count first)
    !retry_attempts
    (Alarm_daemon_core.fired_count final);
  [%expect
    {|
    old-fire-age-hours=72 retained-after-partial=1 retry-attempts=1 final=2
    |}]

let%expect_test
    "daemon skips one malformed file and still schedules valid alarms" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let root = Filename.temp_file "caledonia-daemon-load-" "" in
  Sys.remove root;
  Sys.mkdir root 0o700;
  Fun.protect
    ~finally:(fun () ->
      let rec remove path =
        if Sys.is_directory path then (
          Sys.readdir path
          |> Array.iter (fun name -> remove (Filename.concat path name));
          Unix.rmdir path)
        else Unix.unlink path
      in
      remove root)
    (fun () ->
      let calendar = Filename.concat root "personal" in
      Sys.mkdir calendar 0o700;
      Out_channel.with_open_bin (Filename.concat calendar "good.ics")
        (fun channel -> output_string channel duplicate_alarm_ics);
      Out_channel.with_open_bin (Filename.concat calendar "bad.ics")
        (fun channel -> output_string channel "BEGIN:VCALENDAR\r\nBROKEN\r\n");
      let calendar_dir = Result.get_ok (Calendar_dir.create ~fs root) in
      let reports = ref [] in
      let tolerant =
        Result.get_ok
          (Calendar_dir.get_components_tolerant
             ~report:(fun message -> reports := message :: !reports)
             ~fs calendar_dir)
      in
      let strict_failed =
        Result.is_error (Calendar_dir.get_components ~fs calendar_dir)
      in
      let fires =
        fires_in_range tolerant
          ~from:(instant (2026, 7, 15) (9, 58, 0))
          ~to_:(instant (2026, 7, 15) (10, 0, 0))
      in
      Printf.printf
        "strict-failed=%b valid-components=%d fires=%d diagnostics=%d\n"
        strict_failed (List.length tolerant) (List.length fires)
        (List.length !reports));
  [%expect
    {|
    strict-failed=true valid-components=1 fires=2 diagnostics=1
    |}]

let%expect_test "daemon reports watcher failures and continues its scan loop" =
  let now = instant (2026, 7, 15) (10, 0, 0) in
  let initial = Alarm_daemon_core.empty_state ~watermark:now in
  let saved = ref [] in
  let sleeps = ref [] in
  let reports = ref [] in
  let waits = ref 0 in
  let driver =
    driver ~now ~initial ~fires:[] ~saved ~sleeps ~reports
      ~notify:(fun _ -> Ok ())
      ~wait:(fun ~timeout ->
        incr waits;
        Printf.printf "wait-timeout=%.0f\n" timeout;
        if !waits = 1 then
          Alarm_daemon_core.Watcher_error "injected rebuild failure"
        else Alarm_daemon_core.Changed)
  in
  let final =
    Result.get_ok
      (Alarm_daemon_core.run ~max_cycles:3 ~grace:300 ~rescan_interval:17.
         driver)
  in
  let round_trip =
    Alarm_daemon_core.state_to_json final
    |> Alarm_daemon_core.state_of_json |> Option.get
  in
  Printf.printf
    "waits=%d sleeps=%s saves=%d reports=%d failure-reported=%b \
     state-roundtrip=%b\n"
    !waits
    (String.concat "," (List.map string_of_float (List.rev !sleeps)))
    (List.length !saved) (List.length !reports)
    (List.exists
       (String.equal
          "watcher failure; full rescan is running: injected rebuild failure")
       !reports)
    (Ptime.equal
       (Alarm_daemon_core.watermark round_trip)
       (Alarm_daemon_core.watermark final));
  [%expect
    {|
    wait-timeout=17
    wait-timeout=17
    waits=2 sleeps=0.2 saves=3 reports=1 failure-reported=true state-roundtrip=true
    |}]

let%expect_test "corrupt or unsupported daemon state falls back to grace" =
  let now = instant (2026, 7, 15) (10, 0, 0) in
  let legacy =
    `Assoc
      [
        ("schema_version", `Int 1);
        ("watermark", `String (Ptime.to_rfc3339 now));
        ("fired", `List []);
      ]
  in
  let migrated = Alarm_daemon_core.state_of_json legacy |> Option.get in
  let migrated_schema =
    match Alarm_daemon_core.state_to_json migrated with
    | `Assoc fields -> List.assoc_opt "schema_version" fields
    | _ -> None
  in
  Printf.printf "legacy-v1-migrated=%b pending=%d\n"
    (migrated_schema = Some (`Int 2))
    (Alarm_daemon_core.pending_count migrated);
  let unsupported =
    `Assoc
      [
        ("schema_version", `Int 3);
        ("watermark", `String (Ptime.to_rfc3339 now));
        ("fired", `List []);
      ]
  in
  let saved = ref [] in
  let reports = ref [] in
  let driver : Alarm_daemon_core.driver =
    {
      now = (fun () -> now);
      wait = (fun ~timeout:_ -> Alarm_daemon_core.Timer);
      sleep = ignore;
      load_state = (fun () -> Alarm_daemon_core.state_of_json unsupported);
      save_state =
        (fun state ->
          saved := state :: !saved;
          Ok ());
      load_fires = (fun ~from:_ ~to_:_ -> Ok []);
      notify = (fun _ -> Ok ());
      report = (fun message -> reports := message :: !reports);
    }
  in
  let state =
    Result.get_ok
      (Alarm_daemon_core.run ~max_cycles:1 ~grace:300 ~rescan_interval:60.
         driver)
  in
  let expected = Option.get (Ptime.sub_span now (Ptime.Span.of_int_s 300)) in
  Printf.printf "unsupported-rejected=%b successful-scan-advanced=%b saved=%d\n"
    (Alarm_daemon_core.state_of_json unsupported = None)
    (Ptime.equal (Alarm_daemon_core.watermark state) now)
    (List.length !saved);
  let malformed_entry =
    `Assoc
      [
        ("schema_version", `Int 1);
        ("watermark", `String (Ptime.to_rfc3339 now));
        ("fired", `List [ `List [ `String "truncated" ] ]);
      ]
  in
  Printf.printf "malformed fired entry rejected=%b\n"
    (Alarm_daemon_core.state_of_json malformed_entry = None);
  let initial_seen = ref None in
  let inspect_driver =
    {
      driver with
      load_fires =
        (fun ~from ~to_:_ ->
          initial_seen := Some from;
          Ok []);
    }
  in
  ignore
    (Result.get_ok
       (Alarm_daemon_core.run ~max_cycles:1 ~grace:300 ~rescan_interval:60.
          inspect_driver));
  Printf.printf "scan-started-at-grace=%b\n"
    (Option.fold ~none:false ~some:(Ptime.equal expected) !initial_seen);
  [%expect
    {|
    legacy-v1-migrated=true pending=0
    unsupported-rejected=true successful-scan-advanced=true saved=1
    malformed fired entry rejected=true
    scan-started-at-grace=true
    |}]

let%expect_test "daemon state writes atomically and cleans failed temporaries" =
  let root = Filename.temp_file "caledonia-daemon-state-" "" in
  Sys.remove root;
  Sys.mkdir root 0o700;
  Fun.protect
    ~finally:(fun () ->
      Sys.readdir root
      |> Array.iter (fun name ->
          let path = Filename.concat root name in
          if (Unix.lstat path).st_kind = Unix.S_DIR then Unix.rmdir path
          else Unix.unlink path);
      Unix.rmdir root)
    (fun () ->
      let now = instant (2026, 7, 15) (10, 0, 0) in
      let state = Alarm_daemon_core.empty_state ~watermark:now in
      let path = Filename.concat root "state.json" in
      let saved = Alarm_daemon_core.save_state_file path state = Ok () in
      let loaded = Alarm_daemon_core.load_state_file path in
      let mode = (Unix.stat path).st_perm land 0o777 in
      let victim = Filename.concat root "victim" in
      Out_channel.with_open_bin victim (fun channel ->
          output_string channel "safe");
      Unix.symlink victim (path ^ ".tmp");
      let stale_symlink_safe =
        Alarm_daemon_core.save_state_file path state = Ok ()
        && In_channel.with_open_bin victim In_channel.input_all = "safe"
      in
      let occupied = Filename.concat root "occupied" in
      Sys.mkdir occupied 0o700;
      let failed =
        match Alarm_daemon_core.save_state_file occupied state with
        | Error (`Msg _) -> true
        | Ok () -> false
      in
      Printf.printf
        "saved=%b loaded=%b mode=%03o stale-symlink-safe=%b failed=%b \
         tmp-clean=%b\n"
        saved (Option.is_some loaded) mode stale_symlink_safe failed
        (not (Sys.file_exists (occupied ^ ".tmp"))));
  [%expect
    {|
    saved=true loaded=true mode=600 stale-symlink-safe=true failed=true tmp-clean=true
    |}]
