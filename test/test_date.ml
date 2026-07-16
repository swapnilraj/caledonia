open Caledonia_lib

let fixed_date = Option.get @@ Ptime.of_date_time ((2025, 3, 27), ((0, 0, 0), 0))
let london = Timedesc.Time_zone.make_exn "Europe/London"

let parse_fixed expr parameter =
  Date.parse_date ~tz:london ~now:fixed_date expr parameter

let format_london_date instant =
  let dt = Date.ptime_to_timedesc ~tz:london instant in
  Printf.sprintf "%04d-%02d-%02d" (Timedesc.year dt) (Timedesc.month dt)
    (Timedesc.day dt)

let%expect_test "parse date expressions" =
  let test_expr expr parameter =
    let result = Result.get_ok @@ parse_fixed expr parameter in
    format_london_date result
  in

  Printf.printf "today (from): %s\n" (test_expr "today" `From);
  Printf.printf "today (to): %s\n" (test_expr "today" `To);
  Printf.printf "tomorrow (from): %s\n" (test_expr "tomorrow" `From);
  Printf.printf "tomorrow (to): %s\n" (test_expr "tomorrow" `To);
  Printf.printf "yesterday (from): %s\n" (test_expr "yesterday" `From);
  Printf.printf "yesterday (to): %s\n" (test_expr "yesterday" `To);

  [%expect
    {|
    today (from): 2025-03-27
    today (to): 2025-03-27
    tomorrow (from): 2025-03-28
    tomorrow (to): 2025-03-28
    yesterday (from): 2025-03-26
    yesterday (to): 2025-03-26 |}]

let%expect_test "parse week expressions" =
  let test_expr expr parameter =
    let result = Result.get_ok @@ parse_fixed expr parameter in
    format_london_date result
  in

  Printf.printf "this-week (from): %s\n" (test_expr "this-week" `From);
  Printf.printf "this-week (to): %s\n" (test_expr "this-week" `To);
  Printf.printf "next-week (from): %s\n" (test_expr "next-week" `From);
  Printf.printf "next-week (to): %s\n" (test_expr "next-week" `To);

  [%expect
    {|
    this-week (from): 2025-03-24
    this-week (to): 2025-03-30
    next-week (from): 2025-03-31
    next-week (to): 2025-04-06 |}]

let%expect_test "parse month expressions" =
  let test_expr expr parameter =
    let result = Result.get_ok @@ parse_fixed expr parameter in
    format_london_date result
  in

  Printf.printf "this-month (from): %s\n" (test_expr "this-month" `From);
  Printf.printf "this-month (to): %s\n" (test_expr "this-month" `To);
  Printf.printf "next-month (from): %s\n" (test_expr "next-month" `From);
  Printf.printf "next-month (to): %s\n" (test_expr "next-month" `To);

  [%expect
    {|
    this-month (from): 2025-03-01
    this-month (to): 2025-03-31
    next-month (from): 2025-04-01
    next-month (to): 2025-04-30 |}]

let%expect_test "parse relative date expressions" =
  let test_expr expr parameter =
    let result = Result.get_ok @@ parse_fixed expr parameter in
    format_london_date result
  in

  Printf.printf "+7d: %s\n" (test_expr "+7d" `From);
  Printf.printf "-7d: %s\n" (test_expr "-7d" `From);
  Printf.printf "+2w (from): %s\n" (test_expr "+2w" `From);
  Printf.printf "+2w (to): %s\n" (test_expr "+2w" `To);
  Printf.printf "+1m (from): %s\n" (test_expr "+1m" `From);
  Printf.printf "+1m (to): %s\n" (test_expr "+1m" `To);

  [%expect
    {|
    +7d: 2025-04-03
    -7d: 2025-03-20
    +2w (from): 2025-04-07
    +2w (to): 2025-04-13
    +1m (from): 2025-04-01
    +1m (to): 2025-04-30 |}]

let%expect_test "oversized relative dates return a structured error" =
  (match parse_fixed "+999999999999999999999999d" `From with
  | Ok _ -> print_endline "unexpected success"
  | Error (`Msg message) -> print_endline message);
  [%expect
    {| out-of-range date/time (relative date +999999999999999999999999d) |}]

let%expect_test "parse absolute date expressions" =
  let test_expr expr parameter =
    let result = Result.get_ok @@ parse_fixed expr parameter in
    format_london_date result
  in

  Printf.printf "2025-01-01: %s\n" (test_expr "2025-01-01" `From);
  Printf.printf "2025-01: %s\n" (test_expr "2025-01" `From);
  Printf.printf "2025: %s\n" (test_expr "2025" `From);
  Printf.printf "2025-3-1: %s\n" (test_expr "2025-3-1" `From);

  [%expect
    {|
    2025-01-01: 2025-01-01
    2025-01: 2025-01-01
    2025: 2025-01-01
    2025-3-1: 2025-03-01 |}]

let%expect_test "inclusive CLI date is converted once to an exclusive bound" =
  let inclusive = Result.get_ok (parse_fixed "2025" `To) in
  let exclusive =
    Result.get_ok (Date.next_midnight_result ~tz:london inclusive)
  in
  let print_local label instant =
    let local = Date.ptime_to_timedesc ~tz:london instant in
    Printf.printf "%s: %04d-%02d-%02d %02d:%02d:%02d\n" label
      (Timedesc.year local) (Timedesc.month local) (Timedesc.day local)
      (Timedesc.hour local) (Timedesc.minute local) (Timedesc.second local)
  in
  print_local "inclusive date" inclusive;
  print_local "exclusive bound" exclusive;
  [%expect
    {|
    inclusive date: 2025-12-31 00:00:00
    exclusive bound: 2026-01-01 00:00:00 |}]

let%expect_test "invalid date format" =
  let result = parse_fixed "invalid-format" `From in
  (match result with
  | Error (`Msg msg) ->
      Printf.printf "Error (as expected): %s\n"
        (if String.length msg > 0 then "message received" else "empty message")
  | Ok _ -> Printf.printf "Unexpected success\n");
  [%expect {| Error (as expected): message received |}]

let%expect_test "timezone affects date calculations" =
  let utc = Timedesc.Time_zone.utc in
  let tokyo = Timedesc.Time_zone.make_exn "Asia/Tokyo" in
  (* UTC+9 *)
  let new_york = Timedesc.Time_zone.make_exn "America/New_York" in
  (* UTC-5 or UTC-4 *)

  (* Set a fixed UTC time: 2025-03-27 22:00:00 UTC *)
  (* This is 2025-03-28 07:00:00 in Tokyo (next day) *)
  (* This is 2025-03-27 18:00:00 in New York (same day) *)
  let fixed_utc_time =
    Option.get @@ Ptime.of_date_time ((2025, 3, 27), ((22, 0, 0), 0))
  in

  (* Test that "today" is different in different timezones.
     We extract the local date via ptime_to_timedesc since Ptime.to_date
     always returns the UTC date. *)
  let today_utc = Date.parse_date ~tz:utc ~now:fixed_utc_time "today" `From in
  let today_tokyo =
    Date.parse_date ~tz:tokyo ~now:fixed_utc_time "today" `From
  in
  let today_ny =
    Date.parse_date ~tz:new_york ~now:fixed_utc_time "today" `From
  in

  let print_date name tz result =
    match result with
    | Ok ptime ->
        let dt = Date.ptime_to_timedesc ~tz ptime in
        Printf.printf "%s: %04d-%02d-%02d\n" name (Timedesc.year dt)
          (Timedesc.month dt) (Timedesc.day dt)
    | Error (`Msg msg) -> Printf.printf "%s: Error - %s\n" name msg
  in

  print_date "Today UTC" utc today_utc;
  print_date "Today Tokyo" tokyo today_tokyo;
  print_date "Today New York" new_york today_ny;

  [%expect
    {|
    Today UTC: 2025-03-27
    Today Tokyo: 2025-03-28
    Today New York: 2025-03-27 |}]

let%expect_test "get_start_of_week across timezone boundary" =
  (* Sunday 11pm UTC is Monday 8am in Tokyo *)
  let sunday_11pm_utc =
    Option.get @@ Ptime.of_date_time ((2025, 3, 30), ((23, 0, 0), 0))
  in

  let start_of_week_utc =
    Date.start_of_week_result ~tz:Timedesc.Time_zone.utc sunday_11pm_utc
    |> Result.get_ok
  in
  let dt =
    Date.ptime_to_timedesc ~tz:Timedesc.Time_zone.utc start_of_week_utc
  in
  Printf.printf "Start of week (UTC): %04d-%02d-%02d\n" (Timedesc.year dt)
    (Timedesc.month dt) (Timedesc.day dt);

  (* Pin to Tokyo: it's already Monday March 31, so start of week is March 31 *)
  let tokyo = Timedesc.Time_zone.make_exn "Asia/Tokyo" in
  let start_of_week_tokyo =
    Date.start_of_week_result ~tz:tokyo sunday_11pm_utc |> Result.get_ok
  in
  let dt = Date.ptime_to_timedesc ~tz:tokyo start_of_week_tokyo in
  Printf.printf "Start of week (Tokyo): %04d-%02d-%02d\n" (Timedesc.year dt)
    (Timedesc.month dt) (Timedesc.day dt);

  [%expect
    {|
    Start of week (UTC): 2025-03-24
    Start of week (Tokyo): 2025-03-31 |}]

let wall_clock ymd hms = Option.get (Ptime.of_date_time (ymd, (hms, 0)))

let%expect_test "typed calendar time preserves kind and reports timezone errors"
    =
  let london = Timedesc.Time_zone.make_exn "Europe/London" in
  let print_result = function
    | Ok instant -> Printf.printf "ok %s\n" (Ptime.to_rfc3339 instant)
    | Error (`Unknown_timezone tzid) -> Printf.printf "unknown %s\n" tzid
    | Error (`Nonexistent_local_time label) ->
        Printf.printf "nonexistent %s\n" label
    | Error (`Ambiguous_local_time label) ->
        Printf.printf "ambiguous %s\n" label
    | Error (`Out_of_range label) -> Printf.printf "out-of-range %s\n" label
  in
  print_result
    (Date.ptime_of_ical_result ~floating_tz:london
       (`Datetime
          (`With_tzid
             (wall_clock (2025, 3, 30) (1, 30, 45), (false, "Europe/London")))));
  print_result
    (Date.ptime_of_ical_result ~floating_tz:london
       (`Datetime (`Local (wall_clock (2025, 3, 30) (1, 30, 45)))));
  print_result
    (Date.ptime_of_ical_result ~floating_tz:london
       (`Datetime (`Local (wall_clock (2025, 10, 26) (1, 30, 45)))));
  print_result
    (Date.ptime_of_ical_result ~floating_tz:london
       (`Datetime
          (`With_tzid
             (wall_clock (2025, 10, 26) (1, 30, 45), (false, "Europe/London")))));
  print_result
    (Date.ptime_of_ical_result ~floating_tz:london
       (`Datetime
          (`With_tzid
             (wall_clock (2025, 6, 1) (12, 34, 56), (false, "Mars/Olympus_Mons")))));
  [%expect
    {|
    ok 2025-03-30T01:30:45-00:00
    ok 2025-03-30T01:30:45-00:00
    ok 2025-10-26T00:30:45-00:00
    ok 2025-10-26T00:30:45-00:00
    unknown Mars/Olympus_Mons |}]

let%expect_test "Timedesc compatibility conversion selects first repeated time"
    =
  let date = Timedesc.Date.Ymd.make_exn ~year:2025 ~month:10 ~day:26 in
  let time = Timedesc.Time.make_exn ~hour:1 ~minute:30 ~second:45 () in
  let repeated = Timedesc.of_date_and_time_exn ~tz:london date time in
  let instant = Result.get_ok (Date.timedesc_to_ptime_result repeated) in
  print_endline (Date.rfc3339_utc instant);
  [%expect {| 2025-10-26T00:30:45Z |}]

let%expect_test "calendar arithmetic preserves local clock through DST" =
  let london = Timedesc.Time_zone.make_exn "Europe/London" in
  let start =
    Result.get_ok
      (Date.ptime_of_ical_result ~floating_tz:london
         (`Datetime
            (`With_tzid
               (wall_clock (2025, 3, 29) (12, 34, 56), (false, "Europe/London")))))
  in
  let next = Date.add_days_result ~tz:london start 1 |> Result.get_ok in
  let local = Date.ptime_to_timedesc ~tz:london next in
  Printf.printf "local: %04d-%02d-%02d %02d:%02d:%02d\n" (Timedesc.year local)
    (Timedesc.month local) (Timedesc.day local) (Timedesc.hour local)
    (Timedesc.minute local) (Timedesc.second local);
  Printf.printf "elapsed hours: %.0f\n"
    (Ptime.Span.to_float_s (Ptime.diff next start) /. 3600.0);
  [%expect {|
    local: 2025-03-30 12:34:56
    elapsed hours: 23 |}]

let%expect_test "exclusive local day bounds track both DST transitions" =
  let london = Timedesc.Time_zone.make_exn "Europe/London" in
  let day_length date =
    let local_midnight =
      Result.get_ok (Date.ptime_of_ical_result ~floating_tz:london (`Date date))
    in
    let next =
      Result.get_ok (Date.next_midnight_result ~tz:london local_midnight)
    in
    Ptime.diff next local_midnight |> Ptime.Span.to_float_s |> fun seconds ->
    seconds /. 3600.0
  in
  Printf.printf "spring day: %.0fh\n" (day_length (2025, 3, 30));
  Printf.printf "autumn day: %.0fh\n" (day_length (2025, 10, 26));
  [%expect {|
    spring day: 23h
    autumn day: 25h |}]

let%expect_test "calendar arithmetic applies the RFC pre-gap offset" =
  let london = Timedesc.Time_zone.make_exn "Europe/London" in
  let start =
    Result.get_ok
      (Date.ptime_of_ical_result ~floating_tz:london
         (`Datetime
            (`With_tzid
               (wall_clock (2025, 3, 29) (1, 30, 0), (false, "Europe/London")))))
  in
  (match Date.add_days_result ~tz:london start 1 with
  | Error (`Nonexistent_local_time label) -> Printf.printf "gap: %s\n" label
  | Error error ->
      Printf.printf "other: %s\n" (Date.string_of_conversion_error error)
  | Ok instant -> Printf.printf "resolved: %s\n" (Ptime.to_rfc3339 instant));
  [%expect {| resolved: 2025-03-30T01:30:00-00:00 |}]

let%expect_test "date-time parsing applies RFC gap and repeated-time policy" =
  let london = Timedesc.Time_zone.make_exn "Europe/London" in
  let now = Option.get (Ptime.of_date_time ((2025, 1, 1), ((0, 0, 0), 0))) in
  let print date =
    match Date.parse_date_time ~tz:london ~now ~date ~time:"01:30:00" `From with
    | Ok instant -> Printf.printf "ok: %s\n" (Ptime.to_rfc3339 instant)
    | Error (`Msg message) -> Printf.printf "error: %s\n" message
  in
  print "2025-03-30";
  print "2025-10-26";
  [%expect
    {|
    ok: 2025-03-30T01:30:00-00:00
    ok: 2025-10-26T00:30:00-00:00 |}]
