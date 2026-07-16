type command_result = { status : int; stdout : string; stderr : string }

let failf format = Printf.ksprintf failwith format

let check condition format =
  if condition then Printf.ksprintf (fun _ -> ()) format else failf format

let contains ~needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec loop index =
    index + needle_length <= haystack_length
    &&
    if String.sub haystack index needle_length = needle then true
    else loop (index + 1)
  in
  needle_length = 0 || loop 0

let count_substring ~needle haystack =
  let needle_length = String.length needle in
  let rec loop index count =
    if index + needle_length > String.length haystack then count
    else if String.sub haystack index needle_length = needle then
      loop (index + needle_length) (count + 1)
    else loop (index + 1) count
  in
  if needle_length = 0 then 0 else loop 0 0

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let rec remove_tree path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | stats when stats.Unix.st_kind = Unix.S_DIR ->
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let isolated_environment ~calendar_dir ~home =
  Unix.environment () |> Array.to_list
  |> List.filter (fun entry ->
      not
        (String.starts_with ~prefix:"CALENDAR_DIR=" entry
        || String.starts_with ~prefix:"HOME=" entry
        || String.starts_with ~prefix:"TZ=" entry
        || String.starts_with ~prefix:"NO_COLOR=" entry))
  |> fun inherited ->
  Array.of_list
    (("CALENDAR_DIR=" ^ calendar_dir)
    :: ("HOME=" ^ home) :: "TZ=UTC" :: "NO_COLOR=1" :: inherited)

let environment_with_home_only home =
  Unix.environment () |> Array.to_list
  |> List.filter (fun entry ->
      not
        (String.starts_with ~prefix:"CALENDAR_DIR=" entry
        || String.starts_with ~prefix:"HOME=" entry
        || String.starts_with ~prefix:"TZ=" entry
        || String.starts_with ~prefix:"NO_COLOR=" entry))
  |> fun inherited ->
  Array.of_list (("HOME=" ^ home) :: "TZ=UTC" :: "NO_COLOR=1" :: inherited)

let run ~binary ~environment arguments =
  let stdout_path = Filename.temp_file "caled-cli-stdout-" ".log" in
  let stderr_path = Filename.temp_file "caled-cli-stderr-" ".log" in
  let stdout_fd =
    Unix.openfile stdout_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
  in
  let stderr_fd =
    Unix.openfile stderr_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
  in
  Fun.protect
    ~finally:(fun () ->
      (try Unix.close stdout_fd with Unix.Unix_error _ -> ());
      (try Unix.close stderr_fd with Unix.Unix_error _ -> ());
      (try Unix.unlink stdout_path with Unix.Unix_error _ -> ());
      try Unix.unlink stderr_path with Unix.Unix_error _ -> ())
    (fun () ->
      let argv = Array.of_list (binary :: arguments) in
      let pid =
        Unix.create_process_env binary argv environment Unix.stdin stdout_fd
          stderr_fd
      in
      Unix.close stdout_fd;
      Unix.close stderr_fd;
      let _, process_status = Unix.waitpid [] pid in
      let status =
        match process_status with
        | Unix.WEXITED code -> code
        | Unix.WSIGNALED signal -> 128 + signal
        | Unix.WSTOPPED signal -> 192 + signal
      in
      { status; stdout = read_file stdout_path; stderr = read_file stderr_path })

let require_success label result =
  check (result.status = 0) "%s: expected exit 0, got %d\nstdout=%S\nstderr=%S"
    label result.status result.stdout result.stderr;
  check (result.stderr = "") "%s: unexpected stderr %S" label result.stderr

let require_failure label result =
  check (result.status <> 0) "%s: expected nonzero exit\nstdout=%S" label
    result.stdout;
  check (result.stderr <> "") "%s: failure did not explain itself on stderr"
    label

let verify_informational_commands_are_read_only ~binary ~root =
  let home = Filename.concat root "informational-home-must-not-be-created" in
  let environment = environment_with_home_only home in
  let help = run ~binary ~environment [ "--help=plain" ] in
  require_success "top-level help without calendar root" help;
  check
    (contains ~needle:"CALENDAR_DIR" help.stdout)
    "top-level help omitted CALENDAR_DIR documentation";
  check
    (contains ~needle:"~/.calendar" help.stdout)
    "top-level help omitted the default calendar directory";
  check
    (not (Sys.file_exists home))
    "--help created HOME or the default calendar directory";
  let subcommand_help = run ~binary ~environment [ "list"; "--help=plain" ] in
  require_success "subcommand help without calendar root" subcommand_help;
  check
    (not (Sys.file_exists home))
    "subcommand --help created HOME or the default calendar directory";
  let version = run ~binary ~environment [ "--version" ] in
  require_success "version without calendar root" version;
  check
    (String.trim version.stdout = "0.5.0")
    "unexpected --version output %S" version.stdout;
  check
    (not (Sys.file_exists home))
    "--version created HOME or the default calendar directory";
  let misplaced_version =
    run ~binary ~environment [ "search"; "--text"; "--version" ]
  in
  require_failure "misplaced version does not bypass command parsing"
    misplaced_version;
  check
    (not (contains ~needle:"0.5.0" misplaced_version.stdout))
    "malformed search invocation was mistaken for --version";
  check
    (not (Sys.file_exists home))
    "misplaced --version created HOME or the default calendar directory"

let assoc name = function
  | `Assoc fields -> (
      match List.assoc_opt name fields with
      | Some value -> value
      | None -> failf "missing JSON field %S" name)
  | json ->
      failf "expected JSON object while looking up %S, got %s" name
        (Yojson.Safe.to_string json)

let string = function
  | `String value -> value
  | json -> failf "expected JSON string, got %s" (Yojson.Safe.to_string json)

let int = function
  | `Int value -> value
  | json -> failf "expected JSON integer, got %s" (Yojson.Safe.to_string json)

let wall_datetime timestamp =
  let (year, month, day), ((hour, minute, second), _) =
    Ptime.to_date_time timestamp
  in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d" year month day hour minute
    second

let array = function
  | `List values -> values
  | json -> failf "expected JSON array, got %s" (Yojson.Safe.to_string json)

let json_array output = Yojson.Safe.from_string output |> array

let find_id id objects =
  match
    List.filter (fun object_ -> assoc "id" object_ |> string = id) objects
  with
  | [ object_ ] -> object_
  | values ->
      failf "expected one JSON object with id %S, got %d" id
        (List.length values)

let require_null label = function
  | `Null -> ()
  | value ->
      failf "%s: expected null, got %s" label (Yojson.Safe.to_string value)

let require_empty_array label value =
  check (array value = []) "%s: expected an empty array" label

let field_kinds recurrence =
  assoc "parts" recurrence |> array
  |> List.map (fun part -> assoc "kind" part |> string)

let verify_typed_event event =
  check (assoc "schema_version" event |> int = 1) "event schema version";
  check (assoc "component_type" event |> string = "event") "event type";
  check (assoc "priority" event |> int = 7) "event priority";
  let identity = assoc "identity" event in
  check (assoc "calendar_key" identity |> string = "work") "calendar key";
  check
    (assoc "calendar_display_name" identity
    |> string = "Work & Ops \027ESC \007BELL")
    "calendar display name";
  check
    (Filename.basename (assoc "file" identity |> string) = "main.ics")
    "source file identity";
  check (assoc "uid" identity |> string = "typed-event") "identity uid";
  require_null "event recurrence id" (assoc "recurrence_id" identity);
  ignore (assoc "source_fingerprint" identity |> string);
  check
    (assoc "categories" event |> array
    = [ `String "Engineering"; `String "Urgent" ])
    "event categories";
  check (assoc "status" event |> string = "confirmed") "event status";
  let start = assoc "start" event in
  check (assoc "kind" start |> string = "tzid") "event start kind";
  check
    (assoc "value" start |> string = "2026-07-15T12:30:45")
    "event wall-clock value";
  check (assoc "tzid" start |> string = "Europe/London") "event TZID";
  let end_ = assoc "end" event in
  check (assoc "kind" end_ |> string = "duration") "event end kind";
  check (assoc "seconds" end_ |> int = 3723) "event duration seconds";
  let recurrence = assoc "recurrence" event in
  check
    (assoc "frequency" recurrence |> string = "monthly")
    "recurrence frequency";
  let limit = assoc "limit" recurrence in
  check (assoc "kind" limit |> string = "count") "recurrence limit kind";
  check (assoc "value" limit |> int = 2) "recurrence count";
  check (assoc "interval" recurrence |> int = 1) "recurrence interval";
  let kinds = field_kinds recurrence in
  List.iter
    (fun expected ->
      check (List.mem expected kinds) "missing recurrence part %s" expected)
    [ "by_day"; "by_hour"; "by_minute"; "by_second" ];
  let alarms = assoc "alarms" event |> array in
  check (List.length alarms = 1) "expected one event alarm";
  let alarm = List.hd alarms in
  check (assoc "index" alarm |> int = 0) "alarm index";
  check (assoc "action" alarm |> string = "display") "alarm action";
  let trigger = assoc "trigger" alarm in
  check (assoc "kind" trigger |> string = "relative") "alarm trigger kind";
  check (assoc "seconds" trigger |> int = -90) "alarm trigger seconds";
  check (assoc "related" trigger |> string = "end") "alarm trigger relation";
  check (assoc "repeat" alarm |> int = 2) "alarm repeat";
  check
    (assoc "repeat_interval_seconds" alarm |> int = 30)
    "alarm repeat duration";
  check
    (assoc "parameters" trigger |> array <> [])
    "alarm trigger parameters were dropped";
  check (assoc "description" alarm |> string = "Wake, now") "alarm description";
  check (array (assoc "other" alarm) <> []) "alarm extension property";
  check
    (contains ~needle:"DTSTART;TZID=Europe/London:20260715T123045"
       (assoc "ics" event |> string))
    "component lossless ICS payload dropped DTSTART parameters"

let verify_typed_todo todo =
  check (assoc "component_type" todo |> string = "todo") "todo type";
  check
    (assoc "categories" todo |> array
    = [ `String "Work"; `String "Urgent"; `String "Second" ])
    "todo repeated categories";
  let start = assoc "start" todo in
  check (assoc "kind" start |> string = "floating") "todo start kind";
  check
    (assoc "value" start |> string = "2026-07-15T09:00:01")
    "todo start value";
  let due = assoc "due" todo in
  check (assoc "kind" due |> string = "floating") "todo due kind";
  check (assoc "value" due |> string = "2026-07-15T17:00:02") "todo due value";
  check (assoc "status" todo |> string = "in-process") "todo status";
  check (assoc "priority" todo |> int = 2) "todo priority";
  check (assoc "percent_complete" todo |> int = 40) "todo percent";
  check (assoc "parent" todo |> string = "todo-parent") "todo parent";
  require_null "todo completed" (assoc "completed" todo);
  let alarms = array (assoc "alarms" todo) in
  check (alarms <> []) "todo structured alarm";
  let attachment = assoc "attachment" (List.hd alarms) in
  check
    (assoc "kind" attachment |> string = "binary")
    "todo alarm attachment kind";
  check
    (assoc "encoding" attachment |> string = "base64")
    "todo alarm attachment encoding";
  check
    (assoc "value" attachment |> string = "SGVsbG8=")
    "todo alarm attachment value";
  check
    (assoc "parameters" attachment |> array <> [])
    "todo alarm attachment parameters were dropped"

let verify_typed_journal journal =
  check (assoc "component_type" journal |> string = "journal") "journal type";
  let start = assoc "start" journal in
  check (assoc "kind" start |> string = "date") "journal start kind";
  check (assoc "value" start |> string = "2026-07-15") "journal date";
  check (assoc "status" journal |> string = "final") "journal status";
  check
    (assoc "categories" journal |> array
    = [ `String "Notes"; `String "Second"; `String "Third" ])
    "journal repeated categories"

let verify_complete_recurrence_surface event =
  let recurrence = assoc "recurrence" event in
  check
    (assoc "frequency" recurrence |> string = "yearly")
    "complete recurrence frequency";
  check (assoc "interval" recurrence |> int = 2) "complete recurrence interval";
  let limit = assoc "limit" recurrence in
  check
    (assoc "kind" limit |> string = "until")
    "complete recurrence until kind";
  let until = assoc "value" limit in
  check
    (assoc "kind" until |> string = "floating")
    "complete recurrence until temporal kind";
  check
    (assoc "value" until |> string = "2036-01-01T00:00:00")
    "complete recurrence until value";
  let kinds = field_kinds recurrence in
  List.iter
    (fun expected ->
      check (List.mem expected kinds) "missing supported recurrence part %s"
        expected)
    [
      "by_minute";
      "by_hour";
      "by_second";
      "by_month";
      "by_month_day";
      "by_set_position";
      "by_week";
      "by_year_day";
      "weekday";
      "by_day";
    ]

let parse_csv source =
  let rows = ref [] in
  let row = ref [] in
  let field = Buffer.create 64 in
  let quoted = ref false in
  let finish_field () =
    row := Buffer.contents field :: !row;
    Buffer.clear field
  in
  let finish_row () =
    finish_field ();
    rows := List.rev !row :: !rows;
    row := []
  in
  let rec loop index =
    if index = String.length source then (
      check (not !quoted) "unterminated quoted CSV field";
      check
        (!row = [] && Buffer.length field = 0)
        "CSV did not terminate at a record boundary";
      List.rev !rows)
    else if !quoted then (
      match source.[index] with
      | '"' when index + 1 < String.length source && source.[index + 1] = '"' ->
          Buffer.add_char field '"';
          loop (index + 2)
      | '"' ->
          quoted := false;
          loop (index + 1)
      | character ->
          Buffer.add_char field character;
          loop (index + 1))
    else
      match source.[index] with
      | '"' when Buffer.length field = 0 ->
          quoted := true;
          loop (index + 1)
      | ',' ->
          finish_field ();
          loop (index + 1)
      | '\r' when index + 1 < String.length source && source.[index + 1] = '\n'
        ->
          finish_row ();
          loop (index + 2)
      | '\n' -> failf "CSV contains a lone LF at byte %d" index
      | '\r' -> failf "CSV contains a lone CR at byte %d" index
      | character ->
          Buffer.add_char field character;
          loop (index + 1)
  in
  loop 0

let rec json_of_sexp = function
  | Sexplib.Sexp.Atom "null" -> `Null
  | Sexplib.Sexp.List [ Atom "bool"; Atom value ] ->
      `Bool (bool_of_string value)
  | Sexplib.Sexp.List [ Atom "int"; Atom value ] -> `Int (int_of_string value)
  | Sexplib.Sexp.List [ Atom "integer"; Atom value ] -> `Intlit value
  | Sexplib.Sexp.List [ Atom "float"; Atom value ] ->
      `Float (float_of_string value)
  | Sexplib.Sexp.List [ Atom "string"; Atom value ] -> `String value
  | Sexplib.Sexp.List (Atom "array" :: values) ->
      `List (List.map json_of_sexp values)
  | Sexplib.Sexp.List (Atom "object" :: fields) ->
      `Assoc
        (List.map
           (function
             | Sexplib.Sexp.List [ Atom name; value ] ->
                 (name, json_of_sexp value)
             | field ->
                 failf "invalid object field in machine S-expression: %s"
                   (Sexplib.Sexp.to_string_hum field))
           fields)
  | sexp ->
      failf "invalid tagged machine S-expression: %s"
        (Sexplib.Sexp.to_string_hum sexp)

let assert_no_machine_presentation_lf label source =
  check
    (source = "" || source.[String.length source - 1] <> '\n')
    "%s unexpectedly has a presentation newline" label

let main_calendar =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia CLI integration//EN";
      "BEGIN:VTIMEZONE";
      "TZID:Europe/London";
      "BEGIN:STANDARD";
      "DTSTART:19700101T000000";
      "TZOFFSETFROM:+0000";
      "TZOFFSETTO:+0000";
      "END:STANDARD";
      "END:VTIMEZONE";
      "BEGIN:VEVENT";
      "UID:typed-event";
      "DTSTAMP:20260701T000000Z";
      "DTSTART;TZID=Europe/London:20260715T123045";
      "DURATION:PT1H2M3S";
      "SUMMARY:Typed event";
      "DESCRIPTION:Line one\\nLine two\\, quoted \"text\"";
      "LOCATION:Room 1";
      "CATEGORIES:Engineering,Urgent";
      "STATUS:CONFIRMED";
      "PRIORITY:7";
      "RRULE:FREQ=MONTHLY;COUNT=2;INTERVAL=1;BYDAY=3WE;BYHOUR=12;BYMINUTE=30;BYSECOND=45";
      "BEGIN:VALARM";
      "ACTION:DISPLAY";
      "TRIGGER;RELATED=END:-PT1M30S";
      "DURATION:PT30S";
      "REPEAT:2";
      "DESCRIPTION:Wake\\, now";
      "X-TRACE-ID:alarm-ext";
      "END:VALARM";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:typed-event";
      "RECURRENCE-ID;TZID=Europe/London:20260819T123045";
      "DTSTAMP:20260701T000000Z";
      "DTSTART;TZID=Europe/London:20260819T133045";
      "DURATION:PT1H2M3S";
      "SUMMARY:Moved typed event";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:date-until";
      "DTSTAMP:20260701T000000Z";
      "DTSTART;VALUE=DATE:20260715";
      "RRULE:FREQ=DAILY;UNTIL=20260716";
      "SUMMARY:Date until";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:floating-recur";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:20260715T070005";
      "DURATION:PT1M";
      "SUMMARY:Floating recurrence";
      "RRULE:FREQ=DAILY;COUNT=2";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:date-recur";
      "DTSTAMP:20260701T000000Z";
      "DTSTART;VALUE=DATE:20260715";
      "DURATION:P1D";
      "SUMMARY:Date recurrence";
      "RRULE:FREQ=DAILY;COUNT=2";
      "END:VEVENT";
      "BEGIN:VTODO";
      "UID:todo-parent";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:20260715T080000";
      "SUMMARY:Parent todo";
      "END:VTODO";
      "BEGIN:VTODO";
      "UID:undated-todo";
      "DTSTAMP:20260701T000000Z";
      "SUMMARY:Undated todo";
      "END:VTODO";
      "BEGIN:VTODO";
      "UID:duration-todo";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:20260715T110000Z";
      "DURATION:PT2H3M4S";
      "SUMMARY:Duration todo";
      "END:VTODO";
      "BEGIN:VTODO";
      "UID:typed-todo";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:20260715T090001";
      "DUE:20260715T170002";
      "SUMMARY:Typed todo";
      "DESCRIPTION:Todo details";
      "CATEGORIES:Work,Urgent";
      "CATEGORIES:Second";
      "STATUS:IN-PROCESS";
      "PRIORITY:2";
      "PERCENT-COMPLETE:40";
      "RELATED-TO;RELTYPE=PARENT:todo-parent";
      "BEGIN:VALARM";
      "ACTION:AUDIO";
      "TRIGGER:-PT15M";
      "ATTACH;ENCODING=BASE64;VALUE=BINARY:SGVsbG8=";
      "END:VALARM";
      "END:VTODO";
      "BEGIN:VTODO";
      "UID:completed-todo";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:20260715T100000Z";
      "SUMMARY:Completed todo";
      "STATUS:COMPLETED";
      "PERCENT-COMPLETE:100";
      "COMPLETED:20260715T101112Z";
      "END:VTODO";
      "BEGIN:VJOURNAL";
      "UID:typed-journal";
      "DTSTAMP:20260701T000000Z";
      "DTSTART;VALUE=DATE:20260715";
      "SUMMARY:Typed journal";
      "DESCRIPTION:Journal details";
      "CATEGORIES:Notes";
      "CATEGORIES:Second,Third";
      "STATUS:FINAL";
      "END:VJOURNAL";
      "BEGIN:VJOURNAL";
      "UID:undated-journal";
      "DTSTAMP:20260701T000000Z";
      "SUMMARY:Undated journal";
      "END:VJOURNAL";
      "BEGIN:VEVENT";
      "UID:far-future-event";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:21260715T090000Z";
      "SUMMARY:Far future event";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:far-future-recurring";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:21260715T100000Z";
      "SUMMARY:Far future recurring series";
      "RRULE:FREQ=DAILY;COUNT=3";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:far-future-recurring";
      "RECURRENCE-ID:21260716T100000Z";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:21260716T110000Z";
      "SUMMARY:Moved far future recurrence";
      "END:VEVENT";
      "BEGIN:VTODO";
      "UID:far-future-todo";
      "DTSTAMP:20260701T000000Z";
      "DUE:21260715T170000Z";
      "SUMMARY:Far future todo";
      "END:VTODO";
      "BEGIN:VJOURNAL";
      "UID:far-future-journal";
      "DTSTAMP:20260701T000000Z";
      "DTSTART;VALUE=DATE:21260715";
      "SUMMARY:Far future journal";
      "END:VJOURNAL";
      "BEGIN:VEVENT";
      "UID:hostile-event";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:20260715T140000Z";
      "SUMMARY:Hostile \194\155CSI \226\128\174BIDI";
      "BEGIN:VALARM";
      "ACTION:DISPLAY";
      "TRIGGER:-PT1M";
      "DESCRIPTION:Hostile alarm";
      "END:VALARM";
      "BEGIN:VALARM";
      "ACTION:EMAIL";
      "TRIGGER;VALUE=DATE-TIME:20260715T120000Z";
      "SUMMARY:Email subject";
      "DESCRIPTION:Email body";
      "ATTENDEE:mailto:test@example.invalid";
      "ATTACH:https://example.invalid/context.txt";
      "END:VALARM";
      "BEGIN:VALARM";
      "ACTION:NONE";
      "TRIGGER:-PT3M";
      "END:VALARM";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:recurrence-parts";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:20350101T030201";
      "SUMMARY:All recurrence parts";
      "RRULE:FREQ=YEARLY;UNTIL=20360101T000000;INTERVAL=2;BYSECOND=1;BYMINUTE=2;BYHOUR=3;BYDAY=MO,FR;BYMONTHDAY=1,-1;BYYEARDAY=1,-1;BYWEEKNO=1,-1;BYMONTH=1,12;BYSETPOS=1;WKST=MO";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:clear-event";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:20260715T150000Z";
      "DTEND:20260715T160000Z";
      "SUMMARY:Clear me";
      "DESCRIPTION:Remove me";
      "LOCATION:Old room";
      "CATEGORIES:Old,Temporary";
      "RRULE:FREQ=DAILY;COUNT=2";
      "BEGIN:VALARM";
      "ACTION:DISPLAY";
      "TRIGGER:-PT5M";
      "DESCRIPTION:Remove alarm";
      "END:VALARM";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:clear-event";
      "DTSTAMP:20260701T000000Z";
      "RECURRENCE-ID:20260716T150000Z";
      "DTSTART:20260716T153000Z";
      "SUMMARY:Clear override";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:change-series";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:20260715T170000Z";
      "RRULE:FREQ=DAILY;COUNT=2";
      "SUMMARY:Change series";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:change-series";
      "DTSTAMP:20260701T000000Z";
      "RECURRENCE-ID:20260716T170000Z";
      "DTSTART:20260716T173000Z";
      "SUMMARY:Change override";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:duplicate-id";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:20260715T180000Z";
      "SUMMARY:First duplicate";
      "END:VEVENT";
      "END:VCALENDAR";
      "";
    ]

let duplicate_calendar =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia duplicate//EN";
      "BEGIN:VEVENT";
      "UID:duplicate-id";
      "DTSTAMP:20260701T000000Z";
      "DTSTART:20260715T190000Z";
      "SUMMARY:Second duplicate";
      "BEGIN:VALARM";
      "ACTION:DISPLAY";
      "TRIGGER:-PT2M";
      "DESCRIPTION:Duplicate alarm";
      "END:VALARM";
      "END:VEVENT";
      "END:VCALENDAR";
      "";
    ]

let timezone_export_calendar ~uid ~offset ~reference_timezone =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia timezone export CLI test//EN";
      "BEGIN:VTIMEZONE";
      "TZID:Europe/London";
      "BEGIN:STANDARD";
      "DTSTART:19700101T000000";
      "TZOFFSETFROM:" ^ offset;
      "TZOFFSETTO:" ^ offset;
      "END:STANDARD";
      "END:VTIMEZONE";
      "BEGIN:VEVENT";
      "UID:" ^ uid;
      "DTSTAMP:20260701T000000Z";
      (if reference_timezone then "DTSTART;TZID=Europe/London:20260715T120000"
       else "DTSTART:20260715T120000Z");
      "SUMMARY:" ^ uid;
      "END:VEVENT";
      "END:VCALENDAR";
      "";
    ]

let verify_timezone_export_conflicts ~binary ~environment ~calendar_root =
  let one = Filename.concat calendar_root "timezone-one" in
  let two = Filename.concat calendar_root "timezone-two" in
  Unix.mkdir one 0o700;
  Unix.mkdir two 0o700;
  Fun.protect
    ~finally:(fun () ->
      remove_tree one;
      remove_tree two)
    (fun () ->
      let one_file = Filename.concat one "one.ics" in
      write_file one_file
        (timezone_export_calendar ~uid:"timezone-one" ~offset:"+0000"
           ~reference_timezone:true);
      let two_file = Filename.concat two "two.ics" in
      write_file two_file
        (timezone_export_calendar ~uid:"timezone-two" ~offset:"+0000"
           ~reference_timezone:true);
      let arguments =
        [
          "search";
          "--calendar";
          "timezone-one";
          "--calendar";
          "timezone-two";
          "--format";
          "ics";
          "--no-color";
        ]
      in
      let identical = run ~binary ~environment arguments in
      require_success "identical TZID export" identical;
      check
        (count_substring ~needle:"BEGIN:VTIMEZONE" identical.stdout = 1)
        "identical TZID definitions were not deduplicated: %S" identical.stdout;
      write_file two_file
        (timezone_export_calendar ~uid:"timezone-two" ~offset:"+0100"
           ~reference_timezone:true);
      let conflicting = run ~binary ~environment arguments in
      require_failure "conflicting TZID export" conflicting;
      check (conflicting.stdout = "")
        "conflicting TZID export polluted stdout: %S" conflicting.stdout;
      check
        (contains
           ~needle:"Conflicting VTIMEZONE definitions for TZID Europe/London"
           conflicting.stderr)
        "conflicting TZID export error was not actionable: %S"
        conflicting.stderr;
      write_file one_file
        (timezone_export_calendar ~uid:"timezone-one" ~offset:"+0000"
           ~reference_timezone:false);
      write_file two_file
        (timezone_export_calendar ~uid:"timezone-two" ~offset:"+0100"
           ~reference_timezone:false);
      let unused = run ~binary ~environment arguments in
      require_success "conflicting unused TZID export" unused;
      check
        (count_substring ~needle:"BEGIN:VTIMEZONE" unused.stdout = 0)
        "an unused conflicting VTIMEZONE leaked into export: %S" unused.stdout)

let list_args format from_date =
  [
    "list";
    "--from";
    from_date;
    "--to";
    from_date;
    "--timezone";
    "UTC";
    "--format";
    format;
    "--no-color";
  ]

let verify_machine_formats ~binary ~environment =
  let literal_version =
    run ~binary ~environment
      [ "search"; "--format"; "json"; "--no-color"; "--"; "--version" ]
  in
  require_success "end-of-options literal --version search" literal_version;
  check
    (json_array literal_version.stdout = [])
    "literal --version after -- was treated as an informational option: %S"
    literal_version.stdout;
  let json_result = run ~binary ~environment (list_args "json" "2026-07-15") in
  require_success "nonempty JSON" json_result;
  assert_no_machine_presentation_lf "JSON" json_result.stdout;
  check
    (not
       (contains ~needle:"CALEDONIA-INTERNAL-ALARM-MARKER" json_result.stdout))
    "private alarm parser marker leaked into JSON";
  let objects = json_array json_result.stdout in
  check (objects <> []) "nonempty JSON result was empty";
  let check_authored_series label source =
    let _, components =
      match Icalendar.parse source with
      | Ok calendar -> calendar
      | Error message -> failf "%s did not reparse as ICS: %s" label message
    in
    let events =
      List.filter_map
        (function `Event event -> Some event | _ -> None)
        components
    in
    let ids = List.map (fun event -> snd event.Icalendar.uid) events in
    let override_count =
      List.fold_left
        (fun count event ->
          if
            List.exists
              (function `Recur_id _ -> true | _ -> false)
              event.Icalendar.props
          then count + 1
          else count)
        0 events
    in
    check
      (ids = [ "typed-event"; "typed-event" ] && override_count = 1)
      "%s did not contain exactly the authored master+override series: %S" label
      source
  in
  let series_arguments command format =
    match command with
    | "show" -> [ "show"; "typed-event"; "--format"; format; "--no-color" ]
    | "search" ->
        [ "search"; "--id"; "typed-event"; "--format"; format; "--no-color" ]
    | _ -> assert false
  in
  List.iter
    (fun command ->
      let json = run ~binary ~environment (series_arguments command "json") in
      require_success (command ^ " authored-series JSON") json;
      let embedded =
        json_array json.stdout |> find_id "typed-event" |> assoc "ics" |> string
      in
      check_authored_series (command ^ " JSON ics") embedded;
      let ics = run ~binary ~environment (series_arguments command "ics") in
      require_success (command ^ " authored-series ICS") ics;
      check_authored_series (command ^ " ICS") ics.stdout)
    [ "show"; "search" ];
  let typed_occurrence = find_id "typed-event" objects in
  require_null "materialized occurrence recurrence"
    (assoc "recurrence" typed_occurrence);
  let occurrence_identity = assoc "identity" typed_occurrence in
  let occurrence_recurrence_id = assoc "recurrence_id" occurrence_identity in
  check
    (assoc "kind" occurrence_recurrence_id |> string = "tzid")
    "materialized occurrence recurrence-id kind";
  check
    (assoc "value" occurrence_recurrence_id |> string = "2026-07-15T12:30:45")
    "materialized occurrence recurrence-id wall clock";
  check
    (assoc "tzid" occurrence_recurrence_id |> string = "Europe/London")
    "materialized occurrence recurrence-id TZID";
  let floating_recurrence_id =
    find_id "floating-recur" objects
    |> assoc "identity" |> assoc "recurrence_id"
  in
  check
    (assoc "kind" floating_recurrence_id |> string = "floating")
    "floating occurrence recurrence-id kind";
  check
    (assoc "value" floating_recurrence_id |> string = "2026-07-15T07:00:05")
    "floating occurrence recurrence-id wall clock";
  let date_recurrence_id =
    find_id "date-recur" objects |> assoc "identity" |> assoc "recurrence_id"
  in
  check
    (assoc "kind" date_recurrence_id |> string = "date")
    "DATE occurrence recurrence-id kind";
  check
    (assoc "value" date_recurrence_id |> string = "2026-07-15")
    "DATE occurrence recurrence-id value";
  verify_typed_todo (find_id "typed-todo" objects);
  let duration_todo = find_id "duration-todo" objects in
  let todo_end = assoc "end" duration_todo in
  check
    (assoc "kind" todo_end |> string = "duration")
    "todo DURATION discriminator";
  check (assoc "seconds" todo_end |> int = 7384) "todo DURATION exact seconds";
  verify_typed_journal (find_id "typed-journal" objects);
  let date_until =
    run ~binary ~environment
      [ "show"; "date-until"; "--format"; "json"; "--no-color" ]
  in
  require_success "DATE-valued recurrence UNTIL" date_until;
  let until_value =
    json_array date_until.stdout
    |> find_id "date-until" |> assoc "recurrence" |> assoc "limit"
    |> assoc "value"
  in
  check
    (assoc "kind" until_value |> string = "date")
    "DATE UNTIL was misreported as a datetime";
  check
    (assoc "value" until_value |> string = "2026-07-16")
    "DATE UNTIL value changed";
  let repeated_categories_search =
    run ~binary ~environment
      [ "search"; "Second"; "--categories"; "--format"; "json"; "--no-color" ]
  in
  require_success "search repeated categories" repeated_categories_search;
  let category_ids =
    json_array repeated_categories_search.stdout
    |> List.map (fun value -> assoc "id" value |> string)
  in
  check (List.mem "typed-todo" category_ids) "todo repeated category not found";
  check
    (List.mem "typed-journal" category_ids)
    "journal repeated category not found";
  let child_todo_text =
    run ~binary ~environment
      [
        "search";
        "Todo details";
        "--description";
        "--format";
        "text";
        "--no-color";
      ]
  in
  require_success "human search with a todo parent graph" child_todo_text;
  check
    (contains ~needle:"todo-parent" child_todo_text.stdout
    && contains ~needle:"typed-todo" child_todo_text.stdout)
    "human todo search omitted the selected child or its ancestor: %S"
    child_todo_text.stdout;
  let undated_journal =
    run ~binary ~environment
      [ "search"; "--id"; "undated-journal"; "--format"; "json"; "--no-color" ]
  in
  require_success "search undated journal by id" undated_journal;
  check
    (json_array undated_journal.stdout
    |> List.map (fun value -> assoc "id" value |> string)
    = [ "undated-journal" ])
    "undated journal disappeared from date-unbounded search";
  let far_future =
    run ~binary ~environment
      [ "search"; "Far future"; "--format"; "json"; "--no-color" ]
  in
  require_success "date-unbounded far-future search" far_future;
  let far_future_objects = json_array far_future.stdout in
  let far_future_ids =
    far_future_objects
    |> List.map (fun value -> assoc "id" value |> string)
    |> List.sort String.compare
  in
  check
    (far_future_ids
    = [
        "far-future-event";
        "far-future-journal";
        "far-future-recurring";
        "far-future-todo";
      ])
    "date-unbounded search omitted or duplicated a far-future master: %S"
    far_future.stdout;
  let recurring_master = find_id "far-future-recurring" far_future_objects in
  require_null "unbounded recurring master recurrence-id"
    (assoc "identity" recurring_master |> assoc "recurrence_id");
  ignore (assoc "recurrence" recurring_master);
  let far_future_from =
    run ~binary ~environment
      [
        "search";
        "Far future";
        "--from";
        "2126-07-01";
        "--format";
        "json";
        "--no-color";
      ]
  in
  require_success "far-future one-sided search range" far_future_from;
  check
    (json_array far_future_from.stdout <> [])
    "one-sided --from used an upper bound anchored to today";
  let incomplete arguments =
    run ~binary ~environment
      ([ "list"; "--incomplete"; "--format"; "json"; "--no-color" ] @ arguments)
  in
  let incomplete_all = incomplete [] in
  let incomplete_todos = incomplete [ "--type"; "todo" ] in
  require_success "list incomplete default type" incomplete_all;
  require_success "list incomplete todo type" incomplete_todos;
  let ids result =
    json_array result.stdout
    |> List.map (fun value -> assoc "id" value |> string)
    |> List.sort String.compare
  in
  check
    (ids incomplete_all = ids incomplete_todos)
    "--incomplete changed semantics under --type all";
  check
    (List.mem "undated-todo" (ids incomplete_all))
    "--incomplete omitted an undated todo";
  let dtend_event = find_id "clear-event" objects in
  let dtend = assoc "end" dtend_event in
  check (assoc "kind" dtend |> string = "dtend") "DTEND discriminator";
  let dtend_value = assoc "value" dtend in
  check (assoc "kind" dtend_value |> string = "utc") "DTEND temporal kind";
  check
    (assoc "value" dtend_value |> string = "2026-07-15T16:00:00Z")
    "DTEND exact UTC value";
  let completed_todo = find_id "completed-todo" objects in
  check
    (assoc "status" completed_todo |> string = "completed")
    "completed todo status";
  check
    (assoc "percent_complete" completed_todo |> int = 100)
    "completed todo percent";
  check
    (assoc "completed" completed_todo |> string = "2026-07-15T10:11:12Z")
    "completed todo timestamp";

  let show_event =
    run ~binary ~environment
      [ "show"; "typed-event"; "--format"; "json"; "--no-color" ]
  in
  require_success "show typed event" show_event;
  let canonical_event = json_array show_event.stdout |> find_id "typed-event" in
  verify_typed_event canonical_event;

  let recurrence_parts =
    run ~binary ~environment
      [ "show"; "recurrence-parts"; "--format"; "json"; "--no-color" ]
  in
  require_success "show every recurrence part" recurrence_parts;
  json_array recurrence_parts.stdout
  |> find_id "recurrence-parts" |> verify_complete_recurrence_surface;

  let occurrence_range =
    [
      "list";
      "--from";
      "2026-07-15";
      "--to";
      "2026-08-31";
      "--timezone";
      "UTC";
      "--format";
      "json";
      "--no-color";
    ]
  in
  let two_occurrences = run ~binary ~environment occurrence_range in
  require_success "two materialized recurrence instances" two_occurrences;
  let occurrence_objects =
    json_array two_occurrences.stdout
    |> List.filter (fun object_ -> assoc "id" object_ |> string = "typed-event")
  in
  check
    (List.length occurrence_objects = 2)
    "expected two typed-event instances, got %d"
    (List.length occurrence_objects);
  let recurrence_ids =
    List.map
      (fun object_ ->
        require_null "finite occurrence recurrence" (assoc "recurrence" object_);
        assoc "identity" object_ |> assoc "recurrence_id")
      occurrence_objects
  in
  check
    (List.map (fun value -> assoc "kind" value |> string) recurrence_ids
    = [ "tzid"; "tzid" ])
    "occurrence recurrence-id kinds changed";
  check
    (List.map (fun value -> assoc "tzid" value |> string) recurrence_ids
    = [ "Europe/London"; "Europe/London" ])
    "occurrence recurrence-id TZIDs changed";
  check
    (List.map (fun value -> assoc "value" value |> string) recurrence_ids
    = [ "2026-07-15T12:30:45"; "2026-08-19T12:30:45" ])
    "occurrence identities are not exact/distinct wall clocks";
  let moved_start = assoc "start" (List.nth occurrence_objects 1) in
  check
    (assoc "kind" moved_start |> string = "tzid")
    "persisted override start lost TZID kind";
  check
    (assoc "value" moved_start |> string = "2026-08-19T13:30:45")
    "persisted override start was not retained independently of RECURRENCE-ID";

  let occurrence_ics_args =
    List.map
      (function "json" -> "ics" | argument -> argument)
      occurrence_range
  in
  let occurrence_ics = run ~binary ~environment occurrence_ics_args in
  require_success "finite occurrence ICS" occurrence_ics;
  check
    (count_substring ~needle:"UID:typed-event" occurrence_ics.stdout = 2)
    "ICS did not contain two finite typed-event siblings";
  check
    (count_substring ~needle:"RECURRENCE-ID" occurrence_ics.stdout >= 2)
    "ICS finite siblings do not carry RECURRENCE-ID";
  check
    (not (contains ~needle:"RRULE:FREQ=MONTHLY" occurrence_ics.stdout))
    "ICS finite siblings retained the master recurrence rule";
  let _, parsed_occurrence_components =
    match Icalendar.parse occurrence_ics.stdout with
    | Ok calendar -> calendar
    | Error message -> failf "finite occurrence ICS does not parse: %s" message
  in
  let parsed_typed_occurrences =
    List.filter_map
      (function
        | `Event (event : Icalendar.event) when snd event.uid = "typed-event" ->
            Some event
        | _ -> None)
      parsed_occurrence_components
  in
  check
    (List.length parsed_typed_occurrences = 2)
    "reparsed ICS lost a finite occurrence";
  let parsed_recurrence_ids = ref [] in
  List.iter
    (fun (event : Icalendar.event) ->
      check (event.rrule = None) "reparsed occurrence retained RRULE";
      match
        List.find_map
          (function `Recur_id (_, value) -> Some value | _ -> None)
          event.props
      with
      | Some (`Datetime (`With_tzid (wall, (_, tzid)))) ->
          parsed_recurrence_ids :=
            (wall_datetime wall, tzid) :: !parsed_recurrence_ids
      | Some _ ->
          failf "reparsed occurrence RECURRENCE-ID changed temporal kind"
      | None -> failf "reparsed occurrence lost RECURRENCE-ID")
    parsed_typed_occurrences;
  check
    (List.sort compare !parsed_recurrence_ids
    = [
        ("2026-07-15T12:30:45", "Europe/London");
        ("2026-08-19T12:30:45", "Europe/London");
      ])
    "reparsed occurrence RECURRENCE-ID lost exact wall/TZID semantics";
  let recurrence_ids_for uid =
    List.filter_map
      (function
        | `Event (event : Icalendar.event) when snd event.uid = uid ->
            List.find_map
              (function `Recur_id (_, value) -> Some value | _ -> None)
              event.props
        | _ -> None)
      parsed_occurrence_components
  in
  let floating_ids = recurrence_ids_for "floating-recur" in
  check
    (List.map
       (function
         | `Datetime (`Local wall) -> wall_datetime wall | _ -> "wrong-kind")
       floating_ids
    = [ "2026-07-15T07:00:05"; "2026-07-16T07:00:05" ])
    "reparsed floating RECURRENCE-ID lost wall-clock semantics";
  let date_ids = recurrence_ids_for "date-recur" in
  check
    (List.map
       (function
         | `Date (year, month, day) ->
             Printf.sprintf "%04d-%02d-%02d" year month day
         | _ -> "wrong-kind")
       date_ids
    = [ "2026-07-15"; "2026-07-16" ])
    "reparsed DATE RECURRENCE-ID lost date semantics";

  let csv_result = run ~binary ~environment (list_args "csv" "2026-07-15") in
  require_success "nonempty CSV" csv_result;
  check
    (String.ends_with ~suffix:"\r\n" csv_result.stdout)
    "CSV does not end with CRLF";
  let csv_rows = parse_csv csv_result.stdout in
  let expected_header =
    [
      "schema_version";
      "component_type";
      "id";
      "calendar";
      "summary";
      "start";
      "details_json";
    ]
  in
  check (List.hd csv_rows = expected_header) "CSV v1 header mismatch";
  check
    (List.length csv_rows = List.length objects + 1)
    "CSV row count mismatch";
  List.tl csv_rows
  |> List.iter (fun row ->
      check (List.length row = 7) "CSV record has %d fields" (List.length row);
      let details = List.nth row 6 |> Yojson.Safe.from_string in
      check
        (assoc "schema_version" details |> int = 1)
        "CSV details_json schema";
      check
        (assoc "id" details |> string = List.nth row 2)
        "CSV flat id disagrees with details_json");
  let duration_todo_csv =
    List.tl csv_rows |> List.find (fun row -> List.nth row 2 = "duration-todo")
    |> fun row -> List.nth row 6 |> Yojson.Safe.from_string
  in
  check
    (assoc "end" duration_todo_csv |> assoc "seconds" |> int = 7384)
    "CSV details_json lost the todo DURATION";

  let ics_result = run ~binary ~environment (list_args "ics" "2026-07-15") in
  require_success "nonempty ICS" ics_result;
  check
    (String.ends_with ~suffix:"\r\n" ics_result.stdout)
    "ICS does not end at a CRLF boundary";
  check
    (count_substring ~needle:"BEGIN:VCALENDAR" ics_result.stdout = 1)
    "ICS is not one VCALENDAR envelope";
  check
    (not (contains ~needle:"CALEDONIA-INTERNAL-ALARM-MARKER" ics_result.stdout))
    "private alarm parser marker leaked into ICS";
  (match Icalendar.parse ics_result.stdout with
  | Ok _ -> ()
  | Error message -> failf "exported ICS does not parse: %s" message);
  check
    (contains ~needle:"PERCENT-COMPLETE:40\r\n" ics_result.stdout)
    "ICS export lost the standard PERCENT-COMPLETE todo field";
  check
    (not (contains ~needle:"\r\nPERCENT:40\r\n" ics_result.stdout))
    "ICS export emitted the legacy nonstandard PERCENT property";
  check
    (contains ~needle:"UID:duration-todo\r\n" ics_result.stdout)
    "ICS export lost the DTSTART+DURATION todo";
  check
    (contains ~needle:"DURATION:PT2H3M4S\r\n" ics_result.stdout)
    "ICS export lost the todo DURATION";

  let sexp_result = run ~binary ~environment (list_args "sexp" "2026-07-15") in
  require_success "nonempty S-expression" sexp_result;
  assert_no_machine_presentation_lf "S-expression" sexp_result.stdout;
  let sexp = Sexplib.Sexp.of_string sexp_result.stdout in
  let decoded =
    match sexp with
    | Sexplib.Sexp.List records -> `List (List.map json_of_sexp records)
    | _ -> failf "machine S-expression top level is not a list"
  in
  check
    (decoded = `List objects)
    "S-expression does not preserve complete JSON semantics\njson=%s\nsexp=%s"
    (Yojson.Safe.to_string (`List objects))
    (Yojson.Safe.to_string decoded);

  let hostile_json =
    run ~binary ~environment
      [ "show"; "hostile-event"; "--format"; "json"; "--no-color" ]
  in
  require_success "hostile JSON" hostile_json;
  let hostile = json_array hostile_json.stdout |> find_id "hostile-event" in
  check
    (assoc "summary" hostile |> string = "Hostile \194\155CSI \226\128\174BIDI")
    "machine JSON changed hostile data instead of escaping it";
  let hostile_alarms = assoc "alarms" hostile |> array in
  check
    (List.map (fun alarm -> assoc "action" alarm |> string) hostile_alarms
    = [ "display"; "email"; "none" ])
    "structured alarm action order/kinds changed";
  let email_alarm = List.nth hostile_alarms 1 in
  let absolute_trigger = assoc "trigger" email_alarm in
  check
    (assoc "kind" absolute_trigger |> string = "absolute")
    "absolute alarm trigger kind";
  check
    (assoc "value" absolute_trigger |> string = "2026-07-15T12:00:00Z")
    "absolute alarm trigger UTC value";
  check
    (assoc "summary" email_alarm |> string = "Email subject")
    "email alarm summary";
  check
    (assoc "description" email_alarm |> string = "Email body")
    "email alarm description";
  check
    (assoc "attendees" email_alarm
    |> array
    = [ `String "mailto:test@example.invalid" ])
    "email alarm attendees";
  let email_attachment = assoc "attachment" email_alarm in
  check
    (assoc "kind" email_attachment |> string = "uri")
    "email alarm URI attachment kind";
  check
    (assoc "value" email_attachment
    |> string = "https://example.invalid/context.txt")
    "email alarm URI attachment value";

  List.iter
    (fun (format, expected) ->
      let result = run ~binary ~environment (list_args format "2031-01-01") in
      require_success ("empty " ^ format) result;
      check (result.stdout = expected) "empty %s bytes: expected %S, got %S"
        format expected result.stdout)
    [
      ("json", "[]");
      ( "csv",
        "schema_version,component_type,id,calendar,summary,start,details_json\r\n"
      );
      ("ics", "");
      ("sexp", "()");
    ]

let verify_hostile_terminal ~binary ~environment ~work =
  let result =
    run ~binary ~environment
      [ "show"; "hostile-event"; "--format"; "text"; "--no-color" ]
  in
  require_success "hostile human output" result;
  List.iter
    (fun forbidden ->
      check
        (not (contains ~needle:forbidden result.stdout))
        "terminal output retained forbidden control bytes %S" forbidden)
    [ "\027"; "\194\155"; "\226\128\174"; "\007" ];
  check
    (contains ~needle:"?" result.stdout)
    "terminal sanitizer did not visibly replace hostile controls";

  let forced =
    run ~binary ~environment
      [ "show"; "typed-event"; "--format"; "text"; "--color" ]
  in
  require_success "forced trusted color" forced;
  check
    (contains ~needle:"\027[38;2;255;0;0mWork & Ops ?ESC ?BELL\027[0m"
       forced.stdout)
    "--color did not preserve the trusted calendar SGR sequence: %S"
    forced.stdout;
  check
    (not (contains ~needle:"\027ESC" forced.stdout))
    "--color retained an untrusted calendar escape";

  let plain =
    run ~binary ~environment
      [ "show"; "plain-color-event"; "--format"; "text"; "--color" ]
  in
  require_success "forced color without displayname metadata" plain;
  check
    (contains ~needle:"\027[38;2;0;255;0mplain\027[0m" plain.stdout)
    "key-fallback calendar color was corrupted by pre-sanitize ANSI: %S"
    plain.stdout;
  check
    (not (contains ~needle:"?[38;2" plain.stdout))
    "trusted key-fallback color was sanitized as untrusted data";

  let disabled =
    run ~binary ~environment
      [ "show"; "typed-event"; "--format"; "text"; "--no-color" ]
  in
  require_success "disabled color" disabled;
  check
    (not (contains ~needle:"\027" disabled.stdout))
    "--no-color emitted ANSI escapes";

  let automatic =
    run ~binary ~environment [ "show"; "typed-event"; "--format"; "text" ]
  in
  require_success "automatic non-TTY color" automatic;
  check
    (not (contains ~needle:"\027" automatic.stdout))
    "automatic color emitted ANSI to captured non-TTY stdout";

  let hostile_argument =
    run ~binary ~environment [ "delete"; "missing\027\007component" ]
  in
  require_failure "hostile component id error" hostile_argument;
  check
    (hostile_argument.stdout = "")
    "hostile component id failure polluted stdout: %S" hostile_argument.stdout;
  List.iter
    (fun forbidden ->
      check
        (not (contains ~needle:forbidden hostile_argument.stderr))
        "human stderr retained hostile component-id bytes %S: %S" forbidden
        hostile_argument.stderr)
    [ "\027"; "\007"; "\194\155"; "\226\128\174" ];

  let hostile_source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Caledonia hostile error test//EN";
        "BEGIN:VEVENT";
        "UID:hostile\027\007uid";
        "DTSTAMP:20260701T000000Z";
        "DTSTART:20260715T100000Z";
        "END:VEVENT";
        "BEGIN:VEVENT";
        "UID:hostile\027\007uid";
        "DTSTAMP:20260701T000000Z";
        "DTSTART:20260716T100000Z";
        "END:VEVENT";
        "END:VCALENDAR";
        "";
      ]
  in
  let hostile_path = Filename.concat work "hostile-error.ics" in
  write_file hostile_path hostile_source;
  let hostile_calendar_error =
    Fun.protect
      ~finally:(fun () -> Unix.unlink hostile_path)
      (fun () ->
        run ~binary ~environment [ "list"; "--format"; "json"; "--no-color" ])
  in
  require_failure "hostile calendar-derived error" hostile_calendar_error;
  check
    (hostile_calendar_error.stdout = "")
    "hostile calendar error polluted stdout: %S" hostile_calendar_error.stdout;
  List.iter
    (fun forbidden ->
      check
        (not (contains ~needle:forbidden hostile_calendar_error.stderr))
        "calendar-derived stderr retained hostile bytes %S: %S" forbidden
        hostile_calendar_error.stderr)
    [ "\027"; "\007"; "\194\155"; "\226\128\174" ]

let verify_alarms ~binary ~environment =
  let arguments format date timezone =
    [
      "alarms";
      "--from";
      date;
      "--to";
      date;
      "--timezone";
      timezone;
      "--format";
      format;
      "--no-color";
    ]
  in
  let json_result =
    run ~binary ~environment (arguments "json" "2026-07-15" "UTC")
  in
  require_success "nonempty alarms JSON" json_result;
  assert_no_machine_presentation_lf "alarms JSON" json_result.stdout;
  let fires = json_array json_result.stdout in
  check (fires <> []) "expected alarm fires";
  List.iter
    (fun fire ->
      check (assoc "schema_version" fire |> int = 1) "alarm schema version";
      ignore (assoc "fire_time" fire |> string);
      ignore (assoc "calendar_key" fire |> string);
      ignore (assoc "component_id" fire |> string))
    fires;
  let none_fire_present =
    List.exists
      (fun fire ->
        assoc "component_id" fire |> string = "hostile-event"
        && assoc "fire_time" fire |> string = "2026-07-15T13:57:00Z"
        && assoc "trigger" fire |> string = "3 minutes before")
      fires
  in
  check none_fire_present "ACTION:NONE was omitted from the alarm fire query";

  let empty = run ~binary ~environment (arguments "json" "2031-01-01" "UTC") in
  require_success "empty alarms JSON" empty;
  check (empty.stdout = "[]") "empty alarms JSON bytes: %S" empty.stdout;

  let unknown =
    run ~binary ~environment (arguments "json" "2026-07-15" "Mars/Olympus")
  in
  require_failure "unknown alarms timezone" unknown;
  check (unknown.stdout = "") "unknown timezone polluted stdout: %S"
    unknown.stdout;
  check
    (contains ~needle:"Unknown timezone" unknown.stderr
    || contains ~needle:"unknown timezone" unknown.stderr)
    "unknown timezone stderr was not actionable: %S" unknown.stderr;

  let text_result =
    run ~binary ~environment (arguments "text" "2026-07-15" "UTC")
  in
  require_success "hostile alarms text" text_result;
  List.iter
    (fun forbidden ->
      check
        (not (contains ~needle:forbidden text_result.stdout))
        "alarms text retained hostile control bytes %S" forbidden)
    [ "\027"; "\194\155"; "\226\128\174"; "\007" ];
  check
    (contains ~needle:"?" text_result.stdout)
    "alarms terminal fields were not visibly sanitized";

  let color_result =
    run ~binary ~environment
      [
        "alarms";
        "--from";
        "2026-07-15";
        "--to";
        "2026-07-15";
        "--timezone";
        "UTC";
        "--format";
        "text";
        "--color";
      ]
  in
  require_success "alarm calendar-key color lookup" color_result;
  check
    (contains ~needle:"\027[38;2;255;0;0m" color_result.stdout)
    "forced alarm color did not resolve through the stable calendar key"

let verify_errors ~binary ~environment ~work =
  List.iter
    (fun (label, arguments) ->
      let result = run ~binary ~environment arguments in
      require_failure label result;
      check (result.stdout = "") "%s polluted stdout: %S" label result.stdout;
      check
        (contains ~needle:"mutually exclusive" result.stderr)
        "%s did not explain conflicting date shortcuts: %S" label result.stderr)
    [
      ("list conflicting date shortcuts", [ "list"; "--today"; "--tomorrow" ]);
      ("search conflicting date shortcuts", [ "search"; "--week"; "--month" ]);
      ("alarms conflicting date shortcuts", [ "alarms"; "--today"; "--month" ]);
    ];
  List.iter
    (fun (label, arguments) ->
      let result = run ~binary ~environment arguments in
      require_failure label result;
      check (result.stdout = "") "%s polluted stdout: %S" label result.stdout;
      check
        (contains ~needle:"todo-only" result.stderr)
        "%s did not explain the incompatible component type: %S" label
        result.stderr)
    [
      ("event incomplete filter", [ "list"; "--type"; "event"; "--incomplete" ]);
      ("journal overdue filter", [ "list"; "--type"; "journal"; "--overdue" ]);
    ];
  let oversized_relative =
    run ~binary ~environment
      [
        "list";
        "--from";
        "+999999999999999999999999d";
        "--format";
        "json";
        "--no-color";
      ]
  in
  require_failure "oversized relative date" oversized_relative;
  check
    (oversized_relative.stdout = "")
    "oversized relative date polluted machine stdout: %S"
    oversized_relative.stdout;
  check
    (contains ~needle:"out of range" oversized_relative.stderr
    || contains ~needle:"out-of-range" oversized_relative.stderr)
    "oversized relative date stderr was not actionable: %S"
    oversized_relative.stderr;
  let missing =
    run ~binary ~environment
      [ "show"; "missing-id"; "--format"; "json"; "--no-color" ]
  in
  require_failure "missing id" missing;
  check (missing.stdout = "") "missing id polluted stdout: %S" missing.stdout;
  check
    (contains ~needle:"No component found" missing.stderr)
    "missing id stderr was not actionable: %S" missing.stderr;
  let duplicate =
    run ~binary ~environment
      [ "show"; "duplicate-id"; "--format"; "json"; "--no-color" ]
  in
  require_failure "duplicate id" duplicate;
  check (duplicate.stdout = "") "duplicate id polluted stdout: %S"
    duplicate.stdout;
  check
    (contains ~needle:"Multiple components found" duplicate.stderr)
    "duplicate id stderr was not actionable: %S" duplicate.stderr;
  let invalid_utf8_path = Filename.concat work "invalid-utf8.ics" in
  write_file invalid_utf8_path
    (String.concat "\r\n"
       [
         "BEGIN:VCALENDAR";
         "VERSION:2.0";
         "PRODID:-//Caledonia invalid UTF-8 test//EN";
         "BEGIN:VEVENT";
         "UID:invalid-utf8";
         "DTSTAMP:20260701T000000Z";
         "DTSTART:20260715T120000Z";
         "SUMMARY:bad\255text";
         "END:VEVENT";
         "END:VCALENDAR";
         "";
       ]);
  let invalid_utf8 = run ~binary ~environment (list_args "json" "2026-07-15") in
  require_failure "invalid UTF-8 calendar" invalid_utf8;
  check (invalid_utf8.stdout = "")
    "invalid UTF-8 failure polluted machine stdout: %S" invalid_utf8.stdout;
  check
    (contains ~needle:"not valid UTF-8" invalid_utf8.stderr)
    "invalid UTF-8 error was not actionable: %S" invalid_utf8.stderr;
  Unix.unlink invalid_utf8_path;
  let invalid_domain_path = Filename.concat work "invalid-domain.ics" in
  let main_path = Filename.concat work "main.ics" in
  let verify_rejected_document label expected source =
    write_file invalid_domain_path source;
    let before_invalid = read_file invalid_domain_path in
    let before_main = read_file main_path in
    let read_result =
      run ~binary ~environment (list_args "json" "2026-07-15")
    in
    require_failure (label ^ " read") read_result;
    check (read_result.stdout = "") "%s read polluted stdout: %S" label
      read_result.stdout;
    check
      (contains ~needle:expected read_result.stderr)
      "%s read error was not actionable: %S" label read_result.stderr;
    let edit_result =
      run ~binary ~environment
        [ "edit"; "typed-event"; "--summary"; "must not be written" ]
    in
    require_failure (label ^ " mutation") edit_result;
    check
      (read_file invalid_domain_path = before_invalid)
      "%s failed mutation changed the malformed source bytes" label;
    check
      (read_file main_path = before_main)
      "%s failed mutation changed a valid sibling source" label;
    Unix.unlink invalid_domain_path
  in
  let document properties component =
    String.concat "\r\n"
      ([ "BEGIN:VCALENDAR" ] @ properties @ [ component; "END:VCALENDAR"; "" ])
  in
  let valid_event uid =
    String.concat "\r\n"
      [
        "BEGIN:VEVENT";
        "UID:" ^ uid;
        "DTSTAMP:20260701T000000Z";
        "DTSTART:20260715T120000Z";
        "END:VEVENT";
      ]
  in
  verify_rejected_document "missing VERSION" "VERSION:2.0"
    (document
       [ "PRODID:-//Invalid envelope//EN" ]
       (valid_event "missing-version"));
  verify_rejected_document "duplicate PRODID" "duplicate PRODID"
    (document
       [
         "VERSION:2.0";
         "PRODID:-//Invalid envelope one//EN";
         "PRODID:-//Invalid envelope two//EN";
       ]
       (valid_event "duplicate-prodid"));
  verify_rejected_document "blank VEVENT UID" "VEVENT UID must not be empty"
    (document
       [ "VERSION:2.0"; "PRODID:-//Invalid event identity//EN" ]
       (valid_event "   "));
  verify_rejected_document "malformed VTODO identity" "duplicate DTSTAMP"
    (document
       [ "VERSION:2.0"; "PRODID:-//Invalid todo identity//EN" ]
       (String.concat "\r\n"
          [
            "BEGIN:VTODO";
            "UID:invalid-todo";
            "DTSTAMP:20260701T000000Z";
            "DTSTAMP:20260701T010000Z";
            "END:VTODO";
          ]));
  verify_rejected_document "malformed VJOURNAL identity"
    "requires exactly one UID"
    (document
       [ "VERSION:2.0"; "PRODID:-//Invalid journal identity//EN" ]
       (String.concat "\r\n"
          [ "BEGIN:VJOURNAL"; "DTSTAMP:20260701T000000Z"; "END:VJOURNAL" ]))

let verify_add_calendar_time ~binary ~environment =
  let created_id prefix result =
    check
      (String.starts_with ~prefix result.stdout)
      "add did not return a created id: %S" result.stdout;
    String.sub result.stdout (String.length prefix)
      (String.length result.stdout - String.length prefix)
    |> String.trim
  in
  let timed_no_end =
    run ~binary ~environment
      [
        "add";
        "Timed event without end";
        "--calendar";
        "work";
        "--date";
        "2026-07-20";
        "--time";
        "14:00";
        "--timezone";
        "UTC";
      ]
  in
  require_success "documented timed event without end" timed_no_end;
  let timed_uid = created_id "Event created with ID: " timed_no_end in
  let timed_shown =
    run ~binary ~environment
      [ "show"; timed_uid; "--format"; "json"; "--no-color" ]
  in
  require_success "show timed event without end" timed_shown;
  require_null "timed event default end"
    (json_array timed_shown.stdout |> find_id timed_uid |> assoc "end");

  let all_day =
    run ~binary ~environment
      [
        "add";
        "Default one-day event";
        "--calendar";
        "work";
        "--date";
        "2026-07-20";
      ]
  in
  require_success "all-day exclusive default end" all_day;
  let all_day_uid = created_id "Event created with ID: " all_day in
  let all_day_shown =
    run ~binary ~environment
      [ "show"; all_day_uid; "--format"; "json"; "--no-color" ]
  in
  require_success "show all-day default end" all_day_shown;
  let all_day_end =
    json_array all_day_shown.stdout |> find_id all_day_uid |> assoc "end"
  in
  check
    (assoc "kind" all_day_end |> string = "dtend")
    "all-day default did not use DTEND";
  let all_day_end_value = assoc "value" all_day_end in
  check
    (assoc "kind" all_day_end_value |> string = "date")
    "all-day default end lost DATE kind";
  check
    (assoc "value" all_day_end_value |> string = "2026-07-21")
    "all-day default end was not exclusive next day";

  let london_environment =
    Array.map
      (fun entry ->
        if String.starts_with ~prefix:"TZ=" entry then "TZ=Europe/London"
        else entry)
      environment
  in
  let all_day_text =
    run ~binary ~environment:london_environment
      [
        "list";
        "--calendar";
        "work";
        "--from";
        "2026-07-20";
        "--to";
        "2026-07-20";
        "--format";
        "text";
        "--timezone";
        "UTC";
        "--no-color";
      ]
  in
  require_success "list all-day date independently of process timezone"
    all_day_text;
  let all_day_line =
    String.split_on_char '\n' all_day_text.stdout
    |> List.find (contains ~needle:"Default one-day event")
  in
  check
    (contains ~needle:"2026-07-20" all_day_line)
    "all-day text did not preserve its authored civil date: %S" all_day_line;
  check
    (not (contains ~needle:"2026-07-19" all_day_line))
    "all-day text shifted backward through the process timezone: %S"
    all_day_line;

  let add ?(environment = environment) summary time timezone =
    run ~binary ~environment
      [
        "add";
        summary;
        "--calendar";
        "work";
        "--date";
        "2026-03-29";
        "--time";
        time;
        "--end-time";
        "04:15";
        "--timezone";
        timezone;
      ]
  in
  let gap = add "DST gap policy" "01:30" "Europe/London" in
  require_success "nonexistent DST wall clock uses RFC policy" gap;
  let gap_uid = created_id "Event created with ID: " gap in
  let gap_shown =
    run ~binary ~environment
      [ "show"; gap_uid; "--format"; "json"; "--no-color" ]
  in
  require_success "show RFC gap event" gap_shown;
  let gap_start =
    json_array gap_shown.stdout |> find_id gap_uid |> assoc "start"
  in
  check
    (assoc "kind" gap_start |> string = "tzid"
    && assoc "tzid" gap_start |> string = "Europe/London"
    && assoc "value" gap_start |> string = "2026-03-29T01:30:00")
    "RFC gap policy did not preserve the authored wall clock: %s"
    gap_shown.stdout;

  let floating_gap =
    add ~environment:london_environment "Floating DST gap" "01:30" "FLOATING"
  in
  require_success "floating wall clock ignores host DST gaps" floating_gap;
  let floating_uid = created_id "Event created with ID: " floating_gap in
  let floating_shown =
    run ~binary ~environment:london_environment
      [ "show"; floating_uid; "--format"; "json"; "--no-color" ]
  in
  require_success "show floating gap event" floating_shown;
  let floating_start =
    json_array floating_shown.stdout |> find_id floating_uid |> assoc "start"
  in
  check
    (assoc "kind" floating_start |> string = "floating"
    && assoc "value" floating_start |> string = "2026-03-29T01:30:00")
    "floating wall clock was rejected or shifted by host DST: %s"
    floating_shown.stdout;

  let floating_trailing_colon =
    add ~environment:london_environment "Invalid floating clock" "12:30:"
      "FLOATING"
  in
  require_failure "floating clock rejects a trailing colon"
    floating_trailing_colon;
  check
    (floating_trailing_colon.stdout = "")
    "invalid floating clock polluted stdout: %S" floating_trailing_colon.stdout;
  check
    (contains ~needle:"HH:MM" floating_trailing_colon.stderr)
    "invalid floating clock error was not actionable: %S"
    floating_trailing_colon.stderr;

  let unknown = add "Unknown timezone must fail" "03:15" "Mars/Olympus" in
  require_failure "unknown add timezone" unknown;
  check (unknown.stdout = "") "unknown add timezone polluted stdout: %S"
    unknown.stdout;
  check
    (contains ~needle:"Unknown timezone" unknown.stderr)
    "unknown add timezone stderr was not actionable: %S" unknown.stderr;

  let valid = add "Valid London wall clock" "03:15" "Europe/London" in
  require_success "valid London wall clock" valid;
  let prefix = "Event created with ID: " in
  check
    (String.starts_with ~prefix valid.stdout)
    "add did not return the created stable id: %S" valid.stdout;
  let uid =
    String.sub valid.stdout (String.length prefix)
      (String.length valid.stdout - String.length prefix)
    |> String.trim
  in
  check (uid <> "") "created event id was empty";
  let shown =
    run ~binary ~environment [ "show"; uid; "--format"; "json"; "--no-color" ]
  in
  require_success "show valid London wall clock" shown;
  let event = json_array shown.stdout |> find_id uid in
  let start = assoc "start" event in
  check
    (assoc "kind" start |> string = "tzid")
    "added event did not retain TZID temporal kind";
  check
    (assoc "tzid" start |> string = "Europe/London")
    "added event did not retain Europe/London TZID";
  check
    (assoc "value" start |> string = "2026-03-29T03:15:00")
    "added event relabeled a UTC instant instead of retaining wall clock"

let verify_incompatible_component_options ~binary ~environment =
  let verify_add (component_type, option_name, option_arguments) =
    let result =
      run ~binary ~environment
        ([
           "add";
           "--type";
           component_type;
           "Rejected incompatible option";
           "--calendar";
           "work";
         ]
        @ option_arguments)
    in
    require_failure
      (Printf.sprintf "add %s rejects --%s" component_type option_name)
      result;
    check
      (contains ~needle:("--" ^ option_name ^ " is not valid") result.stderr)
      "add %s --%s error was not specific: %S" component_type option_name
      result.stderr
  in
  List.iter verify_add
    [
      ("event", "due", [ "--due"; "2026-07-20" ]);
      ("event", "due-time", [ "--due-time"; "12:00" ]);
      ("event", "duration", [ "--duration"; "1h" ]);
      ("event", "priority", [ "--priority"; "1" ]);
      ("event", "percent", [ "--percent"; "50" ]);
      ("event", "status", [ "--status"; "tentative" ]);
      ("event", "parent", [ "--parent"; "parent-id" ]);
      ("todo", "end-date", [ "--end-date"; "2026-07-20" ]);
      ("todo", "end-time", [ "--end-time"; "12:00" ]);
      ("todo", "end-timezone", [ "--end-timezone"; "UTC" ]);
      ("todo", "location", [ "--location"; "Room" ]);
      ("todo", "recur", [ "--recur"; "daily" ]);
      ("journal", "end-date", [ "--end-date"; "2026-07-20" ]);
      ("journal", "end-time", [ "--end-time"; "12:00" ]);
      ("journal", "end-timezone", [ "--end-timezone"; "UTC" ]);
      ("journal", "location", [ "--location"; "Room" ]);
      ("journal", "recur", [ "--recur"; "daily" ]);
      ("journal", "due", [ "--due"; "2026-07-20" ]);
      ("journal", "due-time", [ "--due-time"; "12:00" ]);
      ("journal", "duration", [ "--duration"; "1h" ]);
      ("journal", "priority", [ "--priority"; "1" ]);
      ("journal", "percent", [ "--percent"; "50" ]);
      ("journal", "parent", [ "--parent"; "parent-id" ]);
      ("journal", "alarm", [ "--alarm"; "15m" ]);
    ];
  let verify_edit (uid, component_type, option_name, option_arguments) =
    let result =
      run ~binary ~environment ([ "edit"; uid ] @ option_arguments)
    in
    require_failure
      (Printf.sprintf "edit %s rejects --%s" component_type option_name)
      result;
    check
      (contains ~needle:("--" ^ option_name ^ " is not valid") result.stderr)
      "edit %s --%s error was not specific: %S" component_type option_name
      result.stderr
  in
  List.iter verify_edit
    [
      ("clear-event", "event", "due", [ "--due"; "2026-07-20" ]);
      ("clear-event", "event", "due-time", [ "--due-time"; "12:00" ]);
      ("clear-event", "event", "duration", [ "--duration"; "1h" ]);
      ("clear-event", "event", "priority", [ "--priority"; "1" ]);
      ("clear-event", "event", "percent", [ "--percent"; "50" ]);
      ("clear-event", "event", "status", [ "--status"; "cancelled" ]);
      ("clear-event", "event", "parent", [ "--parent"; "parent-id" ]);
      ("clear-event", "event", "no-parent", [ "--no-parent" ]);
      ("typed-todo", "todo", "end-date", [ "--end-date"; "2026-07-20" ]);
      ("typed-todo", "todo", "end-time", [ "--end-time"; "12:00" ]);
      ("typed-todo", "todo", "end-timezone", [ "--end-timezone"; "UTC" ]);
      ("typed-todo", "todo", "location", [ "--location"; "Room" ]);
      ("typed-todo", "todo", "recur", [ "--recur"; "daily" ]);
      ("typed-journal", "journal", "end-date", [ "--end-date"; "2026-07-20" ]);
      ("typed-journal", "journal", "end-time", [ "--end-time"; "12:00" ]);
      ("typed-journal", "journal", "end-timezone", [ "--end-timezone"; "UTC" ]);
      ("typed-journal", "journal", "location", [ "--location"; "Room" ]);
      ("typed-journal", "journal", "recur", [ "--recur"; "daily" ]);
      ("typed-journal", "journal", "due", [ "--due"; "2026-07-20" ]);
      ("typed-journal", "journal", "due-time", [ "--due-time"; "12:00" ]);
      ("typed-journal", "journal", "duration", [ "--duration"; "1h" ]);
      ("typed-journal", "journal", "priority", [ "--priority"; "1" ]);
      ("typed-journal", "journal", "percent", [ "--percent"; "50" ]);
      ("typed-journal", "journal", "parent", [ "--parent"; "parent-id" ]);
      ("typed-journal", "journal", "no-parent", [ "--no-parent" ]);
      ("typed-journal", "journal", "alarm", [ "--alarm"; "15m" ]);
      ("typed-journal", "journal", "no-alarms", [ "--no-alarms" ]);
    ]

let verify_todo_duration_cli ~binary ~environment =
  let added =
    run ~binary ~environment
      [
        "add";
        "--type";
        "todo";
        "Duration CLI todo";
        "--calendar";
        "work";
        "--date";
        "2026-07-15";
        "--time";
        "10:00";
        "--timezone";
        "UTC";
        "--duration";
        "2h3m4s";
      ]
  in
  require_success "add todo duration" added;
  let prefix = "Todo created with ID: " in
  check
    (String.starts_with ~prefix added.stdout)
    "duration add did not return the created todo id: %S" added.stdout;
  let uid =
    String.sub added.stdout (String.length prefix)
      (String.length added.stdout - String.length prefix)
    |> String.trim
  in
  check (uid <> "") "created duration todo id was empty";
  let show label =
    let result =
      run ~binary ~environment [ "show"; uid; "--format"; "json"; "--no-color" ]
    in
    require_success label result;
    json_array result.stdout |> find_id uid
  in
  let end_ = show "show added duration todo" |> assoc "end" in
  check (assoc "kind" end_ |> string = "duration") "added todo end kind";
  check
    (assoc "seconds" end_ |> int = 7384)
    "compound todo duration did not retain exact seconds";

  let add_conflict =
    run ~binary ~environment
      [
        "add";
        "--type";
        "todo";
        "Conflicting duration todo";
        "--calendar";
        "work";
        "--date";
        "2026-07-15";
        "--duration";
        "1h";
        "--due";
        "2026-07-16";
      ]
  in
  require_failure "add due/duration conflict" add_conflict;
  check
    (contains ~needle:"Cannot combine --duration" add_conflict.stderr)
    "add due/duration conflict stderr: %S" add_conflict.stderr;

  let missing_start =
    run ~binary ~environment
      [
        "add";
        "--type";
        "todo";
        "Missing start duration todo";
        "--calendar";
        "work";
        "--duration";
        "1h";
      ]
  in
  require_failure "duration requires DTSTART" missing_start;
  check
    (contains ~needle:"DURATION requires DTSTART" missing_start.stderr)
    "missing DTSTART duration stderr: %S" missing_start.stderr;

  let edited =
    run ~binary ~environment [ "edit"; uid; "--duration"; "45m30s" ]
  in
  require_success "edit todo duration" edited;
  let edited_end = show "show edited duration todo" |> assoc "end" in
  check
    (assoc "seconds" edited_end |> int = 2730)
    "edited todo duration did not retain exact seconds";

  let set_clear_conflict =
    run ~binary ~environment
      [ "edit"; uid; "--duration"; "1h"; "--clear"; "duration" ]
  in
  require_failure "set/clear duration conflict" set_clear_conflict;
  check
    (contains ~needle:"Cannot set and clear duration" set_clear_conflict.stderr)
    "set/clear duration conflict stderr: %S" set_clear_conflict.stderr;

  let due_conflict =
    run ~binary ~environment
      [ "edit"; uid; "--duration"; "1h"; "--due"; "2026-07-16" ]
  in
  require_failure "edit due/duration conflict" due_conflict;
  check
    (contains ~needle:"Cannot combine --duration" due_conflict.stderr)
    "edit due/duration conflict stderr: %S" due_conflict.stderr;

  let cleared =
    run ~binary ~environment [ "edit"; uid; "--clear"; "duration" ]
  in
  require_success "clear todo duration" cleared;
  require_null "cleared todo duration"
    (show "show cleared duration todo" |> assoc "end")

let verify_edit_clear ~binary ~environment ~main_path =
  let canonical_todo_edit =
    run ~binary ~environment
      [ "edit"; "typed-todo"; "--summary"; "Edited typed todo" ]
  in
  require_success "canonical PERCENT-COMPLETE edit" canonical_todo_edit;
  let edited_source = read_file main_path in
  check
    (contains ~needle:"PERCENT-COMPLETE:40\r\n" edited_source)
    "todo edit did not retain canonical PERCENT-COMPLETE";
  check
    (not (contains ~needle:"\r\nPERCENT:40\r\n" edited_source))
    "todo edit wrote the legacy nonstandard PERCENT property";
  check
    (contains ~needle:"UNTIL=20260716" edited_source
    && not (contains ~needle:"UNTIL=20260716T000000Z" edited_source))
    "unrelated edit changed DATE-valued RRULE UNTIL semantics";
  check
    (not (contains ~needle:"X-CALEDONIA-DATE-UNTIL" edited_source))
    "private DATE UNTIL compatibility marker leaked to disk";

  let conflict =
    run ~binary ~environment
      [ "edit"; "clear-event"; "--location"; "New room"; "--clear"; "location" ]
  in
  require_failure "set/clear conflict" conflict;
  check
    (contains ~needle:"Cannot set and clear location" conflict.stderr)
    "set/clear conflict stderr: %S" conflict.stderr;

  let before_required_clear = read_file main_path in
  let required_clear =
    run ~binary ~environment [ "edit"; "clear-event"; "--clear"; "start" ]
  in
  require_failure "required DTSTART clear" required_clear;
  check
    (contains ~needle:"DTSTART is required" required_clear.stderr)
    "required DTSTART clear stderr: %S" required_clear.stderr;
  check
    (read_file main_path = before_required_clear)
    "failed edit mutated the calendar file";

  let clear_event =
    run ~binary ~environment
      [
        "edit";
        "clear-event";
        "--clear";
        "summary";
        "--clear";
        "end";
        "--clear";
        "location";
        "--clear";
        "description";
        "--clear";
        "categories";
        "--clear";
        "recurrence";
        "--clear";
        "alarms";
      ]
  in
  require_success "clear event optional fields" clear_event;
  let event =
    run ~binary ~environment
      [ "show"; "clear-event"; "--format"; "json"; "--no-color" ]
  in
  require_success "show cleared event" event;
  let event = json_array event.stdout |> find_id "clear-event" in
  List.iter
    (fun field -> require_null ("cleared event " ^ field) (assoc field event))
    [ "summary"; "end"; "location"; "description"; "recurrence" ];
  require_empty_array "cleared event categories" (assoc "categories" event);
  require_empty_array "cleared event alarms" (assoc "alarms" event);
  let after_clear_source = read_file main_path in
  check
    (not (contains ~needle:"RECURRENCE-ID:20260716T150000Z" after_clear_source))
    "clearing recurrence left its persisted override in the source document";

  let change_recurrence =
    run ~binary ~environment
      [ "edit"; "change-series"; "--recur"; "FREQ=WEEKLY;COUNT=2" ]
  in
  require_success "change recurrence removes authored overrides"
    change_recurrence;
  check
    (not
       (contains ~needle:"RECURRENCE-ID:20260716T170000Z" (read_file main_path)))
    "changing recurrence left its persisted override in the source document";

  let clear_todo =
    run ~binary ~environment
      [
        "edit";
        "typed-todo";
        "--clear";
        "summary";
        "--clear";
        "start";
        "--clear";
        "due";
        "--clear";
        "description";
        "--clear";
        "categories";
        "--clear";
        "status";
        "--clear";
        "priority";
        "--clear";
        "percent";
        "--clear";
        "parent";
        "--clear";
        "alarms";
      ]
  in
  require_success "clear todo optional fields" clear_todo;
  let todo =
    run ~binary ~environment
      [ "show"; "typed-todo"; "--format"; "json"; "--no-color" ]
  in
  require_success "show cleared todo" todo;
  let todo = json_array todo.stdout |> find_id "typed-todo" in
  List.iter
    (fun field -> require_null ("cleared todo " ^ field) (assoc field todo))
    [
      "summary";
      "start";
      "due";
      "description";
      "status";
      "priority";
      "percent_complete";
      "parent";
    ];
  require_empty_array "cleared todo categories" (assoc "categories" todo);
  require_empty_array "cleared todo alarms" (assoc "alarms" todo);

  let clear_journal =
    run ~binary ~environment
      [
        "edit";
        "typed-journal";
        "--clear";
        "summary";
        "--clear";
        "start";
        "--clear";
        "description";
        "--clear";
        "categories";
        "--clear";
        "status";
      ]
  in
  require_success "clear journal optional fields" clear_journal;
  let journal =
    run ~binary ~environment
      [ "show"; "typed-journal"; "--format"; "json"; "--no-color" ]
  in
  require_success "show cleared journal" journal;
  let journal = json_array journal.stdout |> find_id "typed-journal" in
  List.iter
    (fun field ->
      require_null ("cleared journal " ^ field) (assoc field journal))
    [ "summary"; "start"; "description"; "status" ];
  require_empty_array "cleared journal categories" (assoc "categories" journal)

let verify_human_time_policy ~binary ~environment ~work =
  let london =
    run ~binary ~environment
      [
        "list";
        "--from";
        "2026-07-15";
        "--to";
        "2026-07-15";
        "--timezone";
        "Europe/London";
        "--type";
        "event";
        "--format";
        "entries";
        "--no-color";
      ]
  in
  require_success "floating and DATE human display" london;
  check
    (contains ~needle:"Start: 2026-07-15 Wed 07:00(Europe/London)" london.stdout)
    "floating 07:00 was shifted in Europe/London human output: %S" london.stdout;
  check
    (contains ~needle:"Summary: Date recurrence\nStart: 2026-07-15 Wed"
       london.stdout)
    "DATE shifted or disappeared in Europe/London human output: %S"
    london.stdout;
  let custom_source ~embedded ~suffix =
    String.concat "\r\n"
      ([
         "BEGIN:VCALENDAR";
         "VERSION:2.0";
         "PRODID:-//Caledonia custom timezone CLI test//EN";
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
          "UID:custom-range-event-" ^ suffix;
          "DTSTAMP:20260701T000000Z";
          "DTSTART;TZID=Mars/Olympus:20260715T090000";
          "DTEND;TZID=Mars/Olympus:20260715T100000";
          "SUMMARY:Custom range " ^ suffix;
          "END:VEVENT";
          "BEGIN:VTODO";
          "UID:custom-range-todo-" ^ suffix;
          "DTSTAMP:20260701T000000Z";
          "DTSTART;TZID=Mars/Olympus:20260715T110000";
          "DUE;TZID=Mars/Olympus:20260715T120000";
          "SUMMARY:Custom range " ^ suffix;
          "END:VTODO";
          "END:VCALENDAR";
          "";
        ])
  in
  write_file
    (Filename.concat work "unknown-timezone-range.ics")
    (custom_source ~embedded:false ~suffix:"unknown");
  write_file
    (Filename.concat work "embedded-timezone-range.ics")
    (custom_source ~embedded:true ~suffix:"embedded");
  let machine =
    run ~binary ~environment
      [
        "search";
        "Custom range";
        "--sort";
        "summary";
        "--format";
        "json";
        "--no-color";
      ]
  in
  require_success "date-unbounded custom timezone machine search" machine;
  let objects = json_array machine.stdout in
  let check_tzid label value expected_wall =
    check (assoc "kind" value |> string = "tzid") "%s kind lost" label;
    check
      (assoc "value" value |> string = expected_wall)
      "%s wall-clock value lost" label;
    check
      (assoc "tzid" value |> string = "Mars/Olympus")
      "%s TZID spelling lost" label
  in
  List.iter
    (fun suffix ->
      let event = find_id ("custom-range-event-" ^ suffix) objects in
      let todo = find_id ("custom-range-todo-" ^ suffix) objects in
      check_tzid (suffix ^ " event start") (assoc "start" event)
        "2026-07-15T09:00:00";
      check_tzid (suffix ^ " event end")
        (assoc "end" event |> assoc "value")
        "2026-07-15T10:00:00";
      check_tzid (suffix ^ " todo start") (assoc "start" todo)
        "2026-07-15T11:00:00";
      check_tzid (suffix ^ " todo due") (assoc "due" todo) "2026-07-15T12:00:00";
      let ics = assoc "ics" event |> string in
      check
        (contains ~needle:"BEGIN:VTIMEZONE" ics = String.equal suffix "embedded")
        "%s machine ICS embedded timezone mismatch" suffix)
    [ "unknown"; "embedded" ];
  List.iter
    (fun suffix ->
      let id = "custom-range-event-" ^ suffix in
      let human =
        run ~binary ~environment
          [ "show"; id; "--format"; "text"; "--no-color" ]
      in
      require_failure (suffix ^ " custom timezone human display") human;
      check (human.stdout = "") "%s human failure polluted stdout: %S" suffix
        human.stdout;
      check
        (contains ~needle:"unknown timezone Mars/Olympus" human.stderr)
        "%s human timezone error was not actionable: %S" suffix human.stderr;
      let bounded =
        run ~binary ~environment
          [
            "search";
            "--id";
            id;
            "--from";
            "2026-07-15";
            "--to";
            "2026-07-15";
            "--format";
            "json";
            "--no-color";
          ]
      in
      require_failure (suffix ^ " custom timezone bounded search") bounded;
      check (bounded.stdout = "") "%s bounded failure polluted stdout: %S"
        suffix bounded.stdout;
      check
        (contains ~needle:"unknown timezone Mars/Olympus" bounded.stderr)
        "%s bounded timezone error was not actionable: %S" suffix bounded.stderr)
    [ "unknown"; "embedded" ]

let verify_rdate_only_machine_recurrence ~binary ~environment ~work =
  let path = Filename.concat work "rdate-machine.ics" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Unix.unlink path)
    (fun () ->
      write_file path
        (String.concat "\r\n"
           [
             "BEGIN:VCALENDAR";
             "VERSION:2.0";
             "PRODID:-//Caledonia RDATE machine test//EN";
             "BEGIN:VEVENT";
             "UID:rdate-machine";
             "DTSTAMP:20260701T000000Z";
             "DTSTART:20260715T090000Z";
             "RDATE:20260718T090000Z";
             "EXDATE:20260715T090000Z";
             "SUMMARY:RDATE machine";
             "END:VEVENT";
             "END:VCALENDAR";
             "";
           ]);
      let shown =
        run ~binary ~environment
          [ "show"; "rdate-machine"; "--format"; "json"; "--no-color" ]
      in
      require_success "RDATE-only machine recurrence" shown;
      let event = json_array shown.stdout |> find_id "rdate-machine" in
      require_null "RDATE-only structured RRULE" (assoc "recurrence" event);
      check
        (assoc "recurrence_set" event
         |> array
         = [
             `String "EXDATE:20260715T090000Z"; `String "RDATE:20260718T090000Z";
           ]
        || assoc "recurrence_set" event
           |> array
           = [
               `String "RDATE:20260718T090000Z";
               `String "EXDATE:20260715T090000Z";
             ])
        "RDATE-only recurrence set was not exposed exactly: %S" shown.stdout)

let () =
  if Array.length Sys.argv <> 2 then
    failf "usage: %s /absolute/path/to/built/caled" Sys.argv.(0);
  let binary =
    if Filename.is_relative Sys.argv.(1) then
      Filename.concat (Sys.getcwd ()) Sys.argv.(1)
    else Sys.argv.(1)
  in
  check (Sys.file_exists binary) "built caled executable not found: %s" binary;
  let root = Filename.temp_file "caled-cli-root-" "" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  Fun.protect
    ~finally:(fun () -> remove_tree root)
    (fun () ->
      verify_informational_commands_are_read_only ~binary ~root;
      let calendar_root = Filename.concat root "calendars" in
      let work = Filename.concat calendar_root "work" in
      let other = Filename.concat calendar_root "other" in
      let plain = Filename.concat calendar_root "plain" in
      let home = Filename.concat root "isolated-home" in
      Unix.mkdir calendar_root 0o700;
      Unix.mkdir work 0o700;
      Unix.mkdir other 0o700;
      Unix.mkdir plain 0o700;
      Unix.mkdir home 0o700;
      write_file
        (Filename.concat work "displayname")
        "Work & Ops \027ESC \007BELL\n";
      write_file (Filename.concat work "color") "#ff0000\n";
      let main_path = Filename.concat work "main.ics" in
      write_file main_path main_calendar;
      write_file (Filename.concat other "duplicate.ics") duplicate_calendar;
      write_file
        (Filename.concat other "displayname")
        "Other \194\155CSI \226\128\174BIDI\n";
      write_file (Filename.concat plain "color") "#00ff00\n";
      write_file
        (Filename.concat plain "plain.ics")
        (String.concat "\r\n"
           [
             "BEGIN:VCALENDAR";
             "VERSION:2.0";
             "PRODID:-//Caledonia plain color test//EN";
             "BEGIN:VEVENT";
             "UID:plain-color-event";
             "DTSTAMP:20260701T000000Z";
             "DTSTART:20260715T200000Z";
             "SUMMARY:Plain color";
             "END:VEVENT";
             "END:VCALENDAR";
             "";
           ]);
      let environment =
        isolated_environment ~calendar_dir:calendar_root ~home
      in
      verify_machine_formats ~binary ~environment;
      verify_timezone_export_conflicts ~binary ~environment ~calendar_root;
      verify_hostile_terminal ~binary ~environment ~work;
      verify_alarms ~binary ~environment;
      verify_errors ~binary ~environment ~work;
      verify_add_calendar_time ~binary ~environment;
      verify_incompatible_component_options ~binary ~environment;
      verify_todo_duration_cli ~binary ~environment;
      verify_edit_clear ~binary ~environment ~main_path;
      verify_human_time_policy ~binary ~environment ~work;
      verify_rdate_only_machine_recurrence ~binary ~environment ~work;
      Printf.printf "CLI integration checks passed\n")
