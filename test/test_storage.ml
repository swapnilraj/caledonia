open Caledonia_lib

module Event = struct
  include Event

  let create = create ~now:Ptime.epoch
  let edit_patch = edit_patch ~now:Ptime.epoch
end

module Todo = struct
  include Todo

  let create = create ~now:Ptime.epoch
  let edit = edit ~now:Ptime.epoch
end

module Journal = struct
  include Journal

  let create = create ~now:Ptime.epoch
  let edit = edit ~now:Ptime.epoch
end

let contains_substring ~needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec search offset =
    if offset + needle_length > haystack_length then false
    else if String.sub haystack offset needle_length = needle then true
    else search (offset + 1)
  in
  needle_length = 0 || search 0

let mixed_calendar =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia storage tests//EN";
      "X-DOCUMENT-PROPERTY:preserve-me";
      "BEGIN:VEVENT";
      "UID:event-1";
      "DTSTAMP:20260101T000000Z";
      "DTSTART:20260102T090000Z";
      "SUMMARY:Sibling event";
      "X-EVENT-PROPERTY:event-value";
      "END:VEVENT";
      "BEGIN:VTODO";
      "UID:todo-1";
      "DTSTAMP:20260101T000000Z";
      "SUMMARY:Original todo";
      "DUE:20260103T090000Z";
      "X-TODO-PROPERTY:todo-value";
      "END:VTODO";
      "BEGIN:VJOURNAL";
      "UID:journal-1";
      "DTSTAMP:20260101T000000Z";
      "DTSTART;VALUE=DATE:20260104";
      "SUMMARY:Sibling journal";
      "X-JOURNAL-PROPERTY:journal-value";
      "END:VJOURNAL";
      "END:VCALENDAR";
      "";
    ]

let duplicate_todo_calendar =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia storage tests//EN";
      "BEGIN:VTODO";
      "UID:duplicate";
      "DTSTAMP:20260101T000000Z";
      "SUMMARY:First";
      "END:VTODO";
      "BEGIN:VTODO";
      "UID:duplicate";
      "DTSTAMP:20260101T000000Z";
      "SUMMARY:Second";
      "END:VTODO";
      "END:VCALENDAR";
      "";
    ]

let duplicate_event_master_calendar =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia storage tests//EN";
      "BEGIN:VEVENT";
      "UID:duplicate-event";
      "DTSTAMP:20260101T000000Z";
      "DTSTART:20260102T090000Z";
      "END:VEVENT";
      "BEGIN:VEVENT";
      "UID:duplicate-event";
      "DTSTAMP:20260101T000000Z";
      "DTSTART:20260103T090000Z";
      "END:VEVENT";
      "END:VCALENDAR";
      "";
    ]

let with_calendar_dir fn =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let suffix =
    Uuidm.v4_gen (Random.State.make_self_init ()) () |> Uuidm.to_string
  in
  let root =
    Filename.concat (Filename.get_temp_dir_name ()) ("caledonia-" ^ suffix)
  in
  let root_path = Eio.Path.(fs / root) in
  Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 root_path;
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root_path)
    (fun () -> fn fs root root_path)

let prepare_file fs root root_path key name content =
  let calendar_path = Eio.Path.(root_path / key) in
  Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 calendar_path;
  let file = Eio.Path.(calendar_path / name) in
  Eio.Path.save ~create:(`Exclusive 0o600) file content;
  let calendar_dir = Result.get_ok (Calendar_dir.create ~fs root) in
  (calendar_dir, calendar_path, file)

let replace_body_and_reload ~fs calendar_dir components body =
  let identity = Component.identity_of_body body in
  let original =
    List.find
      (fun component ->
        Component.get_identity component |> Component_identity.equal identity)
      components
  in
  let _canonical =
    Result.get_ok
      (Calendar_dir.replace_stored_component ~fs calendar_dir ~original
         ~replacement:body)
  in
  Result.get_ok (Calendar_dir.get_components ~fs calendar_dir)

let remove_and_reload ~fs calendar_dir component =
  let _outcome =
    Result.get_ok
      (Calendar_dir.remove_stored_component ~fs calendar_dir component)
  in
  Result.get_ok (Calendar_dir.get_components ~fs calendar_dir)

let serialize_body body =
  Calendar_document.known_entries_of_body body
  |> Result.get_ok
  |> Calendar_codec.create_known
       ~properties:
         [
           `Prodid (Icalendar.Params.empty, "-//Caledonia tests//EN");
           `Version (Icalendar.Params.empty, "2.0");
         ]
  |> Calendar_codec.serialize ~cr:true

let event_series_of_calendar_result calendar =
  snd calendar
  |> List.filter_map (function `Event event -> Some event | _ -> None)
  |> List.map (fun event ->
      let known = Calendar_codec.make_known (`Event event) |> Result.get_ok in
      match
        (Calendar_codec.component known, Calendar_codec.rrule_date_untils known)
      with
      | `Event event, [] -> (event, None)
      | `Event event, [ date_until ] -> (event, date_until)
      | _ -> failwith "invalid VEVENT RRULE metadata shape")
  |> Event.of_authored_events_result

let%expect_test
    "DATE UNTIL stays typed and marker-free across repository create and edit" =
  with_calendar_dir @@ fun fs _root _root_path ->
  let calendar_dir = Result.get_ok (Calendar_dir.create ~fs _root) in
  Result.get_ok (Calendar_dir.create_calendar ~fs calendar_dir "work");
  let date_params =
    Icalendar.Params.empty |> Icalendar.Params.add Icalendar.Valuetype `Date
  in
  let until_date = (2026, 7, 16) in
  let until = Ptime.of_date until_date |> Option.get in
  let created =
    Event.create ~summary:"DATE UNTIL create"
      ~start:(date_params, `Date (2026, 7, 15))
      ~recurrence:(`Daily, Some (`Until (`Utc until)), None, [])
      ~recurrence_date_until:until_date ()
    |> Result.get_ok
  in
  let canonical =
    Calendar_dir.create_stored_component ~fs calendar_dir ~calendar_key:"work"
      (Component.event_body created)
    |> Result.get_ok
  in
  let loaded = Component.to_event canonical |> Option.get in
  let raw_master = Event.master loaded in
  let raw =
    Icalendar.to_ics ~cr:true
      ( [
          `Prodid (Icalendar.Params.empty, "-//DATE UNTIL test//EN");
          `Version (Icalendar.Params.empty, "2.0");
        ],
        [ `Event raw_master ] )
  in
  let edited =
    Event.edit_patch ~summary:(Patch.Set "DATE UNTIL edited") loaded
    |> Result.get_ok
  in
  let canonical =
    Calendar_dir.replace_stored_component ~fs calendar_dir ~original:canonical
      ~replacement:(Component.event_body edited)
    |> Result.get_ok
  in
  let written = Eio.Path.load (Component.get_file canonical) in
  let reloaded = Component.to_event canonical |> Option.get in
  let forged_params =
    match raw_master.Icalendar.rrule with
    | Some (params, _) ->
        Icalendar.Params.add (Icalendar.Iana_param "X-CALEDONIA-DATE-UNTIL")
          [ `String "forged" ]
          params
    | None -> assert false
  in
  let forged =
    {
      raw_master with
      rrule =
        Option.map
          (fun (_, recurrence) -> (forged_params, recurrence))
          raw_master.rrule;
    }
  in
  Printf.printf "loaded/edited metadata=%b/%b marker-free=%b date-wire=%b\n"
    (Event.date_until loaded = Some until_date)
    (Event.date_until reloaded = Some until_date)
    (not (contains_substring ~needle:"X-CALEDONIA" raw))
    (contains_substring ~needle:"UNTIL=20260716" written
    && not (contains_substring ~needle:"UNTIL=20260716T000000Z" written));
  Printf.printf "reparse=%b forged-codec-marker-rejected=%b\n"
    (Calendar_codec.parse_document written |> Result.is_ok)
    (Calendar_codec.make_known (`Event forged) |> Result.is_error);
  [%expect
    {|
    loaded/edited metadata=true/true marker-free=true date-wire=true
    reparse=true forged-codec-marker-rejected=true |}]

let export_all_stored ~fs calendar_dir =
  let documents = Result.get_ok (Calendar_dir.get_documents ~fs calendar_dir) in
  let components = List.concat_map Calendar_document.components documents in
  Calendar_export.stored_to_ics ~documents components

let count_component_types calendar =
  List.fold_left
    (fun (events, todos, journals) -> function
      | `Event _ -> (events + 1, todos, journals)
      | `Todo _ -> (events, todos + 1, journals)
      | `Journal _ -> (events, todos, journals + 1)
      | `Freebusy _ | `Timezone _ -> (events, todos, journals))
    (0, 0, 0) (snd calendar)

let has_x_property expected = function
  | `Xprop ((_, name), _, value) -> name = expected && value <> ""
  | `Iana_prop (name, _, value) -> name = expected && value <> ""
  | _ -> false

let%expect_test "editing and deleting a todo preserves its physical document" =
  with_calendar_dir @@ fun fs root _root_path ->
  let calendar_dir, calendar_path, file =
    prepare_file fs root Eio.Path.(fs / root) "work" "mixed.ics" mixed_calendar
  in
  Eio.Path.save ~create:(`Exclusive 0o600)
    Eio.Path.(calendar_path / "displayname")
    "Team calendar\n";
  let components =
    Result.get_ok (Calendar_dir.get_components ~fs calendar_dir)
  in
  let todo_component =
    List.find
      (fun component -> Component.get_id component = "todo-1")
      components
  in
  let todo = Option.get (Component.to_todo todo_component) in
  let edited =
    Result.get_ok (Todo.edit ~summary:(Patch.Set "Edited todo") todo)
  in
  let components =
    replace_body_and_reload ~fs calendar_dir components
      (Component.todo_body edited)
  in
  let parsed = Eio.Path.load file |> Icalendar.parse |> Result.get_ok in
  let event_has_extension =
    List.exists
      (function
        | `Event (event : Icalendar.event) ->
            List.exists (has_x_property "EVENT-PROPERTY") event.props
        | _ -> false)
      (snd parsed)
  in
  let journal_has_extension =
    List.exists
      (function
        | `Journal props ->
            List.exists (has_x_property "JOURNAL-PROPERTY") props
        | _ -> false)
      (snd parsed)
  in
  let document_has_extension =
    List.exists (has_x_property "DOCUMENT-PROPERTY") (fst parsed)
  in
  let backups =
    Eio.Path.read_dir calendar_path
    |> List.filter (fun name ->
        String.starts_with ~prefix:".caledonia-backup-mixed.ics-" name)
  in
  Printf.printf "calendar key: %s\n" (Component.get_calendar_key todo_component);
  Printf.printf "display name: %s\n"
    (Component.get_calendar_name todo_component);
  Printf.printf "components after edit: %d/%d/%d\n"
    (let a, _, _ = count_component_types parsed in
     a)
    (let _, b, _ = count_component_types parsed in
     b)
    (let _, _, c = count_component_types parsed in
     c);
  Printf.printf "extensions preserved: %b\n"
    (event_has_extension && journal_has_extension && document_has_extension);
  Printf.printf "backup created: %b\n" (backups <> []);
  let edited_component =
    List.find
      (fun component -> Component.get_id component = "todo-1")
      components
  in
  let _components = remove_and_reload ~fs calendar_dir edited_component in
  let after_delete = Eio.Path.load file |> Icalendar.parse |> Result.get_ok in
  let events, todos, journals = count_component_types after_delete in
  Printf.printf "components after delete: %d/%d/%d\n" events todos journals;
  [%expect
    {|
      calendar key: work
      display name: Team calendar
      components after edit: 1/1/1
      extensions preserved: true
      backup created: true
      components after delete: 1/0/1 |}]

let%expect_test "event and journal mutations preserve mixed-document siblings" =
  with_calendar_dir @@ fun fs root root_path ->
  let calendar_dir, _calendar_path, file =
    prepare_file fs root root_path "work" "mixed.ics" mixed_calendar
  in
  let components =
    Result.get_ok (Calendar_dir.get_components ~fs calendar_dir)
  in
  let event_component =
    List.find
      (fun component -> Component.get_id component = "event-1")
      components
  in
  let event = Option.get (Component.to_event event_component) in
  let event =
    Result.get_ok (Event.edit_patch ~summary:(Patch.Set "Edited event") event)
  in
  let components =
    replace_body_and_reload ~fs calendar_dir components
      (Component.event_body event)
  in
  let after_event = Eio.Path.load file |> Icalendar.parse |> Result.get_ok in
  let event_counts = count_component_types after_event in
  let journal_component =
    List.find
      (fun component -> Component.get_id component = "journal-1")
      components
  in
  let journal = Option.get (Component.to_journal journal_component) in
  let journal =
    Result.get_ok (Journal.edit ~summary:(Patch.Set "Edited journal") journal)
  in
  let components =
    replace_body_and_reload ~fs calendar_dir components
      (Component.journal_body journal)
  in
  let after_journal = Eio.Path.load file |> Icalendar.parse |> Result.get_ok in
  let journal_counts = count_component_types after_journal in
  let event_component =
    List.find
      (fun component -> Component.get_id component = "event-1")
      components
  in
  let components = remove_and_reload ~fs calendar_dir event_component in
  let after_event_delete =
    Eio.Path.load file |> Icalendar.parse |> Result.get_ok
    |> count_component_types
  in
  let journal_component =
    List.find
      (fun component -> Component.get_id component = "journal-1")
      components
  in
  let components = remove_and_reload ~fs calendar_dir journal_component in
  let after_journal_delete =
    Eio.Path.load file |> Icalendar.parse |> Result.get_ok
    |> count_component_types
  in
  let todo_component = List.hd components in
  let remaining = remove_and_reload ~fs calendar_dir todo_component in
  let print_counts label (events, todos, journals) =
    Printf.printf "%s: %d/%d/%d\n" label events todos journals
  in
  print_counts "after event edit" event_counts;
  print_counts "after journal edit" journal_counts;
  print_counts "after event delete" after_event_delete;
  print_counts "after journal delete" after_journal_delete;
  Printf.printf "last component removes file: %b/%b\n" (remaining = [])
    (not (Eio.Path.is_file file));
  [%expect
    {|
      after event edit: 1/1/1
      after journal edit: 1/1/1
      after event delete: 0/1/1
      after journal delete: 0/1/0
      last component removes file: true/true |}]

let%expect_test
    "deleting a recurrence master removes its overrides and preserves siblings"
    =
  with_calendar_dir @@ fun fs root root_path ->
  let opaque =
    String.concat "\r\n"
      [
        "BEGIN:VAVAILABILITY";
        "UID:opaque-sibling";
        "DTSTAMP:20260715T080000Z";
        "X-OPAQUE:preserve-byte-for-byte";
        "END:VAVAILABILITY";
      ]
  in
  let source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Caledonia recurrence deletion tests//EN";
        "X-DOCUMENT-SIBLING:preserve";
        "BEGIN:VEVENT";
        "UID:series-delete";
        "DTSTAMP:20260715T080000Z";
        "DTSTART:20260716T090000Z";
        "RRULE:FREQ=DAILY;COUNT=3";
        "SUMMARY:Series master";
        "END:VEVENT";
        "BEGIN:VEVENT";
        "UID:series-delete";
        "RECURRENCE-ID:20260717T090000Z";
        "DTSTAMP:20260715T080000Z";
        "DTSTART:20260717T100000Z";
        "SUMMARY:Moved override";
        "END:VEVENT";
        "BEGIN:VEVENT";
        "UID:series-delete";
        "RECURRENCE-ID:20260718T090000Z";
        "DTSTAMP:20260715T080000Z";
        "DTSTART:20260718T090000Z";
        "STATUS:CANCELLED";
        "SUMMARY:Cancelled override";
        "END:VEVENT";
        "BEGIN:VEVENT";
        "UID:event-sibling";
        "DTSTAMP:20260715T080000Z";
        "DTSTART:20260720T120000Z";
        "SUMMARY:Unrelated event";
        "END:VEVENT";
        "BEGIN:VTODO";
        "UID:todo-sibling";
        "DTSTAMP:20260715T080000Z";
        "SUMMARY:Unrelated todo";
        "END:VTODO";
        opaque;
        "END:VCALENDAR";
        "";
      ]
  in
  let calendar_dir, calendar_path, file =
    prepare_file fs root root_path "work" "series.ics" source
  in
  let components =
    Result.get_ok (Calendar_dir.get_components ~fs calendar_dir)
  in
  let master =
    List.find
      (fun component -> Component.get_id component = "series-delete")
      components
  in
  let remaining = remove_and_reload ~fs calendar_dir master in
  let written = Eio.Path.load file in
  let remaining_ids =
    List.map Component.get_id remaining |> List.sort compare
  in
  let backups =
    Eio.Path.read_dir calendar_path
    |> List.filter (String.starts_with ~prefix:".caledonia-backup-series.ics-")
  in
  Printf.printf "series/overrides gone: %b/%b\n"
    (not (contains_substring ~needle:"UID:series-delete" written))
    (not (contains_substring ~needle:"RECURRENCE-ID:" written));
  Printf.printf "siblings retained: %b/%b/%b\n"
    (remaining_ids = [ "event-sibling"; "todo-sibling" ])
    (contains_substring ~needle:"X-DOCUMENT-SIBLING:preserve" written)
    (contains_substring ~needle:opaque written);
  Printf.printf "atomic replacement backup: %b; reparses: %b\n" (backups <> [])
    (Result.is_ok (Calendar_codec.Legacy.parse written));
  [%expect
    {|
    series/overrides gone: true/true
    siblings retained: true/true/true
    atomic replacement backup: true; reparses: true |}]

let%expect_test "master edits synchronize authored recurrence overrides" =
  with_calendar_dir @@ fun fs root root_path ->
  let run calendar_key file_name recurrence_patch =
    let source =
      String.concat "\r\n"
        [
          "BEGIN:VCALENDAR";
          "VERSION:2.0";
          "PRODID:-//Caledonia recurrence edit transaction tests//EN";
          "BEGIN:VEVENT";
          "UID:series-edit";
          "DTSTAMP:20260715T080000Z";
          "DTSTART:20260716T090000Z";
          "RRULE:FREQ=DAILY;COUNT=3";
          "SUMMARY:Series master";
          "END:VEVENT";
          "BEGIN:VEVENT";
          "UID:series-edit";
          "RECURRENCE-ID:20260717T090000Z";
          "DTSTAMP:20260715T080000Z";
          "DTSTART:20260717T100000Z";
          "SUMMARY:Moved override";
          "END:VEVENT";
          "BEGIN:VEVENT";
          "UID:sibling";
          "DTSTAMP:20260715T080000Z";
          "DTSTART:20260720T120000Z";
          "SUMMARY:Sibling";
          "END:VEVENT";
          "END:VCALENDAR";
          "";
        ]
    in
    let calendar_dir, _, file =
      prepare_file fs root root_path calendar_key file_name source
    in
    let components =
      Result.get_ok (Calendar_dir.get_components ~fs calendar_dir)
    in
    let master =
      List.find
        (fun component -> Component.get_id component = "series-edit")
        components
      |> Component.to_event |> Option.get
    in
    let ordinary =
      Result.get_ok (Event.edit_patch ~summary:(Patch.Set "Renamed") master)
    in
    let components =
      replace_body_and_reload ~fs calendar_dir components
        (Component.event_body ordinary)
    in
    let after_ordinary = Eio.Path.load file in
    let canonical_master =
      List.find
        (fun component -> Component.get_id component = "series-edit")
        components
      |> Component.to_event |> Option.get
    in
    let reset =
      Result.get_ok
        (Event.edit_patch ~recurrence:recurrence_patch canonical_master)
    in
    let _ =
      replace_body_and_reload ~fs calendar_dir components
        (Component.event_body reset)
    in
    let after_reset = Eio.Path.load file in
    ( contains_substring ~needle:"RECURRENCE-ID:20260717T090000Z" after_ordinary,
      not (contains_substring ~needle:"RECURRENCE-ID:" after_reset),
      contains_substring ~needle:"UID:sibling" after_reset )
  in
  let clear_result = run "clear" "series.ics" Patch.Clear in
  let change_result =
    run "change" "series.ics" (Patch.Set (`Daily, Some (`Count 5), None, []))
  in
  let print label (ordinary, reset, sibling) =
    Printf.printf "%s ordinary-preserves=%b reset-removes=%b sibling=%b\n" label
      ordinary reset sibling
  in
  print "clear" clear_result;
  print "change" change_result;
  [%expect
    {|
    clear ordinary-preserves=true reset-removes=true sibling=true
    change ordinary-preserves=true reset-removes=true sibling=true |}]

let%expect_test "pure event edits do not acquire repository mutation authority"
    =
  with_calendar_dir @@ fun fs root root_path ->
  let instant date time = Option.get (Ptime.of_date_time (date, (time, 0))) in
  let source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Caledonia fingerprint tests//EN";
        "BEGIN:VEVENT";
        "UID:fingerprint-series";
        "DTSTAMP:20260715T080000Z";
        "DTSTART:20260716T090000Z";
        "RRULE:FREQ=DAILY;COUNT=2";
        "SUMMARY:Series";
        "END:VEVENT";
        "END:VCALENDAR";
        "";
      ]
  in
  let _calendar_dir, _, file =
    prepare_file fs root root_path "work" "fingerprint.ics" source
  in
  let calendar = Result.get_ok (Icalendar.parse source) in
  let pure_series =
    snd calendar
    |> List.filter_map (function `Event event -> Some event | _ -> None)
    |> Event.of_events_result |> Result.get_ok |> List.hd
  in
  let edited =
    Result.get_ok
      (Event.edit_patch ~summary:(Patch.Set "Pure edit") pure_series)
  in
  let occurrence = instant (2026, 7, 17) (9, 0, 0) in
  let reference =
    Event.Recurrence.resolve_reference ~floating_tz:Timedesc.Time_zone.utc
      pure_series occurrence
    |> Result.get_ok
  in
  let override =
    Result.get_ok
      (Event.Recurrence.create_override
         ~now:(instant (2026, 7, 15) (8, 0, 0))
         pure_series reference ())
  in
  Printf.printf "pure operations succeeded=%b/%b bytes unchanged=%b\n"
    (Event.get_summary edited = Some "Pure edit")
    (Option.is_some (Component_identity.of_ical_component (`Event override)))
    (String.equal source (Eio.Path.load file));
  [%expect {| pure operations succeeded=true/true bytes unchanged=true |}]

let%expect_test "a stale component cannot overwrite an external change" =
  with_calendar_dir @@ fun fs root root_path ->
  let calendar_dir, _calendar_path, file =
    prepare_file fs root root_path "work" "mixed.ics" mixed_calendar
  in
  let components =
    Result.get_ok (Calendar_dir.get_components ~fs calendar_dir)
  in
  let todo_component =
    List.find
      (fun component -> Component.get_id component = "todo-1")
      components
  in
  let todo = Option.get (Component.to_todo todo_component) in
  let edited =
    Result.get_ok (Todo.edit ~summary:(Patch.Set "Stale edit") todo)
  in
  let externally_modified =
    Str.global_replace
      (Str.regexp_string "Sibling event")
      "Externally changed event" mixed_calendar
  in
  Eio.Path.save ~create:(`Or_truncate 0o600) file externally_modified;
  let result =
    Calendar_dir.replace_stored_component ~fs calendar_dir
      ~original:todo_component
      ~replacement:(Component.todo_body edited)
  in
  Printf.printf "conflict: %b\n"
    (match result with
    | Error (Storage_error.Conflict _) -> true
    | Error _ | Ok _ -> false);
  Printf.printf "external bytes preserved: %b\n"
    (String.equal externally_modified (Eio.Path.load file));
  [%expect {|
      conflict: true
      external bytes preserved: true |}]

let injected_edit ~fs ~calendar_dir components todo failure =
  let edited =
    Result.get_ok (Todo.edit ~summary:(Patch.Set "Injected edit") todo)
  in
  Calendar_dir.For_test.inject_next failure;
  let original =
    List.find
      (fun component -> Component.get_id component = Todo.get_id todo)
      components
  in
  Calendar_dir.replace_stored_component ~fs calendar_dir ~original
    ~replacement:(Component.todo_body edited)

let temporary_files calendar_path =
  Eio.Path.read_dir calendar_path
  |> List.filter (String.starts_with ~prefix:".caledonia-tmp-")

let%expect_test "temporary write mismatch preserves source and cleans temp file"
    =
  with_calendar_dir @@ fun fs root root_path ->
  let calendar_dir, calendar_path, file =
    prepare_file fs root root_path "work" "mixed.ics" mixed_calendar
  in
  let components =
    Result.get_ok (Calendar_dir.get_components ~fs calendar_dir)
  in
  let todo =
    List.find
      (fun component -> Component.get_id component = "todo-1")
      components
    |> Component.to_todo |> Option.get
  in
  let result =
    injected_edit ~fs ~calendar_dir components todo
      Calendar_dir.For_test.Post_write_mismatch
  in
  Printf.printf "write mismatch rejected: %b\n"
    (match result with
    | Error (Storage_error.Io _) -> true
    | Error _ | Ok _ -> false);
  Printf.printf "source bytes preserved: %b\n"
    (String.equal mixed_calendar (Eio.Path.load file));
  Printf.printf "temporary files cleaned: %b\n"
    (temporary_files calendar_path = []);
  [%expect
    {|
      write mismatch rejected: true
      source bytes preserved: true
      temporary files cleaned: true |}]

let%expect_test "rename failure preserves source and cleans temp file" =
  with_calendar_dir @@ fun fs root root_path ->
  let calendar_dir, calendar_path, file =
    prepare_file fs root root_path "work" "mixed.ics" mixed_calendar
  in
  let components =
    Result.get_ok (Calendar_dir.get_components ~fs calendar_dir)
  in
  let todo =
    List.find
      (fun component -> Component.get_id component = "todo-1")
      components
    |> Component.to_todo |> Option.get
  in
  let result =
    injected_edit ~fs ~calendar_dir components todo
      Calendar_dir.For_test.Rename_failure
  in
  Printf.printf "rename failure reported: %b\n"
    (match result with
    | Error (Storage_error.Io _) -> true
    | Error _ | Ok _ -> false);
  Printf.printf "source bytes preserved: %b\n"
    (String.equal mixed_calendar (Eio.Path.load file));
  Printf.printf "temporary files cleaned: %b\n"
    (temporary_files calendar_path = []);
  [%expect
    {|
      rename failure reported: true
      source bytes preserved: true
      temporary files cleaned: true |}]

let%expect_test
    "temporary parse validation failure preserves source and cleans temp file" =
  with_calendar_dir @@ fun fs root root_path ->
  let calendar_dir, calendar_path, file =
    prepare_file fs root root_path "work" "mixed.ics" mixed_calendar
  in
  let components =
    Result.get_ok (Calendar_dir.get_components ~fs calendar_dir)
  in
  let todo =
    List.find
      (fun component -> Component.get_id component = "todo-1")
      components
    |> Component.to_todo |> Option.get
  in
  let result =
    injected_edit ~fs ~calendar_dir components todo
      Calendar_dir.For_test.Temporary_parse_failure
  in
  Printf.printf "parse failure/source/temp: %b/%b/%b\n" (Result.is_error result)
    (String.equal mixed_calendar (Eio.Path.load file))
    (temporary_files calendar_path = []);
  [%expect {| parse failure/source/temp: true/true/true |}]

let%expect_test "duplicate writable identities are rejected during decode" =
  with_calendar_dir @@ fun fs root root_path ->
  let calendar_dir, _calendar_path, file =
    prepare_file fs root root_path "work" "duplicate.ics"
      duplicate_todo_calendar
  in
  let result = Calendar_dir.get_components ~fs calendar_dir in
  Printf.printf "decode rejected: %b\n" (Result.is_error result);
  Printf.printf "original bytes preserved: %b\n"
    (String.equal duplicate_todo_calendar (Eio.Path.load file));
  [%expect
    {|
      decode rejected: true
      original bytes preserved: true |}]

let%expect_test "corrupt event master identity is surfaced as a load error" =
  with_calendar_dir @@ fun fs root root_path ->
  let calendar_dir, _calendar_path, _file =
    prepare_file fs root root_path "work" "duplicate-event.ics"
      duplicate_event_master_calendar
  in
  let result = Calendar_dir.get_components ~fs calendar_dir in
  Printf.printf "typed load error: %b\n"
    (match result with Error (`Msg _) -> true | _ -> false);
  [%expect {| typed load error: true |}]

let%expect_test "calendar display names are never ambiguous write identities" =
  with_calendar_dir @@ fun fs root root_path ->
  let calendar_dir, first, _ =
    prepare_file fs root root_path "one" "one.ics" mixed_calendar
  in
  let second = Eio.Path.(root_path / "two") in
  Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 second;
  Eio.Path.save ~create:(`Exclusive 0o600)
    Eio.Path.(second / "two.ics")
    mixed_calendar;
  Eio.Path.save ~create:(`Exclusive 0o600)
    Eio.Path.(first / "displayname")
    "Shared\n";
  Eio.Path.save ~create:(`Exclusive 0o600)
    Eio.Path.(second / "displayname")
    "Shared\n";
  let exact = Calendar_dir.resolve_calendar_key ~fs calendar_dir "one" in
  let duplicate = Calendar_dir.resolve_calendar_key ~fs calendar_dir "Shared" in
  let traversal =
    Calendar_dir.get_calendar_components ~fs calendar_dir "../one"
  in
  Printf.printf "exact key resolves: %b\n" (exact = Ok "one");
  Printf.printf "display name rejected as identity: %b\n"
    (duplicate = Error `Not_found);
  Printf.printf "traversal rejected: %b\n"
    (match traversal with Error (`Msg _) -> true | _ -> false);
  [%expect
    {|
      exact key resolves: true
      display name rejected as identity: true
      traversal rejected: true |}]

let%expect_test "calendar loading refuses symbolic-link escapes" =
  with_calendar_dir @@ fun fs root root_path ->
  let calendar_dir, calendar_path, file =
    prepare_file fs root root_path "work" "mixed.ics" mixed_calendar
  in
  let linked = Eio.Path.(calendar_path / "linked.ics") in
  Unix.symlink (Eio.Path.native_exn file) (Eio.Path.native_exn linked);
  let nested_link = Calendar_dir.get_components ~fs calendar_dir in
  let linked_calendar = Eio.Path.(root_path / "linked-calendar") in
  Unix.symlink
    (Eio.Path.native_exn calendar_path)
    (Eio.Path.native_exn linked_calendar);
  let listed =
    Result.get_ok (Calendar_dir.list_calendar_names ~fs calendar_dir)
  in
  Printf.printf "nested link rejected: %b\n"
    (match nested_link with Error (`Msg _) -> true | _ -> false);
  Printf.printf "calendar link excluded: %b\n"
    (not (List.mem "linked-calendar" listed));
  [%expect
    {|
      nested link rejected: true
      calendar link excluded: true |}]

let%expect_test "unknown calendars require explicit creation" =
  with_calendar_dir @@ fun fs root root_path ->
  let calendar_dir = Result.get_ok (Calendar_dir.create ~fs root) in
  let todo = Result.get_ok (Todo.create ~summary:"No typo directory" ()) in
  let result =
    Calendar_dir.create_stored_component ~fs calendar_dir
      ~calendar_key:"misspelled" (Component.todo_body todo)
  in
  Printf.printf "implicit creation rejected: %b\n"
    (match result with
    | Error (Storage_error.Missing_target _) -> true
    | Error _ | Ok _ -> false);
  Printf.printf "directory absent: %b\n"
    (not (Eio.Path.is_directory Eio.Path.(root_path / "misspelled")));
  let created = Calendar_dir.create_calendar ~fs calendar_dir "misspelled" in
  Printf.printf "explicit creation succeeds: %b\n" (Result.is_ok created);
  [%expect
    {|
      implicit creation rejected: true
      directory absent: true
      explicit creation succeeds: true |}]

let%expect_test "a persistent lock filename is not a stale lock" =
  with_calendar_dir @@ fun fs root root_path ->
  let calendar_dir, calendar_path, _file =
    prepare_file fs root root_path "work" "mixed.ics" mixed_calendar
  in
  let lock = Eio.Path.(calendar_path / ".caledonia-lock-mixed.ics.lock") in
  Eio.Path.save ~create:(`Exclusive 0o600) lock "previous process\n";
  let components =
    Result.get_ok (Calendar_dir.get_components ~fs calendar_dir)
  in
  let todo_component =
    List.find
      (fun component -> Component.get_id component = "todo-1")
      components
  in
  let todo = Option.get (Component.to_todo todo_component) in
  let edited =
    Result.get_ok (Todo.edit ~summary:(Patch.Set "Lock recovered") todo)
  in
  let result =
    Calendar_dir.replace_stored_component ~fs calendar_dir
      ~original:todo_component
      ~replacement:(Component.todo_body edited)
  in
  Printf.printf "write succeeds: %b\n" (Result.is_ok result);
  Printf.printf "lock filename retained: %b\n" (Eio.Path.is_file lock);
  [%expect {|
      write succeeds: true
      lock filename retained: true |}]

let relative_alarm related seconds : Icalendar.alarm =
  let params = Icalendar.Params.empty |> Icalendar.Params.add Related related in
  `Display
    {
      Icalendar.trigger = (params, `Duration (Ptime.Span.of_int_s seconds));
      duration_repeat = None;
      summary = None;
      other = [];
      special =
        ({ description = Some (Icalendar.Params.empty, "Reminder") }
          : Icalendar.display_struct);
    }

let absolute_alarm instant : Icalendar.alarm =
  let params =
    Icalendar.Params.empty |> Icalendar.Params.add Icalendar.Valuetype `Datetime
  in
  `Display
    {
      Icalendar.trigger = (params, `Datetime instant);
      duration_repeat = None;
      summary = None;
      other = [];
      special =
        ({ description = Some (Icalendar.Params.empty, "Reminder") }
          : Icalendar.display_struct);
    }

let ptime date time = Option.get (Ptime.of_date_time (date, (time, 0)))

let short_time value =
  let _, ((hour, minute, second), _) = Ptime.to_date_time value in
  Printf.sprintf "%02d:%02d:%02d" hour minute second

let%expect_test "todo alarms distinguish START, END, and absolute triggers" =
  Eio_main.run @@ fun _env ->
  let start = ptime (2026, 1, 2) (10, 0, 30) in
  let due = ptime (2026, 1, 2) (12, 0, 30) in
  let absolute = ptime (2026, 1, 2) (8, 0, 45) in
  let todo =
    Result.get_ok
      (Todo.create
         ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
         ~due:(Icalendar.Params.empty, `Datetime (`Utc due))
         ~alarms:
           [
             absolute_alarm absolute;
             relative_alarm `Start (-90);
             relative_alarm `End (-90);
           ]
         ())
  in
  let fires =
    Todo.compute_alarm_fires_result ~floating_tz:Timedesc.Time_zone.utc
      ~from:(Some (ptime (2026, 1, 2) (0, 0, 0)))
      ~to_:(ptime (2026, 1, 3) (0, 0, 0))
      todo
    |> Result.get_ok
  in
  List.iter
    (fun (fire : Todo.t Alarm.fire) ->
      print_endline (short_time fire.fire_time))
    fires;
  [%expect {|
      08:00:45
      09:59:00
      11:59:00 |}]

let result_is_error = function Error (`Msg _) -> true | Ok _ -> false

let%expect_test "todo creation rejects invalid domain values" =
  Eio_main.run @@ fun _env ->
  let create ?start ?due ?duration ?status ?priority ?percent () =
    Todo.create ?start ?due ?duration ?status ?priority ?percent ()
  in
  let start = ptime (2026, 1, 2) (10, 0, 0) in
  let earlier = ptime (2026, 1, 2) (9, 0, 0) in
  Printf.printf "priority: %b\n" (result_is_error (create ~priority:10 ()));
  Printf.printf "percent: %b\n" (result_is_error (create ~percent:101 ()));
  Printf.printf "status: %b\n" (result_is_error (create ~status:`Draft ()));
  Printf.printf "due before start: %b\n"
    (result_is_error
       (create
          ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
          ~due:(Icalendar.Params.empty, `Datetime (`Utc earlier))
          ()));
  Printf.printf "due equal start: %b\n"
    (result_is_error
       (create
          ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
          ~due:(Icalendar.Params.empty, `Datetime (`Utc start))
          ()));
  Printf.printf "due plus duration: %b\n"
    (result_is_error
       (create
          ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
          ~due:
            ( Icalendar.Params.empty,
              `Datetime (`Utc (ptime (2026, 1, 2) (11, 0, 0))) )
          ~duration:(Icalendar.Params.empty, Ptime.Span.of_int_s 3600)
          ()));
  Printf.printf "duration without start/nonpositive: %b/%b\n"
    (result_is_error
       (create ~duration:(Icalendar.Params.empty, Ptime.Span.of_int_s 3600) ()))
    (result_is_error
       (create
          ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
          ~duration:(Icalendar.Params.empty, Ptime.Span.zero)
          ()));
  Printf.printf "mixed date kinds: %b\n"
    (result_is_error
       (create
          ~start:(Icalendar.Params.empty, `Date (2026, 1, 2))
          ~due:(Icalendar.Params.empty, `Datetime (`Utc start))
          ()));
  [%expect
    {|
      priority: true
      percent: true
      status: true
      due before start: true
      due equal start: true
      due plus duration: true
      duration without start/nonpositive: true/true
      mixed date kinds: true |}]

let%expect_test "direct temporal parameter validation covers every component" =
  Eio_main.run @@ fun _env ->
  let instant = ptime (2026, 7, 15) (10, 0, 0) in
  let datetime = `Datetime (`Utc instant) in
  let extension =
    Icalendar.Params.empty
    |> Icalendar.Params.add
         (Icalendar.Xparam ("TEST", "TRACE"))
         [ `String "kept" ]
  in
  let wrong_value =
    Icalendar.Params.empty |> Icalendar.Params.add Icalendar.Valuetype `Date
  in
  let utc_tzid =
    Icalendar.Params.empty
    |> Icalendar.Params.add Icalendar.Tzid (false, "Europe/London")
  in
  let unrelated =
    Icalendar.Params.empty |> Icalendar.Params.add Icalendar.Language "en"
  in
  let date = `Date (2026, 7, 15) in
  let date_params =
    Icalendar.Params.empty |> Icalendar.Params.add Icalendar.Valuetype `Date
  in
  let fractional_second = Option.get (Ptime.Span.of_d_ps (0, 1L)) in
  let fractional_timestamp =
    Option.get (Ptime.add_span instant fractional_second)
  in
  let event =
    Event.create ~summary:"valid" ~start:(extension, datetime) ()
    |> Result.get_ok
  in
  let invalid_event_create =
    Event.create ~summary:"invalid" ~start:(wrong_value, datetime) ()
  in
  let invalid_event_edit =
    Event.edit_patch
      ~end_:(Patch.Set (`Duration (unrelated, Ptime.Span.of_int_s 60)))
      event
  in
  let invalid_event_date =
    Event.create ~summary:"invalid" ~start:(Icalendar.Params.empty, date) ()
  in
  let invalid_event_fraction =
    Event.create ~summary:"invalid"
      ~start:(Icalendar.Params.empty, datetime)
      ~end_:(`Duration (Icalendar.Params.empty, fractional_second))
      ()
  in
  let invalid_event_fractional_time =
    Event.create ~summary:"invalid"
      ~start:(Icalendar.Params.empty, `Datetime (`Utc fractional_timestamp))
      ()
  in
  let valid_date_event =
    Event.create ~summary:"valid date" ~start:(date_params, date) ()
    |> Result.get_ok
  in
  let todo = Todo.create ~start:(extension, datetime) () |> Result.get_ok in
  let invalid_todo_create = Todo.create ~due:(utc_tzid, datetime) () in
  let invalid_todo_edit =
    Todo.edit ~duration:(Patch.Set (unrelated, Ptime.Span.of_int_s 60)) todo
  in
  let invalid_todo_date = Todo.create ~due:(Icalendar.Params.empty, date) () in
  let invalid_todo_fraction =
    Todo.create
      ~start:(Icalendar.Params.empty, datetime)
      ~duration:(Icalendar.Params.empty, fractional_second)
      ()
  in
  let invalid_todo_fractional_time =
    Todo.create
      ~start:(Icalendar.Params.empty, `Datetime (`Local fractional_timestamp))
      ()
  in
  let valid_date_todo =
    Todo.create ~due:(date_params, date) () |> Result.get_ok
  in
  let journal =
    Journal.create ~start:(extension, datetime) () |> Result.get_ok
  in
  let invalid_journal_create = Journal.create ~start:(utc_tzid, datetime) () in
  let invalid_journal_edit =
    Journal.edit ~start:(Patch.Set (wrong_value, datetime)) journal
  in
  let invalid_journal_date =
    Journal.create ~start:(Icalendar.Params.empty, date) ()
  in
  let invalid_journal_fractional_time =
    Journal.create
      ~start:
        ( Icalendar.Params.empty,
          `Datetime
            (`With_tzid (fractional_timestamp, (false, "Europe/London"))) )
      ()
  in
  let valid_date_journal =
    Journal.create ~start:(date_params, date) () |> Result.get_ok
  in
  Printf.printf "event create/edit=%b/%b\n"
    (result_is_error invalid_event_create)
    (result_is_error invalid_event_edit);
  Printf.printf "todo create/edit=%b/%b\n"
    (result_is_error invalid_todo_create)
    (result_is_error invalid_todo_edit);
  Printf.printf "journal create/edit=%b/%b\n"
    (result_is_error invalid_journal_create)
    (result_is_error invalid_journal_edit);
  Printf.printf "DATE VALUE required=%b/%b/%b\n"
    (result_is_error invalid_event_date)
    (result_is_error invalid_todo_date)
    (result_is_error invalid_journal_date);
  Printf.printf "fractional DURATION rejected=%b/%b\n"
    (result_is_error invalid_event_fraction)
    (result_is_error invalid_todo_fraction);
  Printf.printf "fractional DATE-TIME rejected=%b/%b/%b\n"
    (result_is_error invalid_event_fractional_time)
    (result_is_error invalid_todo_fractional_time)
    (result_is_error invalid_journal_fractional_time);
  let date_serializations =
    [
      serialize_body (Component.event_body valid_date_event);
      serialize_body (Component.todo_body valid_date_todo);
      serialize_body (Component.journal_body valid_date_journal);
    ]
  in
  Printf.printf "valid DATE serialization=%b\n"
    (List.for_all
       (contains_substring ~needle:";VALUE=DATE:")
       date_serializations);
  [%expect
    {|
      event create/edit=true/true
      todo create/edit=true/true
      journal create/edit=true/true
      DATE VALUE required=true/true/true
      fractional DURATION rejected=true/true
      fractional DATE-TIME rejected=true/true/true
      valid DATE serialization=true |}]

let%expect_test "todo END-relative alarm supports DTSTART plus DURATION" =
  Eio_main.run @@ fun _env ->
  let start = ptime (2026, 1, 2) (10, 0, 30) in
  let todo =
    Result.get_ok
      (Todo.create
         ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
         ~duration:(Icalendar.Params.empty, Ptime.Span.of_int_s 7200)
         ~alarms:[ relative_alarm `End (-90) ]
         ())
  in
  Todo.compute_alarm_fires_result ~floating_tz:Timedesc.Time_zone.utc
    ~from:(Some (ptime (2026, 1, 2) (11, 58, 0)))
    ~to_:(ptime (2026, 1, 2) (12, 1, 0))
    todo
  |> Result.get_ok
  |> List.iter (fun (fire : Todo.t Alarm.fire) ->
      print_endline (short_time fire.fire_time));
  [%expect {| 11:59:00 |}]

let%expect_test "todo patches clear every optional editable field" =
  Eio_main.run @@ fun _env ->
  let start = ptime (2026, 1, 2) (10, 0, 0) in
  let due = ptime (2026, 1, 2) (11, 0, 0) in
  let todo =
    Result.get_ok
      (Todo.create ~summary:"summary"
         ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
         ~due:(Icalendar.Params.empty, `Datetime (`Utc due))
         ~description:"description" ~categories:[ "one" ] ~status:`In_process
         ~priority:4 ~percent:50 ~parent:"parent"
         ~alarms:[ relative_alarm `Start (-60) ]
         ())
  in
  let cleared =
    Result.get_ok
      (Todo.edit ~summary:Patch.Clear ~start:Patch.Clear ~due:Patch.Clear
         ~description:Patch.Clear ~categories:Patch.Clear ~status:Patch.Clear
         ~priority:Patch.Clear ~percent:Patch.Clear ~parent:Patch.Clear
         ~alarms:Patch.Clear todo)
  in
  let absent =
    [
      Todo.get_summary cleared;
      Option.map (fun _ -> "present") (Todo.get_start_time cleared);
      Option.map (fun _ -> "present") (Todo.get_due_time cleared);
      Todo.get_description cleared;
      Option.map string_of_int (Todo.get_priority cleared);
      Option.map string_of_int (Todo.get_percent cleared);
      Todo.get_related_parent cleared;
    ]
    |> List.for_all Option.is_none
  in
  Printf.printf "scalar fields absent: %b\n" absent;
  Printf.printf "categories absent: %b\n" (Todo.get_categories cleared = []);
  Printf.printf "status absent: %b\n" (Todo.get_status cleared = None);
  Printf.printf "alarms absent: %b\n" (Todo.get_alarms cleared = []);
  [%expect
    {|
      scalar fields absent: true
      categories absent: true
      status absent: true
      alarms absent: true |}]

let%expect_test "todo completion transitions share one state machine" =
  Eio_main.run @@ fun _env ->
  let todo = Result.get_ok (Todo.create ()) in
  let completed =
    Result.get_ok
      (Todo.edit ~status:(Patch.Set `Completed) ~percent:(Patch.Set 100) todo)
  in
  Printf.printf "completed: %b/%s/%b\n"
    (Todo.is_completed completed)
    (Option.fold ~none:"none" ~some:string_of_int (Todo.get_percent completed))
    (Option.is_some (Todo.get_completed completed));
  let reopened = Result.get_ok (Todo.edit ~percent:(Patch.Set 50) completed) in
  Printf.printf "reopened: %b/%b/%b\n"
    (Todo.get_status reopened = Some `In_process)
    (Todo.get_percent reopened = Some 50)
    (Todo.get_completed reopened = None);
  let incompatible =
    Todo.edit ~status:(Patch.Set `Completed) ~percent:(Patch.Set 50) todo
  in
  Printf.printf "incompatible rejected: %b\n" (result_is_error incompatible);
  let cleared = Result.get_ok (Todo.edit ~status:Patch.Clear completed) in
  Printf.printf "clear completion: %b/%b/%b\n"
    (Todo.get_status cleared = None)
    (Todo.get_percent cleared = None)
    (Todo.get_completed cleared = None);
  [%expect
    {|
      completed: true/100/true
      reopened: true/true/true
      incompatible rejected: true
      clear completion: true/true/true |}]

let%expect_test
    "todo parent graph rejects missing parents, self links, and cycles" =
  Eio_main.run @@ fun _env ->
  let create ?parent summary =
    Result.get_ok (Todo.create ~summary ?parent ())
  in
  let parent = create "parent" in
  let child = create ~parent:(Todo.get_id parent) "child" in
  let orphan = create ~parent:"missing" "orphan" in
  let cycle_parent =
    Result.get_ok (Todo.edit ~parent:(Patch.Set (Todo.get_id child)) parent)
  in
  Printf.printf "valid graph: %b\n"
    (Result.is_ok (Todo.validate_parent_graph [ parent; child ]));
  Printf.printf "missing parent rejected: %b\n"
    (result_is_error (Todo.validate_parent_graph [ orphan ]));
  Printf.printf "cycle rejected: %b\n"
    (result_is_error (Todo.validate_parent_graph [ cycle_parent; child ]));
  Printf.printf "self edit rejected: %b\n"
    (result_is_error
       (Todo.edit ~parent:(Patch.Set (Todo.get_id parent)) parent));
  let ancestors =
    Result.get_ok (Todo.get_ancestors ~all_todos:[ parent; child ] child)
  in
  Printf.printf "checked ancestors: %d\n" (List.length ancestors);
  Printf.printf "corrupt tree surfaced: %b\n"
    (result_is_error (Todo.build_todo_tree [ cycle_parent; child ]));
  [%expect
    {|
      valid graph: true
      missing parent rejected: true
      cycle rejected: true
      self edit rejected: true
      checked ancestors: 1
      corrupt tree surfaced: true |}]

let%expect_test "due-only and DATE todos use explicit range semantics" =
  Eio_main.run @@ fun _env ->
  let todo =
    Result.get_ok
      (Todo.create
         ~due:
           ( Icalendar.Params.add Icalendar.Valuetype `Date
               Icalendar.Params.empty,
             `Date (2026, 4, 10) )
         ())
  in
  let component_start = Todo.get_due_time todo in
  let same_day = ptime (2026, 4, 10) (23, 59, 59) in
  let next_day = ptime (2026, 4, 11) (0, 0, 0) in
  Printf.printf "due-only range anchor: %b\n" (Option.is_some component_start);
  Printf.printf "same local day overdue: %b\n"
    (Result.get_ok
       (Todo.is_overdue_at ~now:same_day ~tz:Timedesc.Time_zone.utc todo));
  Printf.printf "next local day overdue: %b\n"
    (Result.get_ok
       (Todo.is_overdue_at ~now:next_day ~tz:Timedesc.Time_zone.utc todo));
  let los_angeles =
    Option.get (Timedesc.Time_zone.make "America/Los_Angeles")
  in
  let before_local_midnight = ptime (2026, 4, 11) (6, 30, 0) in
  let at_local_midnight = ptime (2026, 4, 11) (7, 0, 0) in
  Printf.printf "explicit-zone overdue boundary: %b/%b\n"
    (not
       (Result.get_ok
          (Todo.is_overdue_at ~now:before_local_midnight ~tz:los_angeles todo)))
    (Result.get_ok
       (Todo.is_overdue_at ~now:at_local_midnight ~tz:los_angeles todo));
  let unknown =
    Result.get_ok
      (Todo.create
         ~due:
           ( Icalendar.Params.empty,
             `Datetime (`With_tzid (same_day, (false, "Mars/Olympus"))) )
         ())
  in
  Printf.printf "unknown TZID is typed error: %b\n"
    (match Todo.get_due_result ~floating_tz:Timedesc.Time_zone.utc unknown with
    | Error (`Unknown_timezone _) -> true
    | _ -> false);
  let maximum_date =
    Result.get_ok
      (Todo.create
         ~due:
           ( Icalendar.Params.add Icalendar.Valuetype `Date
               Icalendar.Params.empty,
             `Date (9999, 12, 31) )
         ())
  in
  Printf.printf "maximum DATE overflow is typed: %b\n"
    (match
       Todo.is_overdue_at
         ~now:(ptime (9999, 12, 31) (0, 0, 0))
         ~tz:Timedesc.Time_zone.utc maximum_date
     with
    | Error (`Out_of_range _) -> true
    | Ok _ | Error _ -> false);
  [%expect
    {|
      due-only range anchor: true
      same local day overdue: false
      next local day overdue: true
      explicit-zone overdue boundary: true/true
      unknown TZID is typed error: true
      maximum DATE overflow is typed: true |}]

let%expect_test "journal validation and clear patches are explicit" =
  Eio_main.run @@ fun _env ->
  let invalid = Journal.create ~status:`Needs_action () in
  let journal =
    Result.get_ok
      (Journal.create ~summary:"summary"
         ~start:
           ( Icalendar.Params.add Icalendar.Valuetype `Date
               Icalendar.Params.empty,
             `Date (2026, 1, 2) )
         ~description:"description" ~categories:[ "one" ] ~status:`Draft ())
  in
  let cleared =
    Result.get_ok
      (Journal.edit ~summary:Patch.Clear ~start:Patch.Clear
         ~description:Patch.Clear ~categories:Patch.Clear ~status:Patch.Clear
         journal)
  in
  Printf.printf "invalid status rejected: %b\n" (result_is_error invalid);
  Printf.printf "all fields cleared: %b\n"
    (Journal.get_summary cleared = None
    && Journal.get_start_time cleared = None
    && Journal.get_description cleared = None
    && Journal.get_categories cleared = []
    && Journal.get_status cleared = None);
  [%expect
    {|
      invalid status rejected: true
      all fields cleared: true |}]

let timezone_calendar ?(offset = "+0000") ?(reference_timezone = true) ~tzid
    ~uid () =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia export tests//EN";
      "BEGIN:VTIMEZONE";
      "TZID:" ^ tzid;
      "BEGIN:STANDARD";
      "DTSTART:19700101T000000";
      "TZOFFSETFROM:" ^ offset;
      "TZOFFSETTO:" ^ offset;
      "END:STANDARD";
      "END:VTIMEZONE";
      "BEGIN:VEVENT";
      "UID:" ^ uid;
      "DTSTAMP:20260101T000000Z";
      (if reference_timezone then "DTSTART;TZID=" ^ tzid ^ ":20260102T090000"
       else "DTSTART:20260102T090000Z");
      "SUMMARY:" ^ uid;
      "END:VEVENT";
      "END:VCALENDAR";
      "";
    ]

let%expect_test
    "ICS export merges VTIMEZONE support from every selected calendar" =
  with_calendar_dir @@ fun fs root root_path ->
  let calendar_dir, _, _ =
    prepare_file fs root root_path "one" "one.ics"
      (timezone_calendar ~tzid:"Example/One" ~uid:"one" ())
  in
  let two_path = Eio.Path.(root_path / "two") in
  Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 two_path;
  Eio.Path.save ~create:(`Exclusive 0o600)
    Eio.Path.(two_path / "two.ics")
    (timezone_calendar ~tzid:"Example/Two" ~uid:"two" ());
  let exported = Result.get_ok (export_all_stored ~fs calendar_dir) in
  let _, parsed_components = Result.get_ok (Icalendar.parse exported) in
  let timezone_count =
    List.fold_left
      (fun count -> function `Timezone _ -> count + 1 | _ -> count)
      0 parsed_components
  in
  let event_count =
    List.fold_left
      (fun count -> function `Event _ -> count + 1 | _ -> count)
      0 parsed_components
  in
  Printf.printf "timezones/events: %d/%d\n" timezone_count event_count;
  [%expect {| timezones/events: 2/2 |}]

let%expect_test
    "ICS export preserves identical selected components from distinct calendars"
    =
  with_calendar_dir @@ fun fs root root_path ->
  let identical_source =
    timezone_calendar ~tzid:"Example/Identical" ~uid:"identical" ()
  in
  let calendar_dir, _, _ =
    prepare_file fs root root_path "one" "one.ics" identical_source
  in
  let two_path = Eio.Path.(root_path / "two") in
  Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 two_path;
  Eio.Path.save ~create:(`Exclusive 0o600)
    Eio.Path.(two_path / "two.ics")
    identical_source;
  let exported =
    export_all_stored ~fs calendar_dir
    |> Result.get_ok |> Calendar_codec.Legacy.parse |> Result.get_ok
  in
  let timezone_count, event_count =
    List.fold_left
      (fun (timezones, events) -> function
        | `Timezone _ -> (timezones + 1, events)
        | `Event _ -> (timezones, events + 1)
        | _ -> (timezones, events))
      (0, 0) (snd exported)
  in
  Printf.printf "timezones/events: %d/%d\n" timezone_count event_count;
  [%expect {| timezones/events: 1/2 |}]

let%expect_test
    "ICS export deduplicates identical TZIDs and rejects conflicting ones" =
  with_calendar_dir @@ fun fs root root_path ->
  let calendar_dir, _, one_file =
    prepare_file fs root root_path "one" "one.ics"
      (timezone_calendar ~tzid:"Example/Shared" ~uid:"one" ())
  in
  let two_path = Eio.Path.(root_path / "two") in
  Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 two_path;
  let two_file = Eio.Path.(two_path / "two.ics") in
  Eio.Path.save ~create:(`Exclusive 0o600) two_file
    (timezone_calendar ~tzid:"Example/Shared" ~uid:"two" ());
  let identical =
    export_all_stored ~fs calendar_dir
    |> Result.get_ok |> Calendar_codec.Legacy.parse |> Result.get_ok
  in
  let identical_timezone_count =
    List.fold_left
      (fun count -> function `Timezone _ -> count + 1 | _ -> count)
      0 (snd identical)
  in
  Eio.Path.save ~create:(`Or_truncate 0o600) two_file
    (timezone_calendar ~offset:"+0100" ~tzid:"Example/Shared" ~uid:"two" ());
  let conflict = export_all_stored ~fs calendar_dir in
  Eio.Path.save ~create:(`Or_truncate 0o600) one_file
    (timezone_calendar ~reference_timezone:false ~tzid:"Example/Shared"
       ~uid:"one" ());
  Eio.Path.save ~create:(`Or_truncate 0o600) two_file
    (timezone_calendar ~offset:"+0100" ~reference_timezone:false
       ~tzid:"Example/Shared" ~uid:"two" ());
  let unused =
    export_all_stored ~fs calendar_dir
    |> Result.get_ok |> Calendar_codec.Legacy.parse |> Result.get_ok
  in
  let unused_timezone_count =
    List.fold_left
      (fun count -> function `Timezone _ -> count + 1 | _ -> count)
      0 (snd unused)
  in
  Printf.printf
    "identical TZID definitions: %d; conflict rejected: %b; unused omitted: %b\n"
    identical_timezone_count
    (match conflict with
    | Error (`Msg message) ->
        String.equal message
          "Conflicting VTIMEZONE definitions for TZID Example/Shared"
    | Ok _ -> false)
    (unused_timezone_count = 0);
  [%expect
    {|
    identical TZID definitions: 1; conflict rejected: true; unused omitted: true |}]

let%expect_test
    "calendar codec preserves VALARM multiplicity/order and canonical names" =
  let source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Caledonia codec tests//EN";
        "BEGIN:VTODO";
        "UID:codec-todo";
        "DTSTAMP:20260715T080000Z";
        "DTSTART:20260715T100000Z";
        "RELATED-TO;RELTYPE=PARENT:parent";
        "RESOURCES:Room One";
        "PERCENT-COMPLETE:40";
        "BEGIN:VALARM";
        "ACTION:DISPLAY";
        "TRIGGER:-PT1M";
        "DESCRIPTION:First";
        "X-FIRST:one";
        "X-SECOND:two";
        "END:VALARM";
        "BEGIN:VALARM";
        "ACTION:DISPLAY";
        "TRIGGER:-PT2M";
        "DESCRIPTION:Second";
        "END:VALARM";
        "BEGIN:VALARM";
        "ACTION:DISPLAY";
        "TRIGGER:-PT2M";
        "DESCRIPTION:Second";
        "END:VALARM";
        "END:VTODO";
        "END:VCALENDAR";
        "";
      ]
  in
  let calendar = Result.get_ok (Calendar_codec.Legacy.parse source) in
  let properties, alarms =
    List.find_map
      (function `Todo value -> Some value | _ -> None)
      (snd calendar)
    |> Option.get
  in
  let offsets =
    List.filter_map
      (fun alarm ->
        match Alarm.trigger alarm with
        | _, `Duration span -> Ptime.Span.to_int_s span
        | _, `Datetime _ -> None)
      alarms
  in
  let encoded = Calendar_codec.Legacy.to_ics ~cr:true calendar in
  let reparsed = Result.get_ok (Calendar_codec.Legacy.parse encoded) in
  let reparsed_alarms =
    List.find_map
      (function `Todo (_, alarms) -> Some alarms | _ -> None)
      (snd reparsed)
    |> Option.get
  in
  let lines = String.split_on_char '\n' encoded in
  let has_line name =
    List.exists
      (fun line ->
        String.starts_with ~prefix:(name ^ ":") line
        || String.starts_with ~prefix:(name ^ ";") line)
      lines
  in
  Printf.printf "alarms/offsets: %d/%s\n" (List.length alarms)
    (String.concat "," (List.map string_of_int offsets));
  Printf.printf "percent read: %s\n"
    (List.find_map
       (function
         | `Iana_prop ("PERCENT-COMPLETE", _, value) -> Some value | _ -> None)
       properties
    |> Option.value ~default:"missing");
  Printf.printf "canonical names: %b/%b/%b legacy absent: %b\n"
    (has_line "PERCENT-COMPLETE")
    (has_line "RELATED-TO") (has_line "RESOURCES")
    (not (has_line "PERCENT" || has_line "RELATED" || has_line "RESOURCE"));
  Printf.printf "private marker absent: %b\n"
    (not
       (List.exists
          (fun line -> String.contains line 'X' && String.contains line ':')
          (List.filter
             (String.starts_with ~prefix:"X-CALEDONIA-INTERNAL-ALARM-MARKER")
             lines)));
  Printf.printf "parse/write/parse alarms stable: %b\n"
    (alarms = reparsed_alarms);
  [%expect
    {|
    alarms/offsets: 3/-60,-120,-120
    percent read: 40
    canonical names: true/true/true legacy absent: true
    private marker absent: true
    parse/write/parse alarms stable: true |}]

let%expect_test
    "ordered codec documents keep opaque positions and hide compatibility \
     markers" =
  let top_opaque_one =
    String.concat "\r\n"
      [
        "BEGIN:VAVAILABILITY";
        "UID:opaque-one";
        "X-OPAQUE:one";
        "END:VAVAILABILITY";
      ]
  in
  let nested_opaque =
    String.concat "\r\n"
      [ "BEGIN:VUNKNOWN"; "X-NESTED:between-alarms"; "END:VUNKNOWN" ]
  in
  let top_opaque_two =
    String.concat "\r\n"
      [ "BEGIN:VPOLL"; "UID:opaque-two"; "X-OPAQUE:two"; "END:VPOLL" ]
  in
  let source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Caledonia ordered codec tests//EN";
        "BEGIN:VEVENT";
        "UID:event-before";
        "DTSTAMP:20260715T080000Z";
        "DTSTART;VALUE=DATE:20260715";
        "RRULE:FREQ=DAILY;UNTIL=20260716";
        "END:VEVENT";
        top_opaque_one;
        "BEGIN:VTODO";
        "UID:todo-middle";
        "DTSTAMP:20260715T080000Z";
        "SUMMARY:Before edit";
        "BEGIN:VALARM";
        "ACTION:DISPLAY";
        "TRIGGER:-PT1M";
        "DESCRIPTION:First alarm";
        "END:VALARM";
        nested_opaque;
        "BEGIN:VALARM";
        "ACTION:DISPLAY";
        "TRIGGER:-PT2M";
        "DESCRIPTION:Second alarm";
        "END:VALARM";
        "END:VTODO";
        top_opaque_two;
        "BEGIN:VJOURNAL";
        "UID:journal-after";
        "DTSTAMP:20260715T080000Z";
        "SUMMARY:After";
        "END:VJOURNAL";
        "END:VCALENDAR";
        "";
      ]
  in
  let document =
    match Calendar_codec.parse_document source with
    | Ok document -> document
    | Error error -> failwith error
  in
  let entry_name = function
    | Calendar_codec.Opaque opaque ->
        "opaque:" ^ Calendar_codec.opaque_name opaque
    | Calendar_codec.Known known -> (
        match Calendar_codec.component known with
        | `Event _ -> "known:VEVENT"
        | `Todo _ -> "known:VTODO"
        | `Journal _ -> "known:VJOURNAL"
        | `Freebusy _ -> "known:VFREEBUSY"
        | `Timezone _ -> "known:VTIMEZONE")
  in
  let entry_names =
    Calendar_codec.entries document |> List.map entry_name |> String.concat ","
  in
  let known_entries =
    Calendar_codec.entries document
    |> List.filter_map (function
      | Calendar_codec.Known known -> Some known
      | Calendar_codec.Opaque _ -> None)
  in
  let marker_free_payloads =
    List.for_all
      (fun known ->
        Icalendar.to_ics ~cr:true
          ( Calendar_codec.properties document,
            [ Calendar_codec.component known ] )
        |> contains_substring ~needle:"X-CALEDONIA"
        |> not)
      known_entries
  in
  let event_known = List.hd known_entries in
  let event =
    match Calendar_codec.component event_known with
    | `Event event -> event
    | _ -> assert false
  in
  let override =
    {
      event with
      rrule = None;
      props =
        `Recur_id (Icalendar.Params.empty, `Date (2026, 7, 15)) :: event.props;
    }
  in
  let override_known =
    Result.get_ok (Calendar_codec.make_known (`Event override))
  in
  let recreated_event_known =
    Result.get_ok
      (Calendar_codec.make_known
         ~rrule_date_untils:[ Some (2026, 7, 16) ]
         (`Event event))
  in
  let todo_known = List.nth known_entries 1 in
  let edited_todo =
    match Calendar_codec.component todo_known with
    | `Todo (properties, alarms) ->
        let properties =
          List.map
            (function
              | `Summary (params, _) -> `Summary (params, "After edit")
              | property -> property)
            properties
        in
        `Todo (properties, alarms)
    | _ -> assert false
  in
  let edited_todo_known =
    Result.get_ok (Calendar_codec.make_known edited_todo)
  in
  let no_alarm_todo_known =
    match edited_todo with
    | `Todo (properties, _) ->
        Result.get_ok (Calendar_codec.make_known (`Todo (properties, [])))
    | _ -> assert false
  in
  let rewritten =
    Calendar_codec.rewrite_known document ~f:(fun known ->
        match Calendar_codec.component known with
        | `Event _ ->
            Calendar_codec.Replace [ recreated_event_known; override_known ]
        | `Todo _ -> Calendar_codec.Replace [ edited_todo_known ]
        | `Journal _ | `Freebusy _ | `Timezone _ -> Calendar_codec.Keep)
    |> Result.get_ok
  in
  let encoded = Calendar_codec.serialize ~cr:true rewritten in
  let position_in text needle =
    let needle_length = String.length needle in
    let rec search offset =
      if offset + needle_length > String.length text then None
      else if String.sub text offset needle_length = needle then Some offset
      else search (offset + 1)
    in
    search 0
  in
  let position = position_in encoded in
  let ordered needles =
    let rec increasing previous = function
      | [] -> true
      | needle :: rest -> (
          match position needle with
          | Some current when current > previous -> increasing current rest
          | Some _ | None -> false)
    in
    increasing (-1) needles
  in
  let date_metadata =
    Calendar_codec.rrule_date_untils event_known = [ Some (2026, 7, 16) ]
  in
  let without_alarms =
    Calendar_codec.rewrite_known document ~f:(fun known ->
        match Calendar_codec.component known with
        | `Todo _ -> Calendar_codec.Replace [ no_alarm_todo_known ]
        | `Event _ | `Journal _ | `Freebusy _ | `Timezone _ ->
            Calendar_codec.Keep)
    |> Result.get_ok
    |> Calendar_codec.serialize ~cr:true
  in
  let nested_clamped =
    match
      ( position_in without_alarms "BEGIN:VUNKNOWN",
        position_in without_alarms "END:VTODO" )
    with
    | Some nested, Some ending ->
        nested < ending
        && not (contains_substring ~needle:"BEGIN:VALARM" without_alarms)
    | _ -> false
  in
  let opaque_only_source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Caledonia opaque-only codec tests//EN";
        top_opaque_one;
        "END:VCALENDAR";
        "";
      ]
  in
  let opaque_only =
    Calendar_codec.parse_document opaque_only_source
    |> Result.get_ok
    |> Calendar_codec.serialize ~cr:true
  in
  let opaque_only_round_trip =
    contains_substring ~needle:top_opaque_one opaque_only
    && (not
          (contains_substring ~needle:"INTERNAL-OPAQUE-PLACEHOLDER" opaque_only))
    && Result.is_ok (Calendar_codec.parse_document opaque_only)
  in
  Printf.printf "entries: %s\n" entry_names;
  Printf.printf "explicit/top opaque: %b/%b\n"
    (Calendar_codec.has_opaque_entries document)
    (contains_substring ~needle:top_opaque_one encoded
    && contains_substring ~needle:top_opaque_two encoded);
  Printf.printf "marker-free/date semantic: %b/%b/%b\n" marker_free_payloads
    date_metadata
    (contains_substring ~needle:"UNTIL=20260716" encoded
    && (not (contains_substring ~needle:"UNTIL=20260716T000000Z" encoded))
    && not (contains_substring ~needle:"X-CALEDONIA" encoded));
  Printf.printf "rewrite order/nested/edit: %b/%b/%b\n"
    (ordered
       [
         "RRULE:FREQ=DAILY";
         "RECURRENCE-ID";
         "BEGIN:VAVAILABILITY";
         "UID:todo-middle";
         "BEGIN:VPOLL";
         "UID:journal-after";
       ])
    (ordered
       [
         "DESCRIPTION:First alarm"; "BEGIN:VUNKNOWN"; "DESCRIPTION:Second alarm";
       ])
    (contains_substring ~needle:"SUMMARY:After edit" encoded);
  Printf.printf "removed-child anchor clamps before END: %b\n" nested_clamped;
  Printf.printf "opaque-only has no placeholder: %b\n" opaque_only_round_trip;
  let abstract_round_trip =
    match Calendar_codec.parse_document encoded with
    | Ok _ -> true
    | Error message ->
        Printf.printf "abstract round-trip error: %s\n" message;
        false
  in
  Printf.printf "abstract round trip: %b\n" abstract_round_trip;
  [%expect
    {|
    entries: known:VEVENT,opaque:VAVAILABILITY,known:VTODO,opaque:VPOLL,known:VJOURNAL
    explicit/top opaque: true/true
    marker-free/date semantic: true/true/true
    rewrite order/nested/edit: true/true/true
    removed-child anchor clamps before END: true
    opaque-only has no placeholder: true
    abstract round trip: true |}]

let%expect_test
    "codec alarm identity helpers preserve schema v2 canonical bytes" =
  let source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Caledonia alarm identity codec tests//EN";
        "BEGIN:VEVENT";
        "UID:override";
        "DTSTAMP:20260715T080000Z";
        "DTSTART;TZID=Europe/London:20260716T100000";
        "RECURRENCE-ID;TZID=Europe/London:20260716T090000";
        "BEGIN:VALARM";
        "ACTION:EMAIL";
        "TRIGGER;RELATED=START:-PT5M";
        "DESCRIPTION:Canonical alarm";
        "SUMMARY:Canonical summary";
        "ATTENDEE;CN=Someone:mailto:someone@example.test";
        "X-ONE:first";
        "X-TWO:second";
        "END:VALARM";
        "END:VEVENT";
        "END:VCALENDAR";
        "";
      ]
  in
  let properties, components =
    Result.get_ok (Calendar_codec.Legacy.parse source)
  in
  let event =
    List.find_map (function `Event event -> Some event | _ -> None) components
    |> Option.get
  in
  let recurrence_id =
    List.find_map
      (function `Recur_id value -> Some value | _ -> None)
      event.props
    |> Option.get
  in
  let old_source = Icalendar.to_ics ~cr:true (properties, [ `Event event ]) in
  let unfolded =
    old_source |> String.split_on_char '\n'
    |> List.fold_left
         (fun lines line ->
           let line =
             if String.ends_with ~suffix:"\r" line then
               String.sub line 0 (String.length line - 1)
             else line
           in
           match (lines, String.length line > 0) with
           | previous :: rest, true when line.[0] = ' ' || line.[0] = '\t' ->
               (previous ^ String.sub line 1 (String.length line - 1)) :: rest
           | _ -> line :: lines)
         []
    |> List.rev
  in
  let old_recurrence =
    List.find (String.starts_with ~prefix:"RECURRENCE-ID") unfolded
  in
  let rec alarm_block inside accumulated = function
    | [] -> assert false
    | "BEGIN:VALARM" :: rest -> alarm_block true [ "BEGIN:VALARM" ] rest
    | "END:VALARM" :: _ when inside ->
        String.concat "\r\n" (List.rev ("END:VALARM" :: accumulated))
    | line :: rest when inside -> alarm_block true (line :: accumulated) rest
    | _ :: rest -> alarm_block false accumulated rest
  in
  let old_alarm = alarm_block false [] unfolded in
  let alarm = List.hd event.alarms in
  Printf.printf "recurrence/alarm parity: %b/%b\n"
    (String.equal old_recurrence
       (Calendar_codec.canonical_recurrence_id_line recurrence_id))
    (String.equal old_alarm (Calendar_codec.canonical_alarm_block alarm));
  [%expect {| recurrence/alarm parity: true/true |}]

let%expect_test "canonical DATE recurrence identity makes VALUE=DATE explicit" =
  let recurrence_id = (Icalendar.Params.empty, `Date (2026, 7, 16)) in
  print_endline (Calendar_codec.canonical_recurrence_id_line recurrence_id);
  [%expect {| RECURRENCE-ID;VALUE=DATE:20260716 |}]

let%expect_test
    "calendar codec accepts mixed-case RFC syntax without folding payloads" =
  let source =
    String.concat "\r\n"
      [
        "begin:vcalendar";
        "version:2.0";
        "prodid:-//Caledonia mixed case tests//EN";
        "calscale:gregorian";
        "method:publish";
        "begin:vevent";
        "uid:mixed-case-event";
        "dtstamp:20260715T080000Z";
        "dtstart;value=date-time;tzid=Europe/London:20260715T100000";
        "summary:MiXeD ACTION:display TZID=Case/Sensitive";
        "class:private";
        "status:confirmed";
        "transp:opaque";
        "rrule:freq=weekly;count=2;byday=mo,we;wkst=su";
        "attendee;cn=\"MiXeD \
         Person\";cutype=individual;role=req-participant;partstat=accepted;rsvp=true:mailto:CaseSensitive@example.test";
        "x-payload;x-custom=DoNotFold:lower ACTION:display";
        "begin:valarm";
        "action:display";
        "trigger;related=start:-PT5M";
        "description:MiXeD alarm text";
        "end:valarm";
        "end:vevent";
        "end:vcalendar";
        "";
      ]
  in
  let properties, components =
    Result.get_ok (Calendar_codec.Legacy.parse source)
  in
  let event =
    List.find_map (function `Event event -> Some event | _ -> None) components
    |> Option.get
  in
  let summary =
    List.find_map
      (function `Summary (_, value) -> Some value | _ -> None)
      event.props
    |> Option.get
  in
  let status_and_class =
    ( List.exists
        (function `Status (_, `Confirmed) -> true | _ -> false)
        event.props,
      List.exists
        (function `Class (_, `Private) -> true | _ -> false)
        event.props )
  in
  let timezone =
    match snd event.dtstart with
    | `Datetime (`With_tzid (_, (_, tzid))) -> tzid
    | `Date _ | `Datetime (`Local _ | `Utc _) -> "wrong-kind"
  in
  let attendee_semantics =
    List.find_map
      (function
        | `Attendee (params, address) ->
            Some
              ( Icalendar.Params.find Icalendar.Cn params,
                Icalendar.Params.find Icalendar.Cutype params,
                Icalendar.Params.find Icalendar.Role params,
                Icalendar.Params.find Icalendar.Partstat params,
                Icalendar.Params.find Icalendar.Rsvp params,
                Uri.to_string address )
        | _ -> None)
      event.props
    |> Option.get
  in
  let x_payload =
    List.find_map
      (function
        | `Xprop ((_, "PAYLOAD"), params, value) ->
            Some
              ( (match
                   Icalendar.Params.find
                     (Icalendar.Xparam ("", "CUSTOM"))
                     params
                 with
                | Some _ as found -> found
                | None ->
                    Icalendar.Params.find (Icalendar.Iana_param "X-CUSTOM")
                      params),
                value )
        | _ -> None)
      event.props
    |> Option.get
  in
  let alarm_is_display_start =
    match event.alarms with
    | [ `Display alarm ] ->
        Icalendar.Params.find Icalendar.Related (fst alarm.trigger)
        = Some `Start
    | _ -> false
  in
  let calendar_tokens =
    ( List.exists
        (function `Calscale (_, "GREGORIAN") -> true | _ -> false)
        properties,
      List.exists
        (function `Method (_, "PUBLISH") -> true | _ -> false)
        properties )
  in
  let encoded =
    Calendar_codec.Legacy.to_ics ~cr:true (properties, components)
  in
  let reparses = Result.is_ok (Calendar_codec.Legacy.parse encoded) in
  Printf.printf "calendar/event tokens: %b/%b %b/%b\n" (fst calendar_tokens)
    (snd calendar_tokens) (fst status_and_class) (snd status_and_class);
  Printf.printf "TZID/text preserved: %b/%b\n"
    (String.equal timezone "Europe/London")
    (String.equal summary "MiXeD ACTION:display TZID=Case/Sensitive");
  Printf.printf "attendee enums/payload: %b/%b\n"
    (attendee_semantics
    = ( Some (`Quoted "MiXeD Person"),
        Some `Individual,
        Some `Reqparticipant,
        Some `Accepted,
        Some true,
        "mailto:CaseSensitive@example.test" ))
    (x_payload = (Some [ `String "DoNotFold" ], "lower ACTION:display"));
  Printf.printf "rrule/alarm/reparse: %b/%b/%b\n"
    (Option.is_some event.rrule)
    alarm_is_display_start reparses;
  Printf.printf "serialized payloads preserved: %b/%b/%b\n"
    (contains_substring
       ~needle:"SUMMARY:MiXeD ACTION:display TZID=Case/Sensitive" encoded)
    (contains_substring ~needle:"TZID=Europe/London" encoded)
    (contains_substring ~needle:"X-PAYLOAD;X-CUSTOM=DoNotFold" encoded);
  [%expect
    {|
    calendar/event tokens: true/true true/true
    TZID/text preserved: true/true
    attendee enums/payload: true/true
    rrule/alarm/reparse: true/true/true
    serialized payloads preserved: true/true/true |}]

let%expect_test
    "unknown components survive a supported-component mutation byte-for-byte" =
  with_calendar_dir @@ fun fs root root_path ->
  let unknown_block =
    String.concat "\r\n"
      [
        "BEGIN:VAVAILABILITY";
        "UID:availability-1";
        "DTSTAMP:20260715T080000Z";
        "X-LONG-UNKNOWN-PROPERTY:abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz";
        "BEGIN:AVAILABLE";
        "UID:available-1";
        "DTSTART:20260720T090000Z";
        "DTEND:20260720T170000Z";
        "END:AVAILABLE";
        "END:VAVAILABILITY";
      ]
  in
  let source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Caledonia unknown component tests//EN";
        "BEGIN:VEVENT";
        "UID:event-before";
        "DTSTAMP:20260715T080000Z";
        "DTSTART:20260716T090000Z";
        "SUMMARY:Before";
        "END:VEVENT";
        unknown_block;
        "BEGIN:VTODO";
        "UID:todo-after";
        "DTSTAMP:20260715T080000Z";
        "SUMMARY:After";
        "END:VTODO";
        "END:VCALENDAR";
        "";
      ]
  in
  let calendar_dir, _, file =
    prepare_file fs root root_path "work" "unknown.ics" source
  in
  let components =
    Result.get_ok (Calendar_dir.get_components ~fs calendar_dir)
  in
  let todo =
    List.find
      (fun component -> Component.get_id component = "todo-after")
      components
    |> Component.to_todo |> Option.get
  in
  let edited = Result.get_ok (Todo.edit ~summary:(Patch.Set "Edited") todo) in
  let _ =
    replace_body_and_reload ~fs calendar_dir components
      (Component.todo_body edited)
  in
  let written = Eio.Path.load file in
  let reparses = Result.is_ok (Calendar_codec.Legacy.parse written) in
  let refreshed =
    Result.get_ok (Calendar_dir.get_components ~fs calendar_dir)
  in
  Printf.printf "raw block preserved: %b\n"
    (contains_substring ~needle:unknown_block written);
  Printf.printf "supported siblings/reparse: %d/%b\n" (List.length refreshed)
    reparses;
  Printf.printf "private raw marker absent: %b\n"
    (not (contains_substring ~needle:"X-CALEDONIA-RAW-COMPONENT" written));
  let event =
    List.find
      (fun component -> Component.get_id component = "event-before")
      refreshed
  in
  let after_event_delete = remove_and_reload ~fs calendar_dir event in
  let todo =
    List.find
      (fun component -> Component.get_id component = "todo-after")
      after_event_delete
  in
  let after_supported_delete = remove_and_reload ~fs calendar_dir todo in
  let opaque_only = Eio.Path.load file in
  Printf.printf "opaque-only file preserved/loadable: %b/%b/%b\n"
    (contains_substring ~needle:unknown_block opaque_only)
    (Result.is_ok (Calendar_codec.Legacy.parse opaque_only))
    (after_supported_delete = []
    && Result.get_ok (Calendar_dir.get_components ~fs calendar_dir) = []);
  [%expect
    {|
    raw block preserved: true
    supported siblings/reparse: 2/true
    private raw marker absent: true
    opaque-only file preserved/loadable: true/true/true |}]

let%expect_test "no-op edits do not touch storage metadata or backups" =
  with_calendar_dir @@ fun fs root root_path ->
  let calendar_dir, calendar_path, file =
    prepare_file fs root root_path "work" "mixed.ics" mixed_calendar
  in
  let native_file = Eio.Path.native_exn file in
  Unix.utimes native_file 1.0 1.0;
  let components =
    Result.get_ok (Calendar_dir.get_components ~fs calendar_dir)
  in
  let component =
    List.find
      (fun component -> Component.get_id component = "todo-1")
      components
  in
  let todo = Component.to_todo component |> Option.get in
  let original_bytes = Eio.Path.load file in
  let original_mtime = (Unix.stat native_file).st_mtime in
  let original_fingerprint = Component.get_source_fingerprint component in
  let backup_count () =
    Eio.Path.read_dir calendar_path
    |> List.filter (String.starts_with ~prefix:".caledonia-backup-")
    |> List.length
  in
  let backups_before = backup_count () in
  let unchanged = Result.get_ok (Todo.edit todo) in
  let refreshed =
    replace_body_and_reload ~fs calendar_dir components
      (Component.todo_body unchanged)
  in
  let refreshed_component =
    List.find (fun item -> Component.get_id item = "todo-1") refreshed
  in
  Printf.printf "bytes/mtime unchanged: %b/%b\n"
    (String.equal original_bytes (Eio.Path.load file))
    (Float.equal original_mtime (Unix.stat native_file).st_mtime);
  Printf.printf "fingerprint/backups unchanged: %b/%b\n"
    (Component.get_source_fingerprint refreshed_component = original_fingerprint)
    (backup_count () = backups_before);
  [%expect
    {|
    bytes/mtime unchanged: true/true
    fingerprint/backups unchanged: true/true |}]

let todo_document ?parent uid summary =
  String.concat "\r\n"
    ([
       "BEGIN:VCALENDAR";
       "VERSION:2.0";
       "PRODID:-//Caledonia graph scope tests//EN";
       "BEGIN:VTODO";
       "UID:" ^ uid;
       "DTSTAMP:20260715T080000Z";
       "SUMMARY:" ^ summary;
     ]
    @ Option.fold ~none:[]
        ~some:(fun parent -> [ "RELATED-TO;RELTYPE=PARENT:" ^ parent ])
        parent
    @ [ "END:VTODO"; "END:VCALENDAR"; "" ])

let%expect_test "todo identities and parent graphs are scoped per calendar" =
  with_calendar_dir @@ fun fs root root_path ->
  let calendar_dir, _, _ =
    prepare_file fs root root_path "one" "one.ics"
      (todo_document "shared-uid" "One")
  in
  let second = Eio.Path.(root_path / "two") in
  Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 second;
  Eio.Path.save ~create:(`Exclusive 0o600)
    Eio.Path.(second / "two.ics")
    (todo_document "shared-uid" "Two");
  let components =
    Result.get_ok (Calendar_dir.get_components ~fs calendar_dir)
  in
  let one_component =
    List.find
      (fun component -> Component.get_calendar_key component = "one")
      components
  in
  let one = Component.to_todo one_component |> Option.get in
  let edited = Result.get_ok (Todo.edit ~summary:(Patch.Set "Edited") one) in
  Printf.printf "same UID in distinct calendars allowed: %b\n"
    (Result.is_ok
       (Calendar_dir.replace_stored_component ~fs calendar_dir
          ~original:one_component
          ~replacement:(Component.todo_body edited)));
  [%expect {| same UID in distinct calendars allowed: true |}]

let%expect_test "todo parents cannot resolve across calendar boundaries" =
  with_calendar_dir @@ fun fs root root_path ->
  let calendar_dir, _, _ =
    prepare_file fs root root_path "one" "child.ics"
      (todo_document ~parent:"parent" "child" "Child")
  in
  let second = Eio.Path.(root_path / "two") in
  Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 second;
  Eio.Path.save ~create:(`Exclusive 0o600)
    Eio.Path.(second / "parent.ics")
    (todo_document "parent" "Parent");
  let components =
    Result.get_ok (Calendar_dir.get_components ~fs calendar_dir)
  in
  let child_component =
    List.find (fun component -> Component.get_id component = "child") components
  in
  let child = Component.to_todo child_component |> Option.get in
  let edited = Result.get_ok (Todo.edit ~summary:(Patch.Set "Edited") child) in
  Printf.printf "cross-calendar parent rejected: %b\n"
    (Result.is_error
       (Calendar_dir.replace_stored_component ~fs calendar_dir
          ~original:child_component
          ~replacement:(Component.todo_body edited)));
  [%expect {| cross-calendar parent rejected: true |}]

let%expect_test "todo and journal loaded-domain validation is fail-closed" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let file = Eio.Path.(fs / Filename.get_temp_dir_name () / "validation.ics") in
  let fixture_source =
    Component_source.of_decoded_document ~calendar_key:"work"
      ~display_name:"work" ~file ~fingerprint:"test-fixture" ()
  in
  let validate source =
    match Calendar_codec.Legacy.parse source with
    | Error _ -> false
    | Ok calendar ->
        Component.stored_views_of_decoded_components ~source:fixture_source
          (snd calendar)
        |> Result.is_ok
  in
  let document component =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Caledonia validation tests//EN";
        component;
        "END:VCALENDAR";
        "";
      ]
  in
  let todo lines =
    document
      (String.concat "\r\n"
         ([ "BEGIN:VTODO"; "UID:invalid-todo"; "DTSTAMP:20260715T080000Z" ]
         @ lines @ [ "END:VTODO" ]))
  in
  let journal lines =
    document
      (String.concat "\r\n"
         ([
            "BEGIN:VJOURNAL"; "UID:invalid-journal"; "DTSTAMP:20260715T080000Z";
          ]
         @ lines @ [ "END:VJOURNAL" ]))
  in
  let rejected sources =
    List.for_all (fun source -> not (validate source)) sources
  in
  let raw_component kind lines =
    document
      (String.concat "\r\n" ([ "BEGIN:" ^ kind ] @ lines @ [ "END:" ^ kind ]))
  in
  Printf.printf "todo scalar/state/range rejected: %b\n"
    (rejected
       [
         todo [ "PRIORITY:42" ];
         todo [ "PERCENT-COMPLETE:101" ];
         todo [ "STATUS:CONFIRMED" ];
         todo [ "STATUS:IN-PROCESS"; "STATUS:NEEDS-ACTION" ];
         todo [ "PERCENT-COMPLETE:100"; "STATUS:IN-PROCESS" ];
         todo [ "DTSTART:20260715T100000Z"; "DUE:20260715T090000Z" ];
       ]);
  Printf.printf "todo alarm boundary rejected: %b\n"
    (not
       (validate
          (todo
             [
               "BEGIN:VALARM";
               "ACTION:DISPLAY";
               "TRIGGER;RELATED=END:-PT5M";
               "DESCRIPTION:Missing end";
               "END:VALARM";
             ])));
  Printf.printf "journal status/duplicates/priority rejected: %b\n"
    (rejected
       [
         journal [ "STATUS:CONFIRMED" ];
         journal [ "STATUS:DRAFT"; "STATUS:FINAL" ];
         journal [ "PRIORITY:1" ];
       ]);
  Printf.printf "event/todo/journal malformed identity rejected: %b/%b/%b\n"
    (rejected
       [
         raw_component "VEVENT"
           [ "UID:   "; "DTSTAMP:20260715T080000Z"; "DTSTART:20260715T090000Z" ];
       ])
    (rejected
       [
         raw_component "VTODO" [ "DTSTAMP:20260715T080000Z" ];
         raw_component "VTODO" [ "UID:   "; "DTSTAMP:20260715T080000Z" ];
         raw_component "VTODO"
           [ "UID:first"; "UID:second"; "DTSTAMP:20260715T080000Z" ];
         raw_component "VTODO" [ "UID:missing-stamp" ];
         raw_component "VTODO"
           [
             "UID:duplicate-stamp";
             "DTSTAMP:20260715T080000Z";
             "DTSTAMP:20260715T090000Z";
           ];
       ])
    (rejected
       [
         raw_component "VJOURNAL" [ "DTSTAMP:20260715T080000Z" ];
         raw_component "VJOURNAL" [ "UID:   "; "DTSTAMP:20260715T080000Z" ];
         raw_component "VJOURNAL"
           [ "UID:first"; "UID:second"; "DTSTAMP:20260715T080000Z" ];
         raw_component "VJOURNAL" [ "UID:missing-stamp" ];
         raw_component "VJOURNAL"
           [
             "UID:duplicate-stamp";
             "DTSTAMP:20260715T080000Z";
             "DTSTAMP:20260715T090000Z";
           ];
       ]);
  let event lines = raw_component "VEVENT" lines in
  let valid_event_prefix =
    [
      "UID:event-scalars";
      "DTSTAMP:20260715T080000Z";
      "DTSTART:20260715T090000Z";
    ]
  in
  Printf.printf "event required/dedicated scalars rejected lexically: %b\n"
    (rejected
       [
         event [ "DTSTAMP:20260715T080000Z"; "DTSTART:20260715T090000Z" ];
         event [ "UID:missing-stamp"; "DTSTART:20260715T090000Z" ];
         event [ "UID:missing-start"; "DTSTAMP:20260715T080000Z" ];
         event
           [
             "UID:first";
             "UID:second";
             "DTSTAMP:20260715T080000Z";
             "DTSTART:20260715T090000Z";
           ];
         event
           [
             "UID:duplicate-stamp";
             "DTSTAMP:20260715T080000Z";
             "DTSTAMP:20260715T081000Z";
             "DTSTART:20260715T090000Z";
           ];
         event
           [
             "UID:duplicate-start";
             "DTSTAMP:20260715T080000Z";
             "DTSTART:20260715T090000Z";
             "DTSTART:20260715T100000Z";
           ];
         event (valid_event_prefix @ [ "RRULE:FREQ=DAILY"; "RRULE:FREQ=WEEKLY" ]);
         event
           (valid_event_prefix
           @ [ "DTEND:20260715T100000Z"; "DTEND:20260715T110000Z" ]);
         event (valid_event_prefix @ [ "DURATION:PT1H"; "DURATION:PT2H" ]);
         event
           (valid_event_prefix @ [ "DTEND:20260715T100000Z"; "DURATION:PT1H" ]);
       ]);
  Printf.printf "event/todo/journal temporal parameters rejected: %b/%b/%b\n"
    (rejected
       [
         event
           [
             "UID:event-temporal-params";
             "DTSTAMP:20260715T080000Z";
             "DTSTART;LANGUAGE=en:20260715T090000Z";
           ];
         event
           (valid_event_prefix @ [ "DTEND;TZID=Europe/London:20260715T100000Z" ]);
         event (valid_event_prefix @ [ "DURATION;LANGUAGE=en:PT1H" ]);
       ])
    (rejected
       [
         todo [ "DTSTART;LANGUAGE=en:20260715T090000Z" ];
         todo [ "DUE;TZID=Europe/London:20260715T100000Z" ];
         todo [ "DTSTART:20260715T090000Z"; "DURATION;LANGUAGE=en:PT1H" ];
       ])
    (rejected [ journal [ "DTSTART;LANGUAGE=en:20260715T090000Z" ] ]);
  Printf.printf "temporal extension parameters accepted: %b/%b/%b\n"
    (validate
       (event
          [
            "UID:event-extension";
            "DTSTAMP:20260715T080000Z";
            "DTSTART;X-TEST-TRACE=kept:20260715T090000Z";
          ]))
    (validate (todo [ "DUE;X-TEST-TRACE=kept:20260715T100000Z" ]))
    (validate (journal [ "DTSTART;X-TEST-TRACE=kept:20260715T090000Z" ]));
  Printf.printf "valid todo/journal accepted: %b/%b\n"
    (validate
       (todo
          [
            "DTSTART:20260715T090000Z";
            "DUE:20260715T100000Z";
            "STATUS:IN-PROCESS";
            "PERCENT-COMPLETE:50";
          ]))
    (validate (journal [ "STATUS:FINAL" ]));
  [%expect
    {|
    todo scalar/state/range rejected: true
    todo alarm boundary rejected: true
    journal status/duplicates/priority rejected: true
    event/todo/journal malformed identity rejected: true/true/true
    event required/dedicated scalars rejected lexically: true
    event/todo/journal temporal parameters rejected: true/true/true
    temporal extension parameters accepted: true/true/true
    valid todo/journal accepted: true/true |}]

let%expect_test "VCALENDAR envelope validation is strict" =
  let event =
    String.concat "\r\n"
      [
        "BEGIN:VEVENT";
        "UID:envelope-event";
        "DTSTAMP:20260715T080000Z";
        "DTSTART:20260715T090000Z";
        "END:VEVENT";
      ]
  in
  let document properties =
    String.concat "\r\n"
      ([ "BEGIN:VCALENDAR" ] @ properties @ [ event; "END:VCALENDAR"; "" ])
  in
  let rejected properties =
    Calendar_codec.Legacy.parse (document properties) |> Result.is_error
  in
  Printf.printf "version missing/duplicate rejected: %b/%b\n"
    (rejected [ "PRODID:-//Envelope test//EN" ])
    (rejected [ "VERSION:2.0"; "VERSION:2.0"; "PRODID:-//Envelope test//EN" ]);
  Printf.printf "prodid missing/blank/duplicate rejected: %b/%b/%b\n"
    (rejected [ "VERSION:2.0" ])
    (rejected [ "VERSION:2.0"; "PRODID:   " ])
    (rejected
       [
         "VERSION:2.0";
         "PRODID:-//Envelope one//EN";
         "PRODID:-//Envelope two//EN";
       ]);
  Printf.printf "calscale/method duplicates rejected: %b/%b\n"
    (rejected
       [
         "VERSION:2.0";
         "PRODID:-//Envelope test//EN";
         "CALSCALE:GREGORIAN";
         "CALSCALE:GREGORIAN";
       ])
    (rejected
       [
         "VERSION:2.0";
         "PRODID:-//Envelope test//EN";
         "METHOD:PUBLISH";
         "METHOD:REQUEST";
       ]);
  Printf.printf "valid envelope accepted: %b\n"
    (Calendar_codec.Legacy.parse
       (document
          [
            "VERSION:2.0";
            "PRODID:-//Envelope test//EN";
            "CALSCALE:GREGORIAN";
            "METHOD:PUBLISH";
          ])
    |> Result.is_ok);
  [%expect
    {|
    version missing/duplicate rejected: true/true
    prodid missing/blank/duplicate rejected: true/true/true
    calscale/method duplicates rejected: true/true
    valid envelope accepted: true |}]

let%expect_test "VEVENT recurrence values must match DTSTART temporal kind" =
  Eio_main.run @@ fun _env ->
  let validate lines =
    let source =
      String.concat "\r\n"
        ([
           "BEGIN:VCALENDAR";
           "VERSION:2.0";
           "PRODID:-//Caledonia recurrence kind tests//EN";
           "BEGIN:VEVENT";
           "UID:recurrence-kind";
           "DTSTAMP:20260715T080000Z";
         ]
        @ lines
        @ [ "END:VEVENT"; "END:VCALENDAR"; "" ])
    in
    match Calendar_codec.Legacy.parse source with
    | Error _ -> false
    | Ok calendar -> event_series_of_calendar_result calendar |> Result.is_ok
  in
  let rejected cases = List.for_all (Fun.negate validate) cases in
  Printf.printf "EXDATE date/UTC/TZID mismatches rejected: %b\n"
    (rejected
       [
         [
           "DTSTART;VALUE=DATE:20260715";
           "RRULE:FREQ=DAILY;COUNT=2";
           "EXDATE:20260716T090000Z";
         ];
         [
           "DTSTART:20260715T090000Z";
           "RRULE:FREQ=DAILY;COUNT=2";
           "EXDATE:20260716T090000";
         ];
         [
           "DTSTART;TZID=Europe/London:20260715T090000";
           "RRULE:FREQ=DAILY;COUNT=2";
           "EXDATE;TZID=Europe/Paris:20260716T090000";
         ];
       ]);
  Printf.printf "RDATE date/UTC/TZID mismatches rejected: %b\n"
    (rejected
       [
         [
           "DTSTART;VALUE=DATE:20260715";
           "RRULE:FREQ=DAILY;COUNT=2";
           "RDATE:20260716T090000Z";
         ];
         [
           "DTSTART:20260715T090000Z";
           "RRULE:FREQ=DAILY;COUNT=2";
           "RDATE:20260716T090000";
         ];
         [
           "DTSTART;TZID=Europe/London:20260715T090000";
           "RRULE:FREQ=DAILY;COUNT=2";
           "RDATE;TZID=Europe/Paris:20260716T090000";
         ];
       ]);
  Printf.printf "RRULE UNTIL date/floating/TZID mismatches rejected: %b\n"
    (rejected
       [
         [
           "DTSTART;VALUE=DATE:20260715";
           "RRULE:FREQ=DAILY;UNTIL=20260716T000000Z";
         ];
         [
           "DTSTART:20260715T090000"; "RRULE:FREQ=DAILY;UNTIL=20260716T090000Z";
         ];
         [
           "DTSTART;TZID=Europe/London:20260715T090000";
           "RRULE:FREQ=DAILY;UNTIL=20260716T090000";
         ];
       ]);
  Printf.printf "matching DATE/floating/TZID recurrence values accepted: %b\n"
    (List.for_all validate
       [
         [
           "DTSTART;VALUE=DATE:20260715";
           "RRULE:FREQ=DAILY;UNTIL=20260716";
           "EXDATE;VALUE=DATE:20260716";
           "RDATE;VALUE=DATE:20260717";
         ];
         [
           "DTSTART:20260715T090000";
           "RRULE:FREQ=DAILY;UNTIL=20260716T090000";
           "EXDATE:20260716T090000";
         ];
         [
           "DTSTART;TZID=Europe/London:20260715T090000";
           "RRULE:FREQ=DAILY;UNTIL=20260716T080000Z";
           "RDATE;TZID=Europe/London:20260717T090000";
         ];
       ]);
  [%expect
    {|
    EXDATE date/UTC/TZID mismatches rejected: true
    RDATE date/UTC/TZID mismatches rejected: true
    RRULE UNTIL date/floating/TZID mismatches rejected: true
    matching DATE/floating/TZID recurrence values accepted: true |}]

let%expect_test
    "typed RRULE validation rejects unrepresentable recurrence states" =
  Eio_main.run @@ fun _env ->
  let start = ptime (2026, 7, 15) (9, 0, 0) in
  let make ?recurrence ?recurrence_params () =
    Event.create ~summary:"recurrence validation"
      ~start:(Icalendar.Params.empty, `Datetime (`Utc start))
      ?recurrence ?recurrence_params ()
  in
  let fractional_second = Option.get (Ptime.Span.of_d_ps (0, 1L)) in
  let fractional_until = Option.get (Ptime.add_span start fractional_second) in
  let invalid_rules =
    [
      (`Daily, Some (`Count 0), None, []);
      (`Daily, None, Some 0, []);
      ( `Weekly,
        None,
        None,
        [ `Byday [ (0, `Monday) ]; `Byday [ (0, `Tuesday) ] ] );
      (`Daily, None, None, [ `Bysecond [] ]);
      (`Daily, None, None, [ `Bysecond [ 61 ] ]);
      (`Daily, None, None, [ `Byminute [ 60 ] ]);
      (`Daily, None, None, [ `Byhour [ 24 ] ]);
      (`Monthly, None, None, [ `Byday [ (54, `Monday) ] ]);
      (`Monthly, None, None, [ `Bymonthday [ 0 ] ]);
      (`Monthly, None, None, [ `Bymonthday [ 32 ] ]);
      (`Yearly, None, None, [ `Byyearday [ 0 ] ]);
      (`Yearly, None, None, [ `Byyearday [ 367 ] ]);
      (`Yearly, None, None, [ `Byweek [ 0 ] ]);
      (`Yearly, None, None, [ `Byweek [ 54 ] ]);
      (`Daily, None, None, [ `Bymonth [ 0 ] ]);
      (`Daily, None, None, [ `Bymonth [ 13 ] ]);
      (`Daily, None, None, [ `Bysetposday [ 0 ] ]);
      (`Daily, None, None, [ `Bysetposday [ 367 ]; `Byday [ (0, `Monday) ] ]);
      (`Daily, Some (`Until (`Utc fractional_until)), None, []);
      (`Weekly, None, None, [ `Byweek [ 1 ] ]);
      (`Daily, None, None, [ `Byyearday [ 1 ] ]);
      (`Weekly, None, None, [ `Bymonthday [ 1 ] ]);
      (`Weekly, None, None, [ `Byday [ (1, `Monday) ] ]);
      (`Yearly, None, None, [ `Byweek [ 1 ]; `Byday [ (1, `Monday) ] ]);
      (`Daily, None, None, [ `Bysetposday [ 1 ] ]);
      (`Daily, None, None, [ `Bysetposday []; `Byday [ (0, `Monday) ] ]);
    ]
  in
  let invalid_messages_are_typed =
    List.for_all
      (fun recurrence ->
        match make ~recurrence () with
        | Error (`Msg message) -> String.trim message <> ""
        | Ok _ -> false)
      invalid_rules
  in
  let unrelated_parameter =
    Icalendar.Params.empty |> Icalendar.Params.add Icalendar.Language "en"
  in
  let shadow_parameter =
    Icalendar.Params.empty
    |> Icalendar.Params.add (Icalendar.Iana_param "VALUE") [ `String "RECUR" ]
  in
  let forged_date_marker =
    Icalendar.Params.empty
    |> Icalendar.Params.add (Icalendar.Iana_param "X-CALEDONIA-DATE-UNTIL")
         [ `String "20260716-forged" ]
  in
  let invalid_parameter_results =
    [
      make ~recurrence:(`Daily, None, None, [])
        ~recurrence_params:unrelated_parameter ();
      make ~recurrence:(`Daily, None, None, [])
        ~recurrence_params:shadow_parameter ();
      make ~recurrence:(`Daily, None, None, [])
        ~recurrence_params:forged_date_marker ();
    ]
  in
  let base = Result.get_ok (make ()) in
  let invalid_edit =
    Event.edit_patch
      ~recurrence:(Patch.Set (`Daily, Some (`Count 0), None, []))
      base
  in
  let invalid_date_subday_rule =
    let date_params =
      Icalendar.Params.empty |> Icalendar.Params.add Icalendar.Valuetype `Date
    in
    Event.create ~summary:"invalid DATE recurrence"
      ~start:(date_params, `Date (2026, 7, 15))
      ~recurrence:(`Daily, None, None, [ `Byhour [ 9 ] ])
      ()
  in
  let valid_boundaries =
    [
      ( `Monthly,
        None,
        None,
        [
          `Bysecond [ 0; 60 ];
          `Byminute [ 0; 59 ];
          `Byhour [ 0; 23 ];
          `Byday [ (-53, `Monday); (0, `Tuesday); (53, `Wednesday) ];
          `Bymonthday [ -31; 31 ];
          `Bymonth [ 1; 12 ];
          `Bysetposday [ -366; 366 ];
        ] );
      ( `Yearly,
        None,
        Some 1,
        [
          `Byweek [ -53; 53 ]; `Byyearday [ -366; 366 ]; `Byday [ (0, `Monday) ];
        ] );
    ]
  in
  Printf.printf "typed invalid rules produce Msg errors: %b\n"
    invalid_messages_are_typed;
  Printf.printf "registered RRULE parameters rejected: %b\n"
    (List.for_all Result.is_error invalid_parameter_results);
  Printf.printf "invalid recurrence edit rejected: %b\n"
    (Result.is_error invalid_edit);
  Printf.printf "DATE with sub-day BY part rejected: %b\n"
    (Result.is_error invalid_date_subday_rule);
  Printf.printf "RFC numeric boundaries accepted: %b\n"
    (List.for_all
       (fun recurrence -> Result.is_ok (make ~recurrence ()))
       valid_boundaries);
  [%expect
    {|
    typed invalid rules produce Msg errors: true
    registered RRULE parameters rejected: true
    invalid recurrence edit rejected: true
    DATE with sub-day BY part rejected: true
    RFC numeric boundaries accepted: true |}]

let%expect_test "loaded recurrence properties enforce RFC parameter domains" =
  Eio_main.run @@ fun _env ->
  let document components =
    String.concat "\r\n"
      ([
         "BEGIN:VCALENDAR";
         "VERSION:2.0";
         "PRODID:-//Caledonia recurrence domain tests//EN";
       ]
      @ components @ [ "END:VCALENDAR"; "" ])
  in
  let event ?(uid = "recurrence-domain") lines =
    [
      "BEGIN:VEVENT";
      "UID:" ^ uid;
      "DTSTAMP:20260715T080000Z";
      "DTSTART:20260715T090000Z";
    ]
    @ lines @ [ "END:VEVENT" ]
  in
  let validate components =
    match Calendar_codec.Legacy.parse (document components) with
    | Error _ -> false
    | Ok calendar -> event_series_of_calendar_result calendar |> Result.is_ok
  in
  let invalid_single_event_cases =
    [
      [ "RRULE:FREQ=DAILY;COUNT=0" ];
      [ "RRULE:FREQ=DAILY;INTERVAL=0" ];
      [ "RRULE:FREQ=WEEKLY;BYDAY=MO;BYDAY=TU" ];
      [ "RRULE:FREQ=DAILY;BYSECOND=" ];
      [ "RRULE:FREQ=DAILY;BYSECOND=61" ];
      [ "RRULE;VALUE=RECUR:FREQ=DAILY;COUNT=2" ];
      [ "RRULE;LANGUAGE=en:FREQ=DAILY;COUNT=2" ];
      [ "RRULE:FREQ=DAILY;COUNT=2"; "EXDATE;LANGUAGE=en:20260716T090000Z" ];
      [ "RRULE:FREQ=DAILY;COUNT=2"; "RDATE;CN=alias:20260716T090000Z" ];
    ]
  in
  let master = event [ "RRULE:FREQ=DAILY;COUNT=2" ] in
  let invalid_override =
    event
      [
        "RECURRENCE-ID;RELATED=START:20260716T090000Z";
        "SUMMARY:invalid override";
      ]
  in
  let valid_override =
    event
      [
        "RECURRENCE-ID;X-TEST-TRACE=kept:20260716T090000Z";
        "SUMMARY:valid override";
      ]
  in
  let valid_extension_and_boundaries =
    event
      [
        "RRULE;EXPERIMENT=kept;X-TEST-TRACE=kept:FREQ=YEARLY;BYWEEKNO=-53,53;BYYEARDAY=-366,366;BYDAY=MO;BYSETPOS=-366,366";
        "EXDATE;X-TEST-TRACE=kept:20270715T090000Z";
        "RDATE;EXPERIMENT=kept:20280715T090000Z";
      ]
  in
  Printf.printf "invalid raw RRULE/value cases rejected: %b\n"
    (List.for_all
       (fun lines -> not (validate (event lines)))
       invalid_single_event_cases);
  Printf.printf "illegal RECURRENCE-ID parameter rejected: %b\n"
    (not (validate (master @ invalid_override)));
  Printf.printf "unknown IANA/X parameters preserved as extensions: %b\n"
    (validate valid_extension_and_boundaries
    && validate (master @ valid_override));
  [%expect
    {|
    invalid raw RRULE/value cases rejected: true
    illegal RECURRENCE-ID parameter rejected: true
    unknown IANA/X parameters preserved as extensions: true |}]

let%expect_test "malformed registered properties cannot hide as IANA extensions"
    =
  Eio_main.run @@ fun _env ->
  let document component =
    String.concat "\r\n"
      ([
         "BEGIN:VCALENDAR";
         "VERSION:2.0";
         "PRODID:-//Caledonia parser demotion tests//EN";
       ]
      @ component @ [ "END:VCALENDAR"; "" ])
  in
  let event lines =
    [
      "BEGIN:VEVENT";
      "UID:demotion-event";
      "DTSTAMP:20260715T080000Z";
      "DTSTART:20260715T090000Z";
    ]
    @ lines @ [ "END:VEVENT" ]
  in
  let todo lines =
    [ "BEGIN:VTODO"; "UID:demotion-todo"; "DTSTAMP:20260715T080000Z" ]
    @ lines @ [ "END:VTODO" ]
  in
  let journal lines =
    [ "BEGIN:VJOURNAL"; "UID:demotion-journal"; "DTSTAMP:20260715T080000Z" ]
    @ lines @ [ "END:VJOURNAL" ]
  in
  let validate component =
    match Calendar_codec.Legacy.parse (document component) with
    | Error _ -> false
    | Ok calendar -> (
        match snd calendar with
        | [ `Event _ ] ->
            event_series_of_calendar_result calendar |> Result.is_ok
        | [ `Todo body ] -> Todo.of_ical_body body |> Result.is_ok
        | [ `Journal body ] -> Journal.of_ical_body body |> Result.is_ok
        | _ -> false)
  in
  let invalid_statuses =
    [
      event [ "STATUS:BOGUS" ];
      todo [ "STATUS:BOGUS" ];
      journal [ "STATUS:BOGUS" ];
    ]
  in
  let malformed_todo_dates =
    [
      todo [ "DUE:not-a-date" ];
      todo [ "STATUS:COMPLETED"; "COMPLETED:20260715T090000" ];
      todo [ "DTSTART:20260715T090000Z"; "DURATION:not-a-duration" ];
    ]
  in
  let malformed_recurring_components =
    [
      todo [ "RRULE:FREQ=DAILY;BYSECOND=61" ];
      journal [ "RRULE:FREQ=DAILY;BYSECOND=61" ];
    ]
  in
  let valid_unknown_extensions =
    [
      event [ "EXPERIMENTAL-PROP:event" ];
      todo [ "EXPERIMENTAL-PROP:todo" ];
      journal [ "EXPERIMENTAL-PROP:journal" ];
    ]
  in
  let valid_pinned_aliases =
    [
      event [ "RELATED:parent"; "RESOURCE:one,two" ];
      todo
        [
          "STATUS:IN-PROCESS";
          "PERCENT-COMPLETE:50";
          "RELATED:parent";
          "RESOURCE:one,two";
        ];
      journal [ "RELATED:parent" ];
    ]
  in
  Printf.printf "invalid STATUS demotions rejected for all components: %b\n"
    (List.for_all (fun component -> not (validate component)) invalid_statuses);
  Printf.printf "malformed VTODO temporal fields rejected: %b\n"
    (List.for_all
       (fun component -> not (validate component))
       malformed_todo_dates);
  Printf.printf "VTODO/VJOURNAL RRULE fallback rejected: %b\n"
    (List.for_all
       (fun component -> not (validate component))
       malformed_recurring_components);
  Printf.printf "unregistered IANA extensions retained: %b\n"
    (List.for_all validate valid_unknown_extensions);
  Printf.printf "documented pinned aliases retained: %b\n"
    (List.for_all validate valid_pinned_aliases);
  [%expect
    {|
    invalid STATUS demotions rejected for all components: true
    malformed VTODO temporal fields rejected: true
    VTODO/VJOURNAL RRULE fallback rejected: true
    unregistered IANA extensions retained: true
    documented pinned aliases retained: true |}]

let%expect_test "opaque surrogate names cannot consume legitimate X-properties"
    =
  let raw = "BEGIN:VAVAILABILITY\r\nEND:VAVAILABILITY" in
  let digits = "0123456789abcdef" in
  let encoded =
    String.to_seq raw
    |> Seq.map (fun character ->
        let value = Char.code character in
        String.init 2 (function
          | 0 -> digits.[value lsr 4]
          | _ -> digits.[value land 0x0f]))
    |> List.of_seq |> String.concat ""
  in
  let marker = "X-CALEDONIA-RAW-COMPONENT:" ^ encoded in
  let source =
    String.concat "\r\n"
      [
        "BEGIN:VCALENDAR";
        "VERSION:2.0";
        "PRODID:-//Caledonia marker collision test//EN";
        marker;
        "BEGIN:VEVENT";
        "UID:marker-collision";
        "DTSTAMP:20260715T080000Z";
        "DTSTART:20260715T090000Z";
        "END:VEVENT";
        "END:VCALENDAR";
        "";
      ]
  in
  let encoded_calendar =
    Calendar_codec.Legacy.parse source
    |> Result.get_ok
    |> Calendar_codec.Legacy.to_ics ~cr:true
  in
  let reparsed_properties =
    Calendar_codec.Legacy.parse encoded_calendar |> Result.get_ok |> fst
  in
  let property_preserved =
    List.exists
      (function
        | `Xprop ((vendor, name), _, value) ->
            value = encoded
            && ((vendor = "CALEDONIA" && name = "RAW-COMPONENT")
               || (vendor = "" && name = "CALEDONIA-RAW-COMPONENT"))
        | _ -> false)
      reparsed_properties
  in
  Printf.printf "legitimate marker-shaped property preserved: %b\n"
    (property_preserved && not (contains_substring ~needle:raw encoded_calendar));
  [%expect {| legitimate marker-shaped property preserved: true |}]
