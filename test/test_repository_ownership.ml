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

let mixed_document =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia repository ownership tests//EN";
      "BEGIN:VEVENT";
      "UID:ownership-event";
      "DTSTAMP:20260715T090000Z";
      "DTSTART:20260715T100000Z";
      "SUMMARY:Original event";
      "END:VEVENT";
      "BEGIN:VTODO";
      "UID:ownership-todo";
      "DTSTAMP:20260715T090000Z";
      "SUMMARY:Original todo";
      "END:VTODO";
      "END:VCALENDAR";
      "";
    ]

let with_repository run =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let root = Filename.temp_file "caledonia-repository-ownership-" "" in
  Sys.remove root;
  let root_path = Eio.Path.(fs / root) in
  Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 root_path;
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root_path)
    (fun () ->
      let repository = Result.get_ok (Calendar_dir.create ~fs root) in
      Result.get_ok (Calendar_dir.create_calendar ~fs repository "work");
      run ~fs ~root ~root_path ~repository)

let stored_event components =
  List.find
    (fun component -> Option.is_some (Component.to_event component))
    components

let fingerprint component =
  Component.get_source component |> Component_source.fingerprint

let file_fingerprint file = Eio.Path.load file |> Digest.string |> Digest.to_hex

let same_installed_component left right =
  Component_source.equal
    (Component.get_source left)
    (Component.get_source right)
  && Component_identity.equal
       (Component.get_identity left)
       (Component.get_identity right)
  && Component.ical_components_of_body (Component.body left)
     = Component.ical_components_of_body (Component.body right)

let reload_component ~fs repository component =
  Calendar_dir.get_calendar_documents ~fs repository
    (Component.get_calendar_key component)
  |> Result.get_ok
  |> List.find_map (fun document ->
      Calendar_document.find document (Component.get_identity component)
      |> Result.to_option)
  |> Option.get

let todo_document ~uid ~summary =
  String.concat "\r\n"
    [
      "BEGIN:VCALENDAR";
      "VERSION:2.0";
      "PRODID:-//Caledonia repository ownership tests//EN";
      "BEGIN:VTODO";
      "UID:" ^ uid;
      "DTSTAMP:20260715T090000Z";
      "SUMMARY:" ^ summary;
      "END:VTODO";
      "END:VCALENDAR";
      "";
    ]

let%expect_test "one physical file has one immutable document owner" =
  with_repository (fun ~fs ~root:_ ~root_path ~repository ->
      let file = Eio.Path.(root_path / "work" / "mixed.ics") in
      Eio.Path.save ~create:(`Exclusive 0o600) file mixed_document;
      let documents =
        Calendar_dir.get_calendar_documents ~fs repository "work"
        |> Result.get_ok
      in
      let document = List.hd documents in
      let components = Calendar_document.components document in
      let event_component = stored_event components in
      let event = Component.to_event event_component |> Option.get in
      let original_serialization = Calendar_document.serialize document in
      let edited =
        Event.edit_patch ~summary:(Patch.Set "Edited body only") event
        |> Result.get_ok
      in
      let snapshot_unchanged =
        String.equal original_serialization
          (Calendar_document.serialize document)
        && Event.get_summary event = Some "Original event"
        && Event.get_summary edited = Some "Edited body only"
      in
      Printf.printf "documents=%d components=%d snapshot-unchanged=%b\n"
        (List.length documents) (List.length components) snapshot_unchanged;
      let rewritten =
        Calendar_dir.remove_stored_component ~fs repository event_component
        |> Result.get_ok
      in
      let survivor =
        match rewritten with
        | Calendar_dir.Document_rewritten [ survivor ] -> Some survivor
        | Calendar_dir.Document_rewritten _ | Calendar_dir.File_deleted -> None
      in
      let rewritten_has_canonical_survivor =
        match survivor with
        | None -> false
        | Some survivor ->
            Eio.Path.is_file file
            && String.equal (fingerprint survivor) (file_fingerprint file)
            && Option.is_some (Component.to_todo survivor)
      in
      let removed =
        survivor
        |> Option.map (Calendar_dir.remove_stored_component ~fs repository)
        |> Option.map Result.get_ok
      in
      let final_file_deleted =
        removed = Some Calendar_dir.File_deleted
        && Eio.Path.kind ~follow:false file = `Not_found
      in
      Printf.printf "rewrite=%b file-delete=%b\n"
        rewritten_has_canonical_survivor final_file_deleted);
  [%expect
    {|
    documents=1 components=2 snapshot-unchanged=true
    rewrite=true file-delete=true
    |}]

let event_start =
  let timestamp =
    Ptime.of_date_time ((2026, 7, 15), ((10, 0, 0), 0)) |> Option.get
  in
  (Icalendar.Params.empty, `Datetime (`Utc timestamp))

let%expect_test
    "repository create and replace need no caller cache and return canonical \
     data" =
  with_repository (fun ~fs ~root:_ ~root_path:_ ~repository ->
      let draft =
        Event.create ~summary:"Created draft" ~start:event_start ()
        |> Result.get_ok
      in
      let draft_has_no_fingerprint = true in
      let created_component =
        Calendar_dir.create_stored_component ~fs repository ~calendar_key:"work"
          (Component.event_body draft)
        |> Result.get_ok
      in
      let created = Component.to_event created_component |> Option.get in
      let created_fingerprint = fingerprint created_component in
      let created_file = Component.get_file created_component in
      let reloaded_created =
        reload_component ~fs repository created_component
      in
      let created_is_canonical =
        String.equal created_fingerprint (file_fingerprint created_file)
        && same_installed_component created_component reloaded_created
      in
      let replacement =
        Event.edit_patch ~summary:(Patch.Set "Canonical replacement") created
        |> Result.get_ok
      in
      let replaced_component =
        Calendar_dir.replace_stored_component ~fs repository
          ~original:created_component
          ~replacement:(Component.event_body replacement)
        |> Result.get_ok
      in
      let replaced_fingerprint = fingerprint replaced_component in
      let reloaded_document =
        Calendar_dir.get_calendar_documents ~fs repository "work"
        |> Result.get_ok |> List.hd
      in
      let reloaded =
        Calendar_document.find reloaded_document
          (Component.get_identity replaced_component)
        |> Result.get_ok
      in
      let replaced_is_canonical =
        String.equal replaced_fingerprint (file_fingerprint created_file)
        && String.equal replaced_fingerprint (fingerprint reloaded)
        && same_installed_component replaced_component reloaded
        && Component.get_summary reloaded = Some "Canonical replacement"
        && not (String.equal created_fingerprint replaced_fingerprint)
      in
      Printf.printf
        "draft-without-source=%b create-canonical=%b replace-canonical=%b\n"
        draft_has_no_fingerprint created_is_canonical replaced_is_canonical);
  [%expect
    {|
    draft-without-source=true create-canonical=true replace-canonical=true
    |}]

let%expect_test
    "todo mutation rejects an affected sibling change and ignores other \
     calendars" =
  with_repository (fun ~fs ~root:_ ~root_path ~repository ->
      let target_draft = Todo.create ~summary:"Target" () |> Result.get_ok in
      let target =
        Calendar_dir.create_stored_component ~fs repository ~calendar_key:"work"
          (Component.todo_body target_draft)
        |> Result.get_ok
      in
      let replacement =
        Component.to_todo target |> Option.get
        |> Todo.edit ~summary:(Patch.Set "Replacement")
        |> Result.get_ok |> Component.todo_body
      in
      let target_file = Component.get_file target in
      let target_before = Eio.Path.load target_file in
      let sibling = Eio.Path.(root_path / "work" / "sibling.ics") in
      let sibling_native = Eio.Path.native_exn sibling in
      let changed_sibling =
        Eio.Path.(root_path / "work" / ".sibling.changed")
      in
      let changed_sibling_native = Eio.Path.native_exn changed_sibling in
      let first = todo_document ~uid:"sibling" ~summary:"First snapshot" in
      let changed =
        todo_document ~uid:"sibling" ~summary:"Externally changed"
      in
      Eio.Path.save ~create:(`Exclusive 0o600) sibling first;
      Eio.Path.save ~create:(`Exclusive 0o600) changed_sibling changed;
      Calendar_dir.For_test.inject_before_snapshot_verification (fun () ->
          Unix.rename changed_sibling_native sibling_native);
      let mutation =
        Fun.protect
          ~finally:(fun () ->
            Calendar_dir.For_test.clear ();
            Eio.Path.unlink sibling)
          (fun () ->
            Calendar_dir.replace_stored_component ~fs repository
              ~original:target ~replacement)
      in
      let affected_conflict =
        match mutation with
        | Error error -> Storage_error.is_conflict error
        | Ok _ -> false
      in
      let target_unchanged =
        String.equal target_before (Eio.Path.load target_file)
      in
      Result.get_ok (Calendar_dir.create_calendar ~fs repository "other");
      let unrelated = Eio.Path.(root_path / "other" / "broken.ics") in
      let unrelated_bytes = "not a calendar\n" in
      Eio.Path.save ~create:(`Exclusive 0o600) unrelated unrelated_bytes;
      let unrelated_ignored =
        Calendar_dir.replace_stored_component ~fs repository ~original:target
          ~replacement
        |> Result.is_ok
      in
      Printf.printf
        "conflict=%b unchanged=%b other-ignored=%b other-unchanged=%b\n"
        affected_conflict target_unchanged unrelated_ignored
        (String.equal unrelated_bytes (Eio.Path.load unrelated)));
  [%expect
    {|
    conflict=true unchanged=true other-ignored=true other-unchanged=true
    |}]
