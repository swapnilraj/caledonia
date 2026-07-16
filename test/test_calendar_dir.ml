open Caledonia_lib

let calendar_dir_path = Filename.concat (Sys.getcwd ()) "calendar"

let%expect_test "list calendar names" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let calendar_names =
    Result.get_ok @@ Calendar_dir.list_calendar_names ~fs calendar_dir
  in
  Printf.printf "Number of calendars: %d\n" (List.length calendar_names);
  Printf.printf "Contains 'example': %b\n"
    (List.exists (fun c -> c = "example") calendar_names);
  Printf.printf "Contains 'recurrence': %b\n"
    (List.exists (fun c -> c = "recurrence") calendar_names);
  [%expect
    {|
    Number of calendars: 3
    Contains 'example': true
    Contains 'recurrence': true |}]

let%expect_test "get calendar events" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let result =
    Calendar_dir.get_calendar_components ~fs calendar_dir "example"
    |> Result.map (List.filter_map Component.to_event)
  in
  Printf.printf "Found 'example' calendar: %b\n" (Result.is_ok result);
  [%expect {| Found 'example' calendar: true |}]

let%expect_test "get all events" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let calendar_dir =
    Result.get_ok @@ Calendar_dir.create ~fs calendar_dir_path
  in
  let events =
    Result.get_ok @@ Calendar_dir.get_components ~fs calendar_dir
    |> List.filter_map Component.to_event
  in
  Printf.printf "Total events: %d\n" (List.length events);
  [%expect {| Total events: 35 |}]

let%expect_test "calendar metadata is presentation, not identity" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let root = Filename.temp_file "caledonia-calendar-metadata-" "" in
  Sys.remove root;
  let root_path = Eio.Path.(fs / root) in
  Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 root_path;
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root_path)
    (fun () ->
      let calendar_dir = Result.get_ok (Calendar_dir.create ~fs root) in
      Result.get_ok (Calendar_dir.create_calendar ~fs calendar_dir "work");
      Printf.printf "fallback display name: %s\n"
        (Calendar_dir.get_display_name ~fs calendar_dir "work");
      Printf.printf "missing color: %b\n"
        (Calendar_dir.get_color ~fs calendar_dir "work" = None);
      let calendar_path = Eio.Path.(root_path / "work") in
      Eio.Path.save ~create:(`Exclusive 0o600)
        Eio.Path.(calendar_path / "displayname")
        "Work Calendar\n";
      Eio.Path.save ~create:(`Exclusive 0o600)
        Eio.Path.(calendar_path / "color")
        "#ff0000\n";
      Printf.printf "configured display name: %s\n"
        (Calendar_dir.get_display_name ~fs calendar_dir "work");
      Printf.printf "configured color: %s\n"
        (Calendar_dir.get_color ~fs calendar_dir "work"
        |> Option.value ~default:"missing");
      Printf.printf "directory key resolves: %b\n"
        (Calendar_dir.resolve_calendar_key ~fs calendar_dir "work" = Ok "work");
      Printf.printf "display name rejected as identity: %b\n"
        (Calendar_dir.resolve_calendar_key ~fs calendar_dir "Work Calendar"
        = Error `Not_found));
  [%expect
    {|
    fallback display name: work
    missing color: true
    configured display name: Work Calendar
    configured color: #ff0000
    directory key resolves: true
    display name rejected as identity: true |}]

let%expect_test "strict recursive document loading is deterministic" =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let root = Filename.temp_file "caledonia-calendar-order-" "" in
  Sys.remove root;
  let root_path = Eio.Path.(fs / root) in
  Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 root_path;
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root_path)
    (fun () ->
      let calendar_dir = Result.get_ok (Calendar_dir.create ~fs root) in
      Result.get_ok (Calendar_dir.create_calendar ~fs calendar_dir "work");
      let work = Eio.Path.(root_path / "work") in
      let nested = Eio.Path.(work / "nested") in
      Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 nested;
      let save directory filename uid =
        Eio.Path.save ~create:(`Exclusive 0o600)
          Eio.Path.(directory / filename)
          (String.concat "\r\n"
             [
               "BEGIN:VCALENDAR";
               "VERSION:2.0";
               "PRODID:-//Caledonia ordering test//EN";
               "BEGIN:VEVENT";
               "UID:" ^ uid;
               "DTSTAMP:20260716T090000Z";
               "DTSTART:20260716T100000Z";
               "END:VEVENT";
               "END:VCALENDAR";
               "";
             ])
      in
      save work "z.ics" "z";
      save nested "m.ics" "m";
      save work "a.ics" "a";
      let files =
        Calendar_dir.get_calendar_documents ~fs calendar_dir "work"
        |> Result.get_ok
        |> List.map (fun document ->
            Calendar_document.source document
            |> Component_source.file |> snd |> Filename.basename)
      in
      print_endline (String.concat "," files));
  [%expect {| a.ics,m.ics,z.ics |}]
