type expectation = { fixture : string; diagnostic_fragments : string list }

let failf format =
  Printf.ksprintf
    (fun message ->
      prerr_endline ("negative interface check: " ^ message);
      exit 1)
    format

let read_all channel =
  let buffer = Buffer.create 256 in
  (try
     while true do
       Buffer.add_channel buffer channel 4096
     done
   with End_of_file -> ());
  Buffer.contents buffer

let contains ~needle haystack =
  let needle_length = String.length needle in
  let rec search offset =
    offset + needle_length <= String.length haystack
    && (String.equal (String.sub haystack offset needle_length) needle
       || search (offset + 1))
  in
  search 0

let package_dependencies =
  String.concat ","
    [
      "eio";
      "fmt";
      "icalendar";
      "ptime";
      "ptime.clock.os";
      "re";
      "sexplib";
      "timedesc";
      "uri";
      "uuidm";
      "uucp";
      "uuseg";
      "uutf";
      "yojson";
    ]

let compile ~ocamlfind ~cmi_directory fixture =
  let arguments =
    [|
      ocamlfind;
      "ocamlc";
      "-package";
      package_dependencies;
      "-I";
      cmi_directory;
      "-c";
      fixture;
    |]
  in
  let stdout, stdin, stderr =
    Unix.open_process_args_full ocamlfind arguments (Unix.environment ())
  in
  close_out stdin;
  let stdout_text = read_all stdout in
  let stderr_text = read_all stderr in
  let status = Unix.close_process_full (stdout, stdin, stderr) in
  (status, stdout_text ^ stderr_text)

let check ~ocamlfind ~cmi_directory ~fixture_directory expectation =
  let fixture = Filename.concat fixture_directory expectation.fixture in
  let status, diagnostic = compile ~ocamlfind ~cmi_directory fixture in
  (match status with
  | Unix.WEXITED 0 ->
      failf "%s unexpectedly compiled; the forbidden API became public"
        expectation.fixture
  | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> ());
  List.iter
    (fun fragment ->
      if not (contains ~needle:fragment diagnostic) then
        failf "%s failed for the wrong reason; expected diagnostic %S, got:\n%s"
          expectation.fixture fragment diagnostic)
    expectation.diagnostic_fragments

let () =
  if Array.length Sys.argv <> 4 then
    failf "usage: check_negative_interfaces OCAMLFIND LIBRARY FIXTURE_DIR";
  let ocamlfind = Sys.argv.(1) in
  let archive = Sys.argv.(2) in
  let fixture_directory = Sys.argv.(3) in
  let cmi_directory =
    Filename.concat (Filename.dirname archive) ".caledonia_lib.objs/byte"
  in
  let expectations =
    [
      {
        fixture = "occurrence_replace.ml";
        diagnostic_fragments = [ "Event.Occurrence.t"; "Component.t" ];
      };
      {
        fixture = "occurrence_remove.ml";
        diagnostic_fragments = [ "Event.Occurrence.t"; "Component.t" ];
      };
      {
        fixture = "event_source.ml";
        diagnostic_fragments = [ "Unbound value"; "Event.get_source" ];
      };
      {
        fixture = "event_path.ml";
        diagnostic_fragments = [ "Unbound value"; "Event.get_file" ];
      };
      {
        fixture = "todo_source.ml";
        diagnostic_fragments = [ "Unbound value"; "Todo.get_source" ];
      };
      {
        fixture = "todo_path.ml";
        diagnostic_fragments = [ "Unbound value"; "Todo.get_file" ];
      };
      {
        fixture = "journal_source.ml";
        diagnostic_fragments = [ "Unbound value"; "Journal.get_source" ];
      };
      {
        fixture = "journal_path.ml";
        diagnostic_fragments = [ "Unbound value"; "Journal.get_file" ];
      };
    ]
  in
  List.iter (check ~ocamlfind ~cmi_directory ~fixture_directory) expectations;
  Printf.printf "negative interface check: %d forbidden programs rejected\n"
    (List.length expectations)
