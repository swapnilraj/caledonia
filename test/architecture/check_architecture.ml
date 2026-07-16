open Yojson.Safe.Util

let failf format =
  Printf.ksprintf
    (fun message ->
      prerr_endline ("architecture check: " ^ message);
      exit 1)
    format

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let contains ~needle haystack =
  try
    ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
    true
  with Not_found -> false

let has_suffix suffix value =
  let suffix_length = String.length suffix in
  let value_length = String.length value in
  value_length >= suffix_length
  && String.equal suffix
       (String.sub value (value_length - suffix_length) suffix_length)

let source_files root directory =
  let path = Filename.concat root directory in
  Sys.readdir path |> Array.to_list
  |> List.filter (fun name -> has_suffix ".ml" name || has_suffix ".mli" name)
  |> List.map (fun name -> Filename.concat directory name)

let sorted = List.sort String.compare

let expected_ids =
  let numbered prefix count =
    List.init count (fun index -> Printf.sprintf "%s-%d" prefix (index + 1))
  in
  numbered "DMO" 8 @ numbered "DMI" 10 @ numbered "DMR" 9 @ numbered "DMC" 8
  @ numbered "DMS" 8 @ numbered "DMA" 12 @ numbered "DMT" 8 @ numbered "DMW" 14

let string_list json = json |> to_list |> List.map to_string

let requirement_field requirement name =
  match requirement |> member name with
  | `Null -> failf "requirement is missing field %S" name
  | value -> value

let check_ledger root =
  let ledger_path = Filename.concat root "docs/data-model-requirements.json" in
  let prd_path = Filename.concat root "docs/data-model-simplification-prd.md" in
  let ledger =
    try Yojson.Safe.from_file ledger_path
    with Yojson.Json_error message ->
      failf "invalid requirement ledger: %s" message
  in
  let schema_version = ledger |> member "schema_version" |> to_int in
  if schema_version <> 1 then
    failf "unsupported requirement-ledger schema version %d" schema_version;
  let requirements = ledger |> member "requirements" |> to_list in
  let expected_count = List.length expected_ids in
  if List.length requirements <> expected_count then
    failf "requirement ledger has %d entries; expected %d"
      (List.length requirements) expected_count;
  let seen = Hashtbl.create expected_count in
  let counts = Hashtbl.create 4 in
  List.iter
    (fun status -> Hashtbl.add counts status 0)
    [ "not_started"; "in_progress"; "blocked"; "complete" ];
  List.iter
    (fun requirement ->
      let id = requirement_field requirement "id" |> to_string in
      if Hashtbl.mem seen id then failf "duplicate requirement ID %s" id;
      Hashtbl.add seen id ();
      let title = requirement_field requirement "title" |> to_string in
      if String.trim title = "" then failf "%s has an empty title" id;
      let priority = requirement_field requirement "priority" |> to_string in
      if not (List.mem priority [ "P0"; "P1"; "P2" ]) then
        failf "%s has unsupported priority %S" id priority;
      let phase = requirement_field requirement "phase" |> to_string in
      if String.trim phase = "" then failf "%s has an empty phase" id;
      let status = requirement_field requirement "status" |> to_string in
      if not (Hashtbl.mem counts status) then
        failf "%s has unsupported status %S" id status;
      Hashtbl.replace counts status (Hashtbl.find counts status + 1);
      let evidence = requirement_field requirement "evidence" |> string_list in
      let remaining =
        requirement_field requirement "remaining" |> string_list
      in
      List.iter
        (fun path ->
          if not (Sys.file_exists (Filename.concat root path)) then
            failf "%s cites missing evidence %S" id path)
        evidence;
      if status = "complete" && evidence = [] then
        failf "%s is complete without evidence" id;
      if status = "in_progress" && evidence = [] then
        failf "%s is in progress without partial evidence" id;
      if status <> "complete" && remaining = [] then
        failf "%s is incomplete without a remaining-work description" id)
    requirements;
  let actual_ids = Hashtbl.to_seq_keys seen |> List.of_seq |> sorted in
  if actual_ids <> sorted expected_ids then
    failf "requirement IDs differ from the expected %d-ID PRD inventory"
      expected_count;
  let summary = ledger |> member "summary" in
  let total = summary |> member "total" |> to_int in
  if total <> List.length requirements then
    failf "ledger summary total is %d but contains %d requirements" total
      (List.length requirements);
  List.iter
    (fun status ->
      let declared = summary |> member status |> to_int in
      let actual = Hashtbl.find counts status in
      if declared <> actual then
        failf "ledger summary says %d %s requirements but contains %d" declared
          status actual)
    [ "not_started"; "in_progress"; "blocked"; "complete" ];
  let prd = read_file prd_path in
  List.iter
    (fun id ->
      if not (contains ~needle:id prd) then
        failf "requirement %s is absent from the source PRD" id)
    expected_ids

let check_no_substrings root ~requirement file forbidden =
  let source = read_file (Filename.concat root file) in
  List.iter
    (fun needle ->
      if contains ~needle source then
        failf "%s violates %s by referencing %S" file requirement needle)
    forbidden

let check_foundational_boundaries root =
  let pure_modules =
    [
      "lib/component_kind.ml";
      "lib/component_kind.mli";
      "lib/component_identity.ml";
      "lib/component_identity.mli";
      "lib/patch.ml";
      "lib/patch.mli";
      "lib/storage_error.ml";
      "lib/storage_error.mli";
    ]
  in
  List.iter
    (fun file ->
      check_no_substrings root ~requirement:"DMT-6 foundational boundary" file
        [
          "Eio.";
          "Calendar_dir.";
          "Calendar_codec.";
          "Component_source.";
          "Format_utils.";
        ])
    pure_modules;
  List.iter
    (fun file ->
      check_no_substrings root ~requirement:"DMI-3/DMI-4 target boundary" file
        [ "Eio."; "Calendar_dir."; "Calendar_codec."; "Format_utils." ])
    [
      "lib/component_target.ml";
      "lib/component_target.mli";
      "lib/alarm_fire_id.ml";
      "lib/alarm_fire_id.mli";
    ]

let files_containing root files needle =
  files
  |> List.filter (fun file ->
      contains ~needle (read_file (Filename.concat root file)))
  |> sorted

let check_allowlist root ~requirement ~needle ~allowed files =
  let actual = files_containing root files needle in
  let unexpected =
    List.filter (fun file -> not (List.mem file allowed)) actual
  in
  match unexpected with
  | [] -> ()
  | _ ->
      failf "%s introduced %S outside its transitional allowlist: %s"
        requirement needle
        (String.concat ", " unexpected)

let domain_files =
  [
    "lib/event.ml";
    "lib/event.mli";
    "lib/todo.ml";
    "lib/todo.mli";
    "lib/journal.ml";
    "lib/journal.mli";
  ]

let check_domain_boundaries root =
  (* These are the actual domain compilation units. Keep transitional
     dependencies explicit here: each list can be reduced to [[]] as its
     presentation/codec migration lands, without changing the gate itself. *)
  List.iter
    (fun needle ->
      check_allowlist root ~requirement:"DMI-5/DMT-6 pure domain boundary"
        ~needle ~allowed:[] domain_files)
    [ "Eio."; "Calendar_dir."; "Component_source."; "Component_target." ];
  List.iter
    (fun (needle, allowed) ->
      check_allowlist root
        ~requirement:"DMA-3/DMC-3/DMT-6 transitional domain dependency" ~needle
        ~allowed domain_files)
    [ ("Calendar_codec.", []); ("Format_utils.", []); ("Sexplib", []) ]

let normalized_interface source =
  Str.global_replace (Str.regexp "[\t\r\n ]+") " " source

let public_declaration source name =
  let source = normalized_interface source in
  let marker = "val " ^ name ^ " :" in
  try
    let start = Str.search_forward (Str.regexp_string marker) source 0 in
    let after_marker = start + String.length marker in
    let finish =
      try Str.search_forward (Str.regexp_string " val ") source after_marker
      with Not_found -> String.length source
    in
    String.sub source start (finish - start)
  with Not_found -> failf "public interface is missing %s" marker

let require_declaration_fragment ~requirement ~name declaration fragment =
  if not (contains ~needle:fragment declaration) then
    failf "%s requires %s to contain %S" requirement name fragment

let check_model_interfaces root =
  let calendar_dir = read_file (Filename.concat root "lib/calendar_dir.mli") in
  let create = public_declaration calendar_dir "create_stored_component" in
  let replace = public_declaration calendar_dir "replace_stored_component" in
  let remove = public_declaration calendar_dir "remove_stored_component" in
  require_declaration_fragment ~requirement:"DMI-5/DMI-6/DMT-2"
    ~name:"create_stored_component" create "Component.body";
  require_declaration_fragment ~requirement:"DMI-6/DMI-7/DMT-2"
    ~name:"create_stored_component" create "Component.t";
  require_declaration_fragment ~requirement:"DMS-2/DMT-2"
    ~name:"replace_stored_component" replace "original:Component.t";
  require_declaration_fragment ~requirement:"DMS-2/DMT-2"
    ~name:"replace_stored_component" replace "replacement:Component.body";
  require_declaration_fragment ~requirement:"DMI-8/DMT-2"
    ~name:"remove_stored_component" remove "Component.t";
  List.iter
    (fun (name, declaration) ->
      if contains ~needle:"Event.Occurrence.t" declaration then
        failf
          "DMT-2 repository mutation %s accepts a derived Event.Occurrence.t"
          name)
    [
      ("create_stored_component", create);
      ("replace_stored_component", replace);
      ("remove_stored_component", remove);
    ];
  List.iter
    (fun file ->
      check_no_substrings root ~requirement:"DMO-3/DMI-5/DMT-2 draft boundary"
        file
        [
          "Component_source.";
          "Component_target.";
          "val get_source";
          "val get_file";
          "source_fingerprint";
          "val get_calendar_key";
          "val get_calendar_name";
        ])
    [ "lib/event.mli"; "lib/todo.mli"; "lib/journal.mli" ];
  let component = read_file (Filename.concat root "lib/component.mli") in
  if contains ~needle:"Icalendar.calendar" component then
    failf "DMO-1 complete calendar escaped the abstract Calendar_document owner";
  if contains ~needle:"val store :" (normalized_interface component) then
    failf
      "DMI-3/DMI-6 public Component.store can manufacture stored lifecycle \
       values outside document decoding";
  let source_interface =
    read_file (Filename.concat root "lib/component_source.mli")
  in
  if not (contains ~needle:"val of_decoded_document" source_interface) then
    failf "DMI-3 source construction lacks an explicit decoder-only seam";
  if contains ~needle:"val create :" source_interface then
    failf "DMI-3 broad Component_source.create decoder alias is public";
  let production_sources = source_files root "lib" @ source_files root "bin" in
  check_allowlist root ~requirement:"DMI-3 deprecated source decoder alias"
    ~needle:"Component_source.create" ~allowed:[] production_sources;
  check_allowlist root ~requirement:"DMI-3 named source decoder seam"
    ~needle:"Component_source.of_decoded_document"
    ~allowed:[ "lib/calendar_dir.ml" ] production_sources;
  check_allowlist root ~requirement:"DMI-3 stored-view decoder seam"
    ~needle:"Component.stored_views_of_decoded_components"
    ~allowed:[ "lib/calendar_document.ml" ]
    production_sources;
  List.iter
    (fun (file, forbidden) ->
      let interface = read_file (Filename.concat root file) in
      if contains ~needle:forbidden (normalized_interface interface) then
        failf "DMA-8 reintroduced exception-raising convenience API %s in %s"
          forbidden file)
    [
      ("lib/event.mli", "val compute_alarm_fires :");
      ("lib/todo.mli", "val compute_alarm_fires :");
      ("lib/alarm_query.mli", "val run :");
    ]

let check_serialization_ratcheting root =
  let production_sources = source_files root "lib" @ source_files root "bin" in
  check_allowlist root ~requirement:"DMC-1 raw upstream parsing"
    ~needle:"Icalendar.parse"
    ~allowed:[ "lib/calendar_codec.ml" ]
    production_sources;
  check_allowlist root ~requirement:"DMC-4 raw upstream serialization"
    ~needle:"Icalendar.to_ics"
    ~allowed:[ "lib/calendar_codec.ml" ]
    production_sources;
  check_allowlist root ~requirement:"DMC-4 component-level codec serialization"
    ~needle:"Calendar_codec.to_ics" ~allowed:[] production_sources;
  check_allowlist root ~requirement:"DMA-9 legacy codec quarantine"
    ~needle:"Calendar_codec.Legacy" ~allowed:[] production_sources;
  let codec = read_file (Filename.concat root "lib/calendar_codec.mli") in
  if not (contains ~needle:"[@@deprecated" codec) then
    failf "DMA-9 Calendar_codec.Legacy is not formally deprecated"

let check_shared_algebra root =
  let production_sources = source_files root "lib" @ source_files root "bin" in
  List.iter
    (fun legacy ->
      match files_containing root production_sources legacy with
      | [] -> ()
      | files ->
          failf "DMI-1 legacy kind constructor %s appears in %s" legacy
            (String.concat ", " files))
    [
      "CEvent";
      "CTodo";
      "CJournal";
      "type component_kind";
      "type component_type";
    ];
  let sexp = read_file (Filename.concat root "lib/sexp.ml") in
  let reexport = "type 'a patch = 'a Patch.t = Keep | Clear | Set of 'a" in
  if not (contains ~needle:reexport sexp) then
    failf "DMA-1 protocol patch is not a true Patch.t re-export";
  if contains ~needle:"let to_patch" sexp then
    failf "DMA-1 reintroduced a protocol-to-domain patch conversion"

let count_occurrences ~needle source =
  let expression = Str.regexp_string needle in
  let rec loop count offset =
    try
      let found = Str.search_forward expression source offset in
      loop (count + 1) (found + String.length needle)
    with Not_found -> count
  in
  loop 0 0

let check_legacy_domain_ratcheting root =
  List.iter
    (fun (file, maximum_optional_fingerprints) ->
      let source = read_file (Filename.concat root file) in
      let calendars = count_occurrences ~needle:"calendar : calendar" source in
      if calendars > 1 then
        failf "%s added component-local full-calendar ownership (DMO-2)" file;
      let optional_fingerprints =
        count_occurrences ~needle:"source_fingerprint : string option" source
      in
      if optional_fingerprints > maximum_optional_fingerprints then
        failf "%s added optional persisted-fingerprint state (DMO-4)" file)
    [ ("lib/event.ml", 0); ("lib/todo.ml", 0); ("lib/journal.ml", 0) ]

let check_second_simplicity_audit root =
  check_no_substrings root ~requirement:"DMO-8 derivable document indexes"
    "lib/calendar_document.ml"
    [ "known_identities"; "identity index is" ];
  check_no_substrings root ~requirement:"DMI-10 one component kind"
    "lib/property_validation.ml"
    [ "type component ="; "~component:`Event" ];
  let source = read_file (Filename.concat root "lib/component_source.mli") in
  if not (contains ~needle:"val display_name : t -> string" source) then
    failf "DMI-9 source display name is not total";
  List.iter
    (fun forbidden ->
      if contains ~needle:forbidden source then
        failf "DMI-9 source interface retains duplicate name API %S" forbidden)
    [ "val calendar_name"; "val display_name : t -> string option" ];
  let event = read_file (Filename.concat root "lib/event.mli") in
  List.iter
    (fun forbidden ->
      if contains ~needle:forbidden event then
        failf "DMR-9/DMA-12 event interface retains %S" forbidden)
    [ "module Series"; "type t = Series.t"; "val edit :"; "type event_id" ];
  let date = read_file (Filename.concat root "lib/date.mli") in
  List.iter
    (fun forbidden ->
      if contains ~needle:forbidden date then
        failf "DMA-10 date interface retains parallel temporal API %S" forbidden)
    [
      "type calendar_time";
      "calendar_time_of_ical";
      "ical_of_calendar_time";
      "instant_of_calendar_time";
    ];
  check_no_substrings root ~requirement:"DMA-11 one query sort algebra"
    "bin/query_args.ml"
    [ "type sort_spec ="; "| `Summary ->" ];
  let todo = read_file (Filename.concat root "lib/todo.mli") in
  List.iter
    (fun forbidden ->
      if contains ~needle:forbidden todo then
        failf "DMA-12 todo interface retains alias %S" forbidden)
    [
      "get_ancestors_checked"; "expand_with_ancestors_checked"; "mark_complete";
    ];
  let query = read_file (Filename.concat root "lib/component_query.mli") in
  if contains ~needle:"val stored_series" query then
    failf "DMA-12 query interface retains unused stored_series accessor";
  let patch = read_file (Filename.concat root "lib/patch.mli") in
  if contains ~needle:"val map" patch then
    failf "DMA-12 patch interface retains unused map helper"

let check_whole_diff_audit root =
  check_no_substrings root ~requirement:"DMW-2 derived stored identity"
    "lib/component.ml"
    [ "identity : Component_identity.t" ];
  check_no_substrings root ~requirement:"DMW-3 derived snapshots"
    "lib/calendar_dir.ml"
    [ "snapshot.components"; "with _ -> calendar_key"; "with _ -> None" ];
  let calendar_dir = read_file (Filename.concat root "lib/calendar_dir.ml") in
  if
    not
      (contains ~needle:"Eio.Path.read_dir dir_path |> List.sort String.compare"
         calendar_dir)
  then failf "DMW-3 strict recursive document traversal is not deterministic";
  List.iter
    (fun file ->
      check_no_substrings root ~requirement:"DMW-4 explicit domain clock" file
        [ "Ptime_clock.now"; "Uuidm.v4_gen" ])
    [ "lib/event.ml"; "lib/todo.ml"; "lib/journal.ml" ];
  List.iter
    (fun file ->
      check_no_substrings root ~requirement:"DMW-4 shared identifiers" file
        [ "Uuidm.v4_gen"; "Random.State.make_self_init" ])
    [ "lib/calendar_codec.ml"; "lib/calendar_dir.ml" ];
  let fresh_id = read_file (Filename.concat root "lib/fresh_id.ml") in
  if count_occurrences ~needle:"Uuidm.v4_gen" fresh_id <> 1 then
    failf "DMW-4 Fresh_id must own exactly one UUID generator";
  List.iter
    (fun file ->
      let interface = read_file (Filename.concat root file) in
      if not (contains ~needle:"now:Ptime.t" interface) then
        failf "DMW-4 %s does not require an explicit mutation clock" file)
    [ "lib/event.mli"; "lib/todo.mli"; "lib/journal.mli" ];
  check_no_substrings root ~requirement:"DMW-5 one alarm/query owner"
    "lib/alarm_query.mli"
    [ "type subject"; "val subject_" ];
  let query = read_file (Filename.concat root "lib/component_query.mli") in
  if not (contains ~needle:"val get_target : item -> Component_target.t" query)
  then failf "DMW-5 query items do not own their common target derivation";
  List.iter
    (fun file ->
      check_no_substrings root ~requirement:"DMW-6 presentation-free daemon"
        file
        [ "type notification"; "Format_utils"; "timezone :" ])
    [ "lib/alarm_daemon_core.ml"; "lib/alarm_daemon_core.mli" ];
  let daemon = read_file (Filename.concat root "lib/alarm_daemon_core.mli") in
  if not (contains ~needle:"notify : Alarm_query.fire" daemon) then
    failf "DMW-6 daemon notify boundary does not carry the domain fire";
  List.iter
    (fun file ->
      check_no_substrings root ~requirement:"DMW-7 one event response model"
        file [ "Events_with_timezone" ])
    [ "lib/sexp.ml"; "lib/sexp.mli"; "bin/server_cmd.ml" ];
  let sexp = read_file (Filename.concat root "lib/sexp.ml") in
  if
    not
      (contains
         ~needle:
           "stored_event_wire_sexp ~documents ~occurrence_timezone \
            stored_series"
         sexp)
  then failf "DMW-7 series_master does not retain its stored source target";
  check_no_substrings root ~requirement:"DMW-8 total alarm trigger"
    "lib/format_utils.mli" [ "val alarm_trigger :" ];
  check_no_substrings root ~requirement:"DMW-8 typed DATE overflow"
    "lib/todo.ml"
    [ "let due_end = Date.add_days" ];
  check_no_substrings root ~requirement:"DMW-8 result-only date calculations"
    "lib/date.mli"
    [
      "val today :";
      "val add_days :";
      "val add_weeks :";
      "val add_months :";
      "val add_years :";
      "val get_start_";
      "val get_end_";
      "val timedesc_to_ptime :";
    ];
  check_no_substrings root ~requirement:"DMA-12 minimal codec surface"
    "lib/calendar_codec.mli"
    [ "val create :"; "val opaque_raw :" ];
  let query_args = read_file (Filename.concat root "bin/query_args.ml") in
  if not (contains ~needle:"let resolve_temporal_scope" query_args) then
    failf "DMW-9 shared temporal-scope parser is missing";
  List.iter
    (fun file ->
      check_no_substrings root ~requirement:"DMW-9 shared temporal scope" file
        [ "Date.convert_relative_date_formats" ])
    [ "bin/list_cmd.ml"; "bin/search_cmd.ml"; "bin/alarms_cmd.ml" ];
  check_no_substrings root ~requirement:"DMW-10 shared status codec"
    "bin/output.ml"
    [ "| `Draft -> \"draft\"" ];
  let component_args =
    read_file (Filename.concat root "bin/component_args.ml")
  in
  if not (contains ~needle:"Component_status.bindings" component_args) then
    failf "DMW-10 component arguments do not use the shared status codec";
  List.iter
    (fun file ->
      check_no_substrings root ~requirement:"DMW-11 shared list patch" file
        [ "let replace_property" ])
    [ "lib/event.ml"; "lib/todo.ml"; "lib/journal.ml" ];
  let patch = read_file (Filename.concat root "lib/patch.mli") in
  if not (contains ~needle:"val replace_in_list" patch) then
    failf "DMW-11 shared property-list patch utility is missing";
  let common = read_file (Filename.concat root "bin/command_common.ml") in
  List.iter
    (fun required ->
      if not (contains ~needle:required common) then
        failf "DMW-12 command common utility is missing %S" required)
    [ "let storage_result"; "let map_result"; "let find_unique_component" ];
  if count_occurrences ~needle:"Unix.lockf fd Unix.F_TLOCK" calendar_dir <> 1
  then failf "DMW-12 advisory lock acquisition is duplicated";
  List.iter
    (fun file ->
      if not (Sys.file_exists (Filename.concat root file)) then
        failf "DMW-1/13/14 audit evidence is missing %s" file)
    [
      "docs/whole-diff-simplicity-prd.md"; "docs/whole-diff-data-flow-audit.md";
    ]

let () =
  if Array.length Sys.argv <> 2 then
    failf "usage: check_architecture WORKSPACE_ROOT";
  let root = Sys.argv.(1) in
  check_ledger root;
  check_foundational_boundaries root;
  check_domain_boundaries root;
  check_model_interfaces root;
  check_shared_algebra root;
  check_serialization_ratcheting root;
  check_legacy_domain_ratcheting root;
  check_second_simplicity_audit root;
  check_whole_diff_audit root;
  Printf.printf
    "architecture check: %d requirements and migration ratchets verified\n"
    (List.length expected_ids)
