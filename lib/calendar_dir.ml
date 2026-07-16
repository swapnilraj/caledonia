open Icalendar

type t = string
type deletion_outcome = File_deleted | Document_rewritten of Component.t list

let ( let* ) = Result.bind

let invalid_document result =
  Result.map_error
    (fun (`Msg message) -> Storage_error.Invalid_document message)
    result

let invalid_replacement result =
  Result.map_error
    (fun (`Msg message) -> Storage_error.Invalid_replacement message)
    result

module For_test = struct
  type failure =
    | Post_write_mismatch
    | Temporary_parse_failure
    | Rename_failure

  let next_failure : failure option ref = ref None
  let before_snapshot_verification : (unit -> unit) option ref = ref None
  let inject_next (failure : failure) = next_failure := Some failure

  let inject_before_snapshot_verification callback =
    before_snapshot_verification := Some callback

  let clear () =
    next_failure := None;
    before_snapshot_verification := None

  let consume (failure : failure) =
    if !next_failure = Some failure then (
      next_failure := None;
      true)
    else false

  let run_before_snapshot_verification () =
    match !before_snapshot_verification with
    | None -> ()
    | Some callback ->
        before_snapshot_verification := None;
        callback ()
end

module Calendar_key_map = Map.Make (String)

let fingerprint content = Digest.to_hex (Digest.string content)

let invalid_key key =
  key = "" || key = "." || key = ".."
  || (not (Filename.is_relative key))
  || String.contains key '/' || String.contains key '\\'
  || String.contains key '\000'

let validate_calendar_key key =
  if invalid_key key then
    Error (`Msg (Printf.sprintf "Invalid calendar directory key %S" key))
  else Ok ()

let get_calendar_path ~fs calendar_dir calendar_key =
  Eio.Path.(fs / calendar_dir / calendar_key)

let ensure_dir path =
  try
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 path;
    Ok ()
  with Eio.Exn.Io _ as exn ->
    Error
      (`Msg
         (Fmt.str "Failed to create directory %s: %a" (snd path) Eio.Exn.pp exn))

let create ~fs path =
  match ensure_dir Eio.Path.(fs / path) with
  | Ok () -> Ok path
  | Error e -> Error e

let of_path path = path

let create_calendar ~fs calendar_dir calendar_key =
  let* () = validate_calendar_key calendar_key in
  let root = Eio.Path.(fs / calendar_dir) in
  if not (Eio.Path.is_directory root) then
    Error (`Msg "The configured calendar root does not exist")
  else
    let path = Eio.Path.(root / calendar_key) in
    match Eio.Path.kind ~follow:false path with
    | `Not_found -> ensure_dir path
    | `Directory -> Ok ()
    | _ -> Error (`Msg "Calendar directory key does not name a directory")

let get_display_name ~fs calendar_dir calendar_key =
  let displayname_path =
    Eio.Path.(fs / calendar_dir / calendar_key / "displayname")
  in
  try
    let content = Eio.Path.load displayname_path |> String.trim in
    if content = "" then calendar_key else content
  with Eio.Exn.Io _ -> calendar_key

let get_color ~fs calendar_dir calendar_key =
  let color_path = Eio.Path.(fs / calendar_dir / calendar_key / "color") in
  try
    let content = Eio.Path.load color_path |> String.trim in
    if content = "" then None else Some content
  with Eio.Exn.Io _ -> None

let list_calendar_names ~fs calendar_dir =
  try
    let dir = Eio.Path.(fs / calendar_dir) in
    let calendar_names =
      Eio.Path.read_dir dir
      |> List.filter_map (fun file ->
          if
            String.length file > 0
            && file.[0] <> '.'
            && (not (invalid_key file))
            && Eio.Path.kind ~follow:false Eio.Path.(dir / file) = `Directory
          then Some file
          else None)
      |> List.sort String.compare
    in
    Ok calendar_names
  with Eio.Exn.Io _ as exn ->
    Error
      (`Msg
         (Fmt.str "Failed to list calendar directory %s: %a" calendar_dir
            Eio.Exn.pp exn))

let resolve_calendar_key ~fs calendar_dir name =
  let* keys = list_calendar_names ~fs calendar_dir in
  if List.mem name keys then Ok name else Error `Not_found

let rec load_documents_recursive calendar_key display_name dir_path =
  try
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | name :: rest ->
          let path = Eio.Path.(dir_path / name) in
          if String.length name > 0 && name.[0] = '.' then loop acc rest
          else if Eio.Path.kind ~follow:false path = `Directory then
            let* nested =
              load_documents_recursive calendar_key display_name path
            in
            loop (List.rev_append nested acc) rest
          else if Eio.Path.kind ~follow:false path = `Symbolic_link then
            Error
              (`Msg
                 (Printf.sprintf
                    "Refusing to follow symbolic link inside calendar root: %s"
                    (snd path)))
          else if Filename.check_suffix name ".ics" then
            let content = Eio.Path.load path in
            let source =
              Component_source.of_decoded_document ~calendar_key ~display_name
                ~file:path ~fingerprint:(fingerprint content) ()
            in
            let* document =
              match Calendar_document.parse ~source content with
              | Ok document -> Ok document
              | Error (`Msg error) ->
                  Error
                    (`Msg
                       (Printf.sprintf "Failed to decode %s: %s" (snd path)
                          error))
            in
            loop (document :: acc) rest
          else loop acc rest
    in
    loop [] (Eio.Path.read_dir dir_path |> List.sort String.compare)
  with Eio.Exn.Io _ as exn ->
    Error
      (`Msg
         (Fmt.str "Failed to read directory %s: %a" (snd dir_path) Eio.Exn.pp
            exn))

let get_calendar_documents ~fs calendar_dir calendar_key =
  let* () = validate_calendar_key calendar_key in
  let calendar_path = get_calendar_path ~fs calendar_dir calendar_key in
  if Eio.Path.kind ~follow:false calendar_path = `Not_found then
    Error `Not_found
  else if Eio.Path.kind ~follow:false calendar_path <> `Directory then
    Error (`Msg "Calendar directory key is not a confined directory")
  else
    let display_name = get_display_name ~fs calendar_dir calendar_key in
    load_documents_recursive calendar_key display_name calendar_path

let get_calendar_components ~fs calendar_dir calendar_key =
  let* documents = get_calendar_documents ~fs calendar_dir calendar_key in
  Ok (List.concat_map Calendar_document.components documents)

let get_documents ~fs calendar_dir =
  let* ids = list_calendar_names ~fs calendar_dir in
  let rec loop acc = function
    | [] -> Ok (List.concat (List.rev acc))
    | id :: rest -> (
        match get_calendar_documents ~fs calendar_dir id with
        | Ok documents -> loop (documents :: acc) rest
        | Error `Not_found -> loop acc rest
        | Error (`Msg _ as error) -> Error error)
  in
  loop [] ids

let get_components ~fs calendar_dir =
  let* documents = get_documents ~fs calendar_dir in
  Ok (List.concat_map Calendar_document.components documents)

let rec load_components_recursive_tolerant ~report calendar_key display_name
    dir_path =
  try
    Eio.Path.read_dir dir_path |> List.sort String.compare
    |> List.concat_map (fun name ->
        let path = Eio.Path.(dir_path / name) in
        try
          if String.length name > 0 && name.[0] = '.' then []
          else
            match Eio.Path.kind ~follow:false path with
            | `Directory ->
                load_components_recursive_tolerant ~report calendar_key
                  display_name path
            | `Symbolic_link ->
                report
                  (Printf.sprintf
                     "Alarm scan skipped symbolic link inside calendar root: %s"
                     (snd path));
                []
            | _ when Filename.check_suffix name ".ics" -> (
                let content = Eio.Path.load path in
                let source =
                  Component_source.of_decoded_document ~calendar_key
                    ~display_name ~file:path ~fingerprint:(fingerprint content)
                    ()
                in
                match Calendar_document.parse ~source content with
                | Error (`Msg error) ->
                    report
                      (Printf.sprintf "Alarm scan skipped malformed %s: %s"
                         (snd path) error);
                    []
                | Ok document -> Calendar_document.components document)
            | _ -> []
        with Eio.Exn.Io _ as exn ->
          report
            (Fmt.str "Alarm scan could not read %s: %a" (snd path) Eio.Exn.pp
               exn);
          [])
  with Eio.Exn.Io _ as exn ->
    report
      (Fmt.str "Alarm scan could not list %s: %a" (snd dir_path) Eio.Exn.pp exn);
    []

let get_components_tolerant ~report ~fs calendar_dir =
  let* calendar_keys = list_calendar_names ~fs calendar_dir in
  Ok
    (List.concat_map
       (fun calendar_key ->
         let display_name = get_display_name ~fs calendar_dir calendar_key in
         let path = get_calendar_path ~fs calendar_dir calendar_key in
         load_components_recursive_tolerant ~report calendar_key display_name
           path)
       calendar_keys)

let same_identity = Component_identity.equal

let path_is_within ~root path =
  let root = if Filename.check_suffix root "/" then root else root ^ "/" in
  String.length path >= String.length root
  && String.sub path 0 (String.length root) = root

let real_path path = Unix.realpath (Eio.Path.native_exn path)

let validate_file_path ~fs calendar_dir calendar_key file =
  let* () =
    validate_calendar_key calendar_key
    |> Result.map_error (fun (`Msg message) ->
        Storage_error.Path_violation message)
  in
  let* () =
    if
      String.split_on_char '/' (snd file)
      |> List.exists (fun segment -> segment = "." || segment = "..")
    then
      Error
        (Storage_error.Path_violation
           "Calendar component path contains a traversal segment")
    else Ok ()
  in
  let root = Eio.Path.(fs / calendar_dir) in
  let calendar_path = Eio.Path.(root / calendar_key) in
  let* parent =
    match Eio.Path.split file with
    | Some (parent, basename)
      when basename <> ""
           && Filename.check_suffix basename ".ics"
           && Filename.basename basename = basename
           && not (String.contains basename '\000') ->
        Ok parent
    | _ ->
        Error
          (Storage_error.Path_violation
             "Calendar component path is not a confined .ics file")
  in
  try
    let root_real = real_path root in
    let calendar_real = real_path calendar_path in
    let parent_real = real_path parent in
    let file_is_confined =
      match Eio.Path.kind ~follow:false file with
      | `Not_found -> true
      | _ ->
          let file_real = real_path file in
          path_is_within ~root:calendar_real file_real
    in
    if
      (String.equal calendar_real root_real
      || not (path_is_within ~root:root_real calendar_real))
      || (not (String.equal parent_real calendar_real))
         && not (path_is_within ~root:calendar_real parent_real)
      || not file_is_confined
    then
      Error
        (Storage_error.Path_violation
           "Calendar component path escapes the configured calendar root")
    else Ok ()
  with Unix.Unix_error (error, operation, path) ->
    Error
      (Storage_error.Path_violation
         (Printf.sprintf "Cannot validate calendar path %s (%s: %s)" path
            operation (Unix.error_message error)))

let random_suffix = Fresh_id.generate

let sibling_path file prefix =
  match Eio.Path.split file with
  | None -> invalid_arg "Calendar file must have a basename"
  | Some (dir, basename) ->
      Eio.Path.(dir / (prefix ^ basename ^ "-" ^ random_suffix ()))

let lock_path file =
  match Eio.Path.split file with
  | None -> invalid_arg "Calendar file must have a basename"
  | Some (dir, basename) ->
      Eio.Path.(dir / (".caledonia-lock-" ^ basename ^ ".lock"))

let io_error action file exn =
  Storage_error.Io (Fmt.str "%s %s: %a" action (snd file) Eio.Exn.pp exn)

let with_advisory_lock ~native ~conflict_message ~io_message fn =
  match
    try
      let fd = Unix.openfile native [ Unix.O_CREAT; Unix.O_RDWR ] 0o600 in
      try
        Unix.lockf fd Unix.F_TLOCK 0;
        Ok fd
      with exn ->
        Unix.close fd;
        raise exn
    with
    | Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
        Error (Storage_error.Conflict conflict_message)
    | Unix.Unix_error (error, operation, path) ->
        Error
          (Storage_error.Io
             (Printf.sprintf "%s (%s %s: %s)" io_message operation path
                (Unix.error_message error)))
  with
  | Error _ as error -> error
  | Ok fd ->
      Fun.protect
        ~finally:(fun () ->
          (try Unix.lockf fd Unix.F_ULOCK 0 with Unix.Unix_error _ -> ());
          Unix.close fd)
        fn

let with_file_lock file fn =
  let lock = lock_path file in
  with_advisory_lock ~native:(Eio.Path.native_exn lock)
    ~conflict_message:
      (Printf.sprintf "Another Caledonia writer holds the lock for %s"
         (snd file))
    ~io_message:(Printf.sprintf "Cannot lock %s" (snd file))
    (fun () ->
      try fn ()
      with Eio.Exn.Io _ as exn -> Error (io_error "Writing" file exn))

let with_calendar_lock ~fs calendar_dir calendar_key fn =
  let lock =
    Eio.Path.(
      fs / calendar_dir / calendar_key / ".caledonia-calendar-scope.lock")
  in
  with_advisory_lock ~native:(Eio.Path.native_exn lock)
    ~conflict_message:
      (Printf.sprintf "Another Caledonia writer is changing calendar %S"
         calendar_key)
    ~io_message:(Printf.sprintf "Cannot lock calendar %S" calendar_key)
    fn

let write_synced_exclusive path content =
  Eio.Path.with_open_out ~create:(`Exclusive 0o600) path (fun flow ->
      Eio.Flow.copy_string content flow;
      Eio.File.sync flow)

let validate_serialized_calendar content =
  match Calendar_codec.parse_document content with
  | Ok document -> Ok document
  | Error error ->
      Error
        (Storage_error.Invalid_document
           ("Refusing to persist an invalid VCALENDAR: " ^ error))

let save_backup file content =
  let backup =
    match Eio.Path.split file with
    | None -> invalid_arg "Calendar file must have a basename"
    | Some (dir, basename) ->
        let stamp = Int64.of_float (Unix.gettimeofday () *. 1_000_000.) in
        Eio.Path.(
          dir
          / Printf.sprintf ".caledonia-backup-%s-%020Ld-%s" basename stamp
              (random_suffix ()))
  in
  try
    write_synced_exclusive backup content;
    Ok backup
  with Eio.Exn.Io _ as exn -> Error (io_error "Creating backup for" file exn)

let prune_backups file =
  match Eio.Path.split file with
  | None -> ()
  | Some (dir, basename) ->
      let prefix = ".caledonia-backup-" ^ basename ^ "-" in
      let backups =
        Eio.Path.read_dir dir
        |> List.filter (String.starts_with ~prefix)
        |> List.sort (fun a b -> String.compare b a)
      in
      backups
      |> List.filteri (fun index _ -> index >= 10)
      |> List.iter (fun name ->
          try Eio.Path.unlink Eio.Path.(dir / name) with Eio.Exn.Io _ -> ())

let current_content file =
  try Ok (Eio.Path.load file)
  with Eio.Exn.Io _ as exn -> Error (io_error "Reading" file exn)

let verify_fingerprint ~file ~expected content =
  let actual = fingerprint content in
  if String.equal actual expected then Ok ()
  else
    Error
      (Storage_error.Conflict
         (Printf.sprintf
            "The source file %s changed after it was loaded; reload before \
             retrying"
            (snd file)))

let sync_parent_directory file =
  match Eio.Path.split file with
  | None -> ()
  | Some (parent, _) -> (
      try
        let fd =
          Unix.openfile (Eio.Path.native_exn parent) [ Unix.O_RDONLY ] 0
        in
        Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> Unix.fsync fd)
      with Unix.Unix_error _ -> ())

let atomic_replace_existing ~file ~expected_fingerprint content =
  let* _ = validate_serialized_calendar content in
  with_file_lock file (fun () ->
      let* original = current_content file in
      let* () =
        verify_fingerprint ~file ~expected:expected_fingerprint original
      in
      let temporary = sibling_path file ".caledonia-tmp-" in
      Fun.protect
        ~finally:(fun () ->
          try Eio.Path.unlink temporary with Eio.Exn.Io _ -> ())
        (fun () ->
          write_synced_exclusive temporary content;
          let written = Eio.Path.load temporary in
          let written =
            if For_test.consume For_test.Post_write_mismatch then
              if written = "" then "simulated-short-write"
              else String.sub written 0 (String.length written - 1)
            else written
          in
          if not (String.equal written content) then
            Error
              (Storage_error.Io
                 "The temporary calendar file failed write verification")
          else
            let* _ =
              if For_test.consume For_test.Temporary_parse_failure then
                Error
                  (Storage_error.Invalid_document
                     "Injected temporary calendar parse failure")
              else validate_serialized_calendar written
            in
            let* current = current_content file in
            let* () =
              verify_fingerprint ~file ~expected:expected_fingerprint current
            in
            let* _backup = save_backup file current in
            let* current = current_content file in
            let* () =
              verify_fingerprint ~file ~expected:expected_fingerprint current
            in
            let* () =
              if For_test.consume For_test.Rename_failure then
                Error
                  (Storage_error.Io
                     "Injected failure before atomic calendar rename")
              else Ok ()
            in
            Eio.Path.rename temporary file;
            sync_parent_directory file;
            prune_backups file;
            Ok (fingerprint content)))

let atomic_create ~file content =
  let* _ = validate_serialized_calendar content in
  with_file_lock file (fun () ->
      if Eio.Path.kind ~follow:false file <> `Not_found then
        Error
          (Storage_error.Conflict
             (Printf.sprintf "The target file %s already exists" (snd file)))
      else
        let temporary = sibling_path file ".caledonia-tmp-" in
        Fun.protect
          ~finally:(fun () ->
            try Eio.Path.unlink temporary with Eio.Exn.Io _ -> ())
          (fun () ->
            write_synced_exclusive temporary content;
            let written = Eio.Path.load temporary in
            if not (String.equal written content) then
              Error
                (Storage_error.Io
                   "The temporary calendar file failed write verification")
            else
              let* _ = validate_serialized_calendar written in
              try
                (* Installing the fully-written inode with [link] gives create
                   operations no-replace semantics.  A final [rename] would
                   overwrite a file created by an external writer between the
                   existence check and installation. *)
                Unix.link
                  (Eio.Path.native_exn temporary)
                  (Eio.Path.native_exn file);
                Eio.Path.unlink temporary;
                sync_parent_directory file;
                Ok (fingerprint content)
              with
              | Unix.Unix_error (Unix.EEXIST, _, _) ->
                  Error
                    (Storage_error.Conflict
                       (Printf.sprintf
                          "The target file %s appeared during creation"
                          (snd file)))
              | Unix.Unix_error (error, operation, path) ->
                  Error
                    (Storage_error.Io
                       (Printf.sprintf "Cannot install %s (%s %s: %s)"
                          (snd file) operation path (Unix.error_message error)))))

let atomic_delete ~file ~expected_fingerprint =
  with_file_lock file (fun () ->
      let* original = current_content file in
      let* () =
        verify_fingerprint ~file ~expected:expected_fingerprint original
      in
      let* _backup = save_backup file original in
      let* current = current_content file in
      let* () =
        verify_fingerprint ~file ~expected:expected_fingerprint current
      in
      Eio.Path.unlink file;
      sync_parent_directory file;
      prune_backups file;
      Ok ())

let components_from_content ~fs calendar_dir calendar_key file content =
  let display_name = get_display_name ~fs calendar_dir calendar_key in
  let source =
    Component_source.of_decoded_document ~calendar_key ~display_name ~file
      ~fingerprint:(fingerprint content) ()
  in
  Calendar_document.parse ~source content
  |> Result.map Calendar_document.components
  |> invalid_document

let snapshot_components = List.concat_map Calendar_document.components

let load_calendar_snapshot ~fs calendar_dir calendar_key =
  match get_calendar_documents ~fs calendar_dir calendar_key with
  | Ok documents -> Ok documents
  | Error `Not_found ->
      Error
        (Storage_error.Missing_target
           (Printf.sprintf "Calendar %S does not exist" calendar_key))
  | Error (`Msg message) -> Error (Storage_error.Invalid_document message)

let verify_calendar_snapshot snapshot =
  For_test.run_before_snapshot_verification ();
  let rec verify = function
    | [] -> Ok ()
    | document :: rest ->
        let source = Calendar_document.source document in
        let file = Component_source.file source in
        let* content = current_content file in
        let* () =
          verify_fingerprint ~file
            ~expected:(Component_source.fingerprint source)
            content
        in
        verify rest
  in
  verify snapshot

let reload_installed_components ~fs calendar_dir calendar_key file
    ~expected_fingerprint =
  let* content = current_content file in
  let* () = verify_fingerprint ~file ~expected:expected_fingerprint content in
  components_from_content ~fs calendar_dir calendar_key file content

let update_cached_file components file replacements =
  let path = snd file in
  replacements
  @ List.filter
      (fun component -> snd (Component.get_file component) <> path)
      components

let validate_todo_graph components =
  let by_calendar =
    components
    |> List.filter_map (fun component ->
        Option.map
          (fun todo -> (Component.get_calendar_key component, todo))
          (Component.to_todo component))
    |> List.fold_left
         (fun grouped (key, todo) ->
           Calendar_key_map.update key
             (fun existing -> Some (todo :: Option.value ~default:[] existing))
             grouped)
         Calendar_key_map.empty
  in
  Calendar_key_map.fold
    (fun _ todos result ->
      let* () = result in
      Todo.validate_parent_graph todos)
    by_calendar (Ok ())
  |> invalid_replacement

let canonical_component identity components =
  match
    List.filter
      (fun component ->
        Component.get_identity component |> Component_identity.equal identity)
      components
  with
  | [ component ] -> Ok component
  | [] ->
      Error
        (Storage_error.Missing_target
           "Canonical post-write reload did not contain the changed component")
  | _ ->
      Error
        (Storage_error.Ambiguous_identity
           "Canonical post-write reload contained an ambiguous component \
            identity")

let create_stored_component ~fs calendar_dir ~calendar_key body =
  let* () =
    validate_calendar_key calendar_key
    |> Result.map_error (fun (`Msg message) ->
        Storage_error.Path_violation message)
  in
  let calendar_path = get_calendar_path ~fs calendar_dir calendar_key in
  let* () =
    if Eio.Path.is_directory calendar_path then Ok ()
    else
      Error
        (Storage_error.Missing_target
           (Printf.sprintf "Calendar %S does not exist" calendar_key))
  in
  with_calendar_lock ~fs calendar_dir calendar_key (fun () ->
      let* () =
        validate_calendar_key calendar_key
        |> Result.map_error (fun (`Msg message) ->
            Storage_error.Path_violation message)
      in
      let identity = Component.identity_of_body body in
      let calendar_path = get_calendar_path ~fs calendar_dir calendar_key in
      let* () =
        if Eio.Path.is_directory calendar_path then Ok ()
        else
          Error
            (Storage_error.Missing_target
               (Printf.sprintf "Calendar %S does not exist" calendar_key))
      in
      let basename = identity.uid ^ ".ics" in
      let* () =
        if Filename.basename basename = basename then Ok ()
        else
          Error
            (Storage_error.Path_violation
               "Generated component UID is not a safe filename")
      in
      let file = Eio.Path.(calendar_path / basename) in
      let* () = validate_file_path ~fs calendar_dir calendar_key file in
      let* known_entries =
        Calendar_document.known_entries_of_body body
        |> Result.map_error (fun (`Msg message) ->
            Storage_error.Invalid_replacement message)
      in
      let codec =
        Calendar_codec.create_known
          ~properties:
            [
              `Prodid (Params.empty, "-//Freumh//Caledonia//EN");
              `Version (Params.empty, "2.0");
            ]
          known_entries
      in
      let content = Calendar_codec.serialize ~cr:true codec in
      let* snapshot = load_calendar_snapshot ~fs calendar_dir calendar_key in
      let* candidate =
        components_from_content ~fs calendar_dir calendar_key file content
      in
      let* () =
        validate_todo_graph (candidate @ snapshot_components snapshot)
      in
      let* () = verify_calendar_snapshot snapshot in
      let* installed_fingerprint = atomic_create ~file content in
      let* installed =
        reload_installed_components ~fs calendar_dir calendar_key file
          ~expected_fingerprint:installed_fingerprint
      in
      canonical_component identity installed)

let replace_stored_component ~fs calendar_dir ~original ~replacement =
  let calendar_key = Component.get_calendar_key original in
  with_calendar_lock ~fs calendar_dir calendar_key (fun () ->
      let original_identity = Component.get_identity original in
      let replacement_identity = Component.identity_of_body replacement in
      let* () =
        if Component_identity.equal original_identity replacement_identity then
          Ok ()
        else
          Error
            (Storage_error.Invalid_replacement
               "An edit cannot change component identity")
      in
      let file = Component.get_file original in
      let* () = validate_file_path ~fs calendar_dir calendar_key file in
      let expected_fingerprint = Component.get_source_fingerprint original in
      let* current_content = current_content file in
      let* () =
        verify_fingerprint ~file ~expected:expected_fingerprint current_content
      in
      let* current_document =
        Calendar_document.parse
          ~source:(Component.get_source original)
          current_content
        |> invalid_document
      in
      let* updated_document =
        Calendar_document.replace current_document ~target:original_identity
          ~replacement
        |> invalid_replacement
      in
      let content = Calendar_document.serialize ~cr:true updated_document in
      let* snapshot = load_calendar_snapshot ~fs calendar_dir calendar_key in
      let* candidate =
        components_from_content ~fs calendar_dir calendar_key file content
      in
      let* () =
        validate_todo_graph
          (update_cached_file (snapshot_components snapshot) file candidate)
      in
      let* () = verify_calendar_snapshot snapshot in
      let* installed_fingerprint =
        if String.equal content current_content then Ok expected_fingerprint
        else atomic_replace_existing ~file ~expected_fingerprint content
      in
      let* installed =
        reload_installed_components ~fs calendar_dir calendar_key file
          ~expected_fingerprint:installed_fingerprint
      in
      canonical_component original_identity installed)

let remove_stored_component ~fs calendar_dir component =
  let calendar_key = Component.get_calendar_key component in
  with_calendar_lock ~fs calendar_dir calendar_key (fun () ->
      let file = Component.get_file component in
      let* () = validate_file_path ~fs calendar_dir calendar_key file in
      let expected_fingerprint = Component.get_source_fingerprint component in
      let target = Component.get_identity component in
      let* content = current_content file in
      let* () =
        verify_fingerprint ~file ~expected:expected_fingerprint content
      in
      let* document =
        Calendar_document.parse ~source:(Component.get_source component) content
        |> invalid_document
      in
      let* remaining =
        Calendar_document.delete document ~target
        |> Result.map_error (fun (`Msg message) ->
            Storage_error.Missing_target message)
      in
      let rewritten = Calendar_document.serialize ~cr:true remaining in
      let* snapshot = load_calendar_snapshot ~fs calendar_dir calendar_key in
      if not (Calendar_document.has_entries remaining) then
        let* () =
          validate_todo_graph
            (update_cached_file (snapshot_components snapshot) file [])
        in
        let* () = verify_calendar_snapshot snapshot in
        let* () = atomic_delete ~file ~expected_fingerprint in
        if Eio.Path.kind ~follow:false file = `Not_found then Ok File_deleted
        else
          Error
            (Storage_error.Conflict
               "The deleted calendar file was recreated before canonical reload")
      else
        let* survivors =
          components_from_content ~fs calendar_dir calendar_key file rewritten
        in
        let* () =
          validate_todo_graph
            (update_cached_file (snapshot_components snapshot) file survivors)
        in
        let* () = verify_calendar_snapshot snapshot in
        let* installed_fingerprint =
          atomic_replace_existing ~file ~expected_fingerprint rewritten
        in
        let* installed =
          reload_installed_components ~fs calendar_dir calendar_key file
            ~expected_fingerprint:installed_fingerprint
        in
        Ok (Document_rewritten installed))

let delete_occurrence ~fs calendar_dir component reference =
  let* event =
    match Component.to_event component with
    | Some event -> Ok event
    | None ->
        Error
          (Storage_error.Unsupported
             "Occurrence deletion requires a stored event series")
  in
  match Event.Recurrence.delete_occurrence event reference with
  | Error (`Msg message) -> Error (Storage_error.Invalid_replacement message)
  | Ok modified ->
      replace_stored_component ~fs calendar_dir ~original:component
        ~replacement:(Component.event_body modified)

let add_occurrence_override ~fs calendar_dir component reference
    override_ical_event =
  let* event =
    match Component.to_event component with
    | Some event -> Ok event
    | None ->
        Error
          (Storage_error.Unsupported
             "Occurrence override requires a stored event series")
  in
  let override_identity =
    Component_identity.of_ical_component (`Event override_ical_event)
  in
  let* override_identity =
    match override_identity with
    | Some ({ recurrence_id = Some _; _ } as identity)
      when String.equal identity.uid (Event.get_id event) ->
        Ok identity
    | _ ->
        Error
          (Storage_error.Invalid_replacement
             "Occurrence override must have the master UID and a RECURRENCE-ID")
  in
  let* () =
    Event.Recurrence.validate_override event reference override_ical_event
    |> invalid_replacement
  in
  let overrides = Event.overrides event in
  let without_existing =
    List.filter
      (fun candidate ->
        match Component_identity.of_ical_component (`Event candidate) with
        | Some identity -> not (same_identity override_identity identity)
        | None -> true)
      overrides
  in
  let* modified =
    Event.make ~date_until:(Event.date_until event) ~master:(Event.master event)
      ~overrides:(without_existing @ [ override_ical_event ])
    |> invalid_replacement
  in
  replace_stored_component ~fs calendar_dir ~original:component
    ~replacement:(Component.event_body modified)

let get_path t = t
