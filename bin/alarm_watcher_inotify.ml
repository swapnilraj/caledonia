type event = Timer | Changed | Overflow

type t = {
  mutable fd : Unix.file_descr;
  root : string;
  mutable paths : (int, string) Hashtbl.t;
  mutable forced_overflow : bool;
  mutable forced_rebuild_failure : string option;
  mutable forced_nested_rebuild_failure : (string * string) option;
  mutable needs_rebuild : bool;
  mutable generation : int;
}

let backend_name = "Linux inotify"

let selectors =
  [
    Inotify.S_Attrib;
    Inotify.S_Close_write;
    Inotify.S_Create;
    Inotify.S_Delete;
    Inotify.S_Delete_self;
    Inotify.S_Modify;
    Inotify.S_Move_self;
    Inotify.S_Moved_from;
    Inotify.S_Moved_to;
    Inotify.S_Dont_follow;
    Inotify.S_Onlydir;
  ]

let visible name = String.length name > 0 && name.[0] <> '.'

let kind_no_follow path =
  try Some (Unix.lstat path).Unix.st_kind
  with Unix.Unix_error (Unix.ENOENT, _, _) -> None

let report_symlink path =
  Printf.eprintf "Alarm watcher skipped symbolic link: %s\n%!"
    (Caledonia_lib.Format_utils.sanitize_terminal_line path)

exception Injected_rebuild_failure of string

let rec add_tree ?injected_failure fd paths path =
  (match injected_failure with
  | Some (target, message) when String.equal target path ->
      raise (Injected_rebuild_failure message)
  | Some _ | None -> ());
  match kind_no_follow path with
  | Some Unix.S_LNK -> report_symlink path
  | Some Unix.S_DIR ->
      let watch = Inotify.add_watch fd path selectors in
      Hashtbl.replace paths (Inotify.int_of_watch watch) path;
      Array.iter
        (fun name ->
          if visible name then
            let child = Filename.concat path name in
            match kind_no_follow child with
            | Some Unix.S_DIR -> add_tree ?injected_failure fd paths child
            | Some Unix.S_LNK -> report_symlink child
            | _ -> ())
        (Sys.readdir path)
  | _ -> ()

let new_watch_set ?injected_failure ?(include_root = true) root =
  let fd = Inotify.create () in
  let paths = Hashtbl.create 32 in
  try
    if include_root then add_tree ?injected_failure fd paths root;
    (fd, paths)
  with exn ->
    Unix.close fd;
    raise exn

let rebuild_watch_set ?injected_failure root =
  match kind_no_follow root with
  | None -> new_watch_set ~include_root:false root
  | Some Unix.S_DIR -> new_watch_set ?injected_failure root
  | Some Unix.S_LNK ->
      invalid_arg "refusing to watch a symbolic-link calendar root"
  | Some _ -> invalid_arg "calendar root is not a directory"

let create root =
  try
    match kind_no_follow root with
    | Some Unix.S_LNK ->
        Error (`Msg "refusing to watch a symbolic-link calendar root")
    | Some Unix.S_DIR ->
        let fd, paths = new_watch_set root in
        Ok
          {
            fd;
            root;
            paths;
            forced_overflow = false;
            forced_rebuild_failure = None;
            forced_nested_rebuild_failure = None;
            needs_rebuild = false;
            generation = 0;
          }
    | _ -> Error (`Msg "calendar root is not a directory")
  with
  | Unix.Unix_error (error, operation, path) ->
      Error
        (`Msg
           (Printf.sprintf "inotify %s failed for %s: %s" operation path
              (Unix.error_message error)))
  | Sys_error message | Invalid_argument message -> Error (`Msg message)

let message_of_exception context = function
  | Unix.Unix_error (error, operation, path) ->
      Printf.sprintf "%s: inotify %s failed for %s: %s" context operation path
        (Unix.error_message error)
  | Sys_error message | Invalid_argument message -> context ^ ": " ^ message
  | _ -> assert false

(* Rebuilding on a fresh descriptor is deliberate. Removing a watch queues an
   IGNORED event, and Linux may reuse that descriptor before the event is read.
   Updating one descriptor at a time can therefore make a stale IGNORED event
   erase the mapping for a new watch. A descriptor swap also gives queue
   overflow recovery a clean snapshot and immediately drops watches for trees
   that were moved outside [root]. The live descriptor is swapped only after
   the replacement snapshot succeeds, so a failed rebuild leaves the previous
   watch set usable until the safety timer retries. *)
let rebuild watcher =
  watcher.needs_rebuild <- true;
  match watcher.forced_rebuild_failure with
  | Some message ->
      watcher.forced_rebuild_failure <- None;
      Error (`Msg message)
  | None -> (
      try
        let injected_failure = watcher.forced_nested_rebuild_failure in
        watcher.forced_nested_rebuild_failure <- None;
        let fd, paths = rebuild_watch_set ?injected_failure watcher.root in
        let old_fd = watcher.fd in
        watcher.fd <- fd;
        watcher.paths <- paths;
        watcher.needs_rebuild <- false;
        watcher.generation <- watcher.generation + 1;
        try
          Unix.close old_fd;
          Ok ()
        with (Unix.Unix_error _ | Sys_error _ | Invalid_argument _) as exn ->
          Error (`Msg (message_of_exception "closing old watch set" exn))
      with
      | Injected_rebuild_failure message -> Error (`Msg message)
      | (Unix.Unix_error _ | Sys_error _ | Invalid_argument _) as exn ->
          Error (`Msg (message_of_exception "rebuilding watch set" exn)))

let consume watcher =
  let overflow = ref false in
  let topology_changed = ref false in
  let meaningful = ref false in
  try
    List.iter
      (fun (_watch, kinds, _cookie, name) ->
        if List.mem Inotify.Q_overflow kinds then overflow := true
        else if
          match name with Some name -> not (visible name) | None -> false
        then ()
        else if
          let () = meaningful := true in
          List.mem Inotify.Ignored kinds
          || List.mem Inotify.Unmount kinds
          || List.mem Inotify.Delete_self kinds
          || List.mem Inotify.Move_self kinds
          || List.mem Inotify.Isdir kinds
             && (List.mem Inotify.Create kinds
                || List.mem Inotify.Delete kinds
                || List.mem Inotify.Moved_from kinds
                || List.mem Inotify.Moved_to kinds)
        then topology_changed := true)
      (Inotify.read watcher.fd);
    if !overflow then Result.map (fun () -> Some Overflow) (rebuild watcher)
    else if !topology_changed then
      Result.map (fun () -> Some Changed) (rebuild watcher)
    else Ok (if !meaningful then Some Changed else None)
  with (Unix.Unix_error _ | Sys_error _ | Invalid_argument _) as exn ->
    watcher.needs_rebuild <- true;
    Error (`Msg (message_of_exception "reading watch events" exn))

let watches_root watcher =
  Hashtbl.fold
    (fun _ path found -> found || String.equal path watcher.root)
    watcher.paths false

let wait ~clock ~timeout watcher =
  try
    if watcher.forced_overflow then (
      watcher.forced_overflow <- false;
      Result.map (fun () -> Overflow) (rebuild watcher))
    else if watcher.needs_rebuild then (
      Eio.Time.sleep clock timeout;
      Result.map (fun () -> Timer) (rebuild watcher))
    else
      Eio.Fiber.first
        (fun () ->
          let rec await_meaningful_event () =
            Eio_unix.await_readable watcher.fd;
            match consume watcher with
            | Ok (Some event) -> Ok event
            | Ok None -> await_meaningful_event ()
            | Error _ as error -> error
          in
          await_meaningful_event ())
        (fun () ->
          Eio.Time.sleep clock timeout;
          (* The root itself cannot be observed while it is absent. Retrying
             the snapshot on the safety timer makes root replacement recover
             without requiring a process restart. *)
          if not (watches_root watcher) then
            Result.map (fun () -> Timer) (rebuild watcher)
          else Ok Timer)
  with (Unix.Unix_error _ | Sys_error _ | Invalid_argument _) as exn ->
    watcher.needs_rebuild <- true;
    Error (`Msg (message_of_exception "waiting for watch events" exn))

let close watcher = Unix.close watcher.fd

module For_test = struct
  let inject_overflow watcher = watcher.forced_overflow <- true

  let inject_rebuild_failure watcher message =
    watcher.forced_rebuild_failure <- Some message

  let inject_nested_rebuild_failure watcher ~path message =
    watcher.forced_nested_rebuild_failure <- Some (path, message)

  let watch_count watcher = Hashtbl.length watcher.paths
  let generation watcher = watcher.generation

  let descriptor_open watcher =
    try
      ignore (Unix.fstat watcher.fd);
      true
    with Unix.Unix_error (Unix.EBADF, _, _) -> false
end
