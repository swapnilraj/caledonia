type event = Timer | Changed | Overflow
type t = { mutable forced_error : string option }

let backend_name = "portable polling"
let create _root = Ok { forced_error = None }

let wait ~clock ~timeout watcher =
  match watcher.forced_error with
  | Some message ->
      watcher.forced_error <- None;
      Error (`Msg message)
  | None ->
      Eio.Time.sleep clock timeout;
      Ok Timer

let close _watcher = ()

module For_test = struct
  let inject_overflow _watcher = ()

  let inject_rebuild_failure watcher message =
    watcher.forced_error <- Some message

  let inject_nested_rebuild_failure watcher ~path:_ message =
    watcher.forced_error <- Some message

  let watch_count _watcher = 0
  let generation _watcher = 0
  let descriptor_open _watcher = true
end
