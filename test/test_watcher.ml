let rec remove path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | stat when stat.st_kind = Unix.S_DIR ->
      Sys.readdir path
      |> Array.iter (fun name -> remove (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_temporary_directory fn =
  let root = Filename.temp_file "caledonia-watcher-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () -> remove root) (fun () -> fn root)

let save path value =
  Out_channel.with_open_bin path (fun channel -> output_string channel value)

let%expect_test "Linux inotify watches initial, created, and moved subtrees" =
  let linux_inotify_available =
    Sys.file_exists "/proc/sys/fs/inotify/max_user_watches"
  in
  if linux_inotify_available && Alarm_watcher.backend_name <> "Linux inotify"
  then failwith "Linux build selected the polling watcher instead of inotify";
  (if Alarm_watcher.backend_name <> "Linux inotify" then
     print_endline "watcher-contract=true"
   else
     Eio_main.run @@ fun env ->
     with_temporary_directory @@ fun root ->
     let initial = Filename.concat root "initial" in
     Unix.mkdir initial 0o700;
     let watcher = Result.get_ok (Alarm_watcher.create root) in
     Fun.protect
       ~finally:(fun () -> Alarm_watcher.close watcher)
       (fun () ->
         let wait () =
           Alarm_watcher.wait ~clock:(Eio.Stdenv.clock env) ~timeout:1. watcher
           |> Result.get_ok
         in
         let changed = function Alarm_watcher.Changed -> true | _ -> false in
         let timer = function Alarm_watcher.Timer -> true | _ -> false in
         let overflow = function
           | Alarm_watcher.Overflow -> true
           | _ -> false
         in
         let hidden_temp = Filename.concat initial ".alarm-state.tmp" in
         let hidden_state = Filename.concat initial ".alarm-state.json" in
         save hidden_temp "temporary";
         Unix.rename hidden_temp hidden_state;
         save hidden_state "state";
         let hidden_event =
           Alarm_watcher.wait ~clock:(Eio.Stdenv.clock env) ~timeout:0.05
             watcher
           |> Result.get_ok
         in
         save (Filename.concat initial "first.ics") "initial";
         let initial_event = wait () in
         let created = Filename.concat root "created" in
         Unix.mkdir created 0o700;
         let created_directory_event = wait () in
         save (Filename.concat created "second.ics") "created";
         let created_child_event = wait () in
         let outside = Filename.temp_file "caledonia-moved-watcher-" "" in
         Sys.remove outside;
         Unix.mkdir outside 0o700;
         save (Filename.concat outside "existing.ics") "moved";
         let moved = Filename.concat root "moved" in
         Unix.rename outside moved;
         let moved_directory_event = wait () in
         save (Filename.concat moved "after-move.ics") "after";
         let moved_child_event = wait () in
         let watch_count_before_eviction =
           Alarm_watcher.For_test.watch_count watcher
         in
         let evicted = Filename.temp_file "caledonia-evicted-watcher-" "" in
         Sys.remove evicted;
         Fun.protect
           ~finally:(fun () -> remove evicted)
           (fun () ->
             Unix.rename moved evicted;
             let rec wait_for_count attempts expected =
               if Alarm_watcher.For_test.watch_count watcher = expected then
                 true
               else if attempts = 0 then false
               else
                 let _ = wait () in
                 wait_for_count (attempts - 1) expected
             in
             let moved_out_pruned =
               wait_for_count 4 (watch_count_before_eviction - 1)
             in
             save (Filename.concat evicted "outside.ics") "outside";
             let outside_event =
               Alarm_watcher.wait ~clock:(Eio.Stdenv.clock env) ~timeout:0.05
                 watcher
               |> Result.get_ok
             in
             let created_file = Filename.concat created "second.ics" in
             Unix.unlink created_file;
             Unix.rmdir created;
             let deleted_pruned =
               wait_for_count 4 (watch_count_before_eviction - 2)
             in
             if not (moved_out_pruned && timer outside_event && deleted_pruned)
             then failwith "inotify retained a moved or deleted subtree");
         let displaced_root =
           Filename.temp_file "caledonia-displaced-watch-root-" ""
         in
         Sys.remove displaced_root;
         let recovered_child_event =
           Fun.protect
             ~finally:(fun () -> remove displaced_root)
             (fun () ->
               Unix.rename root displaced_root;
               let root_move_event = wait () in
               if
                 not
                   (changed root_move_event
                   && Alarm_watcher.For_test.watch_count watcher = 0)
               then failwith "moved root retained inotify descriptors";
               Unix.mkdir root 0o700;
               let recovery_event =
                 Alarm_watcher.wait ~clock:(Eio.Stdenv.clock env) ~timeout:0.05
                   watcher
                 |> Result.get_ok
               in
               if
                 not
                   (timer recovery_event
                   && Alarm_watcher.For_test.watch_count watcher = 1)
               then failwith "watcher did not recover a replaced root";
               save (Filename.concat root "recovered.ics") "recovered";
               wait ())
         in
         let retained_count = Alarm_watcher.For_test.watch_count watcher in
         let retained_generation = Alarm_watcher.For_test.generation watcher in
         let partial_subtree = Filename.concat root "partial-subtree" in
         Unix.mkdir partial_subtree 0o700;
         Alarm_watcher.For_test.inject_nested_rebuild_failure watcher
           ~path:partial_subtree "injected rebuild failure";
         let rebuild_failure =
           Alarm_watcher.wait ~clock:(Eio.Stdenv.clock env) ~timeout:1. watcher
         in
         let failed_snapshot_retained =
           rebuild_failure = Error (`Msg "injected rebuild failure")
           && Alarm_watcher.For_test.watch_count watcher = retained_count
           && Alarm_watcher.For_test.generation watcher = retained_generation
           && Alarm_watcher.For_test.descriptor_open watcher
         in
         let rebuild_retry =
           Alarm_watcher.wait ~clock:(Eio.Stdenv.clock env) ~timeout:0.01
             watcher
           |> Result.get_ok
         in
         let retry_recovered =
           timer rebuild_retry
           && Alarm_watcher.For_test.generation watcher
              = retained_generation + 1
           && Alarm_watcher.For_test.watch_count watcher = retained_count + 1
         in
         save
           (Filename.concat partial_subtree "after-rebuild-recovery.ics")
           "recovered";
         let after_rebuild_failure_event = wait () in
         Alarm_watcher.For_test.inject_overflow watcher;
         let overflow_event = wait () in
         Printf.printf "watcher-contract=%b\n"
           (timer hidden_event && changed initial_event
           && changed created_directory_event
           && changed created_child_event
           && changed moved_directory_event
           && changed moved_child_event
           && changed recovered_child_event
           && failed_snapshot_retained && retry_recovered
           && changed after_rebuild_failure_event
           && overflow overflow_event)));
  [%expect {| watcher-contract=true |}]
