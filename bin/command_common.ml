open Caledonia_lib

let storage_error error =
  let message = Storage_error.message error in
  if Storage_error.is_conflict error then `Conflict message else `Msg message

let storage_result result = Result.map_error storage_error result

let map_result f values =
  let rec loop accumulated = function
    | [] -> Ok (List.rev accumulated)
    | value :: rest ->
        Result.bind (f value) (fun mapped -> loop (mapped :: accumulated) rest)
  in
  loop [] values

let find_unique_component ~id components =
  match
    List.filter (fun component -> Component.get_id component = id) components
  with
  | [ component ] -> Ok component
  | [] -> Error (`Msg ("No component found for id " ^ id))
  | _ -> Error (`Msg ("Multiple components found for id " ^ id))
