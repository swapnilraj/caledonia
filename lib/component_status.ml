let bindings =
  [
    ("tentative", `Tentative);
    ("confirmed", `Confirmed);
    ("cancelled", `Cancelled);
    ("needs-action", `Needs_action);
    ("completed", `Completed);
    ("in-process", `In_process);
    ("draft", `Draft);
    ("final", `Final);
  ]

let to_string status =
  List.find_map
    (fun (name, candidate) -> if candidate = status then Some name else None)
    bindings
  |> Option.get

let of_string value =
  match List.assoc_opt (String.lowercase_ascii value) bindings with
  | Some status -> Ok status
  | None -> Error (`Msg (Printf.sprintf "unknown component status %S" value))
