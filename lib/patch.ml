type 'a t = Keep | Clear | Set of 'a

let apply patch ~current =
  match patch with Keep -> current | Clear -> None | Set value -> Some value

let replace_in_list matches make patch values =
  match patch with
  | Keep -> values
  | Clear -> List.filter (fun value -> not (matches value)) values
  | Set value ->
      make value :: List.filter (fun value -> not (matches value)) values
