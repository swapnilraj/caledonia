open Caledonia_lib

let invalid_remove ~fs calendar_dir (occurrence : Event.Occurrence.t) =
  Calendar_dir.remove_stored_component ~fs calendar_dir occurrence
