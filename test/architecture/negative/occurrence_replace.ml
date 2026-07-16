open Caledonia_lib

let invalid_replace ~fs calendar_dir (occurrence : Event.Occurrence.t)
    replacement =
  Calendar_dir.replace_stored_component ~fs calendar_dir ~original:occurrence
    ~replacement
