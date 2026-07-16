(** Selected iCalendar export with explicit immutable document context. *)

val to_ics :
  documents:Calendar_document.t list ->
  Component_query.item list ->
  (string, [ `Msg of string ]) result
(** Serialize the selection as one self-contained VCALENDAR.

    Only VTIMEZONE definitions referenced by the exported components are
    included. Identical definitions are deduplicated and conflicting definitions
    for a referenced TZID are rejected. Every selected stored source must have a
    matching document snapshot. Stored series export their exact authored body;
    occurrences export only their effective finite VEVENT. The empty selection
    returns the empty stream. *)

val stored_to_ics :
  documents:Calendar_document.t list ->
  Component.t list ->
  (string, [ `Msg of string ]) result
(** Convenience entry point for selections containing only stored views. *)
