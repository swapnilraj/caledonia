(** Guards against the pinned parser demoting malformed registered component
    properties into generic IANA extension properties. *)

val validate_singleton :
  component:Component_kind.t ->
  required:bool ->
  string ->
  'a list ->
  (unit, [> `Msg of string ]) result
(** Enforce the common zero-or-one or exactly-one component-property shape. *)

val validate_event_properties :
  Icalendar.event_prop list -> (unit, [> `Msg of string ]) result

val validate_todo_properties :
  Icalendar.todo_prop list -> (unit, [> `Msg of string ]) result

val validate_journal_properties :
  Icalendar.journal_prop list -> (unit, [> `Msg of string ]) result
