(** Todo task handling *)

type t

val create :
  now:Ptime.t ->
  ?summary:string ->
  ?start:Icalendar.params * Icalendar.date_or_datetime ->
  ?due:Icalendar.params * Icalendar.date_or_datetime ->
  ?duration:Icalendar.params * Ptime.Span.t ->
  ?description:string ->
  ?categories:string list ->
  ?status:Icalendar.status ->
  ?priority:int ->
  ?percent:int ->
  ?parent:string ->
  ?alarms:Icalendar.alarm list ->
  unit ->
  (t, [> `Msg of string ]) result

val edit :
  now:Ptime.t ->
  ?summary:string Patch.t ->
  ?start:(Icalendar.params * Icalendar.date_or_datetime) Patch.t ->
  ?due:(Icalendar.params * Icalendar.date_or_datetime) Patch.t ->
  ?duration:(Icalendar.params * Ptime.Span.t) Patch.t ->
  ?description:string Patch.t ->
  ?categories:string list Patch.t ->
  ?status:Icalendar.status Patch.t ->
  ?priority:int Patch.t ->
  ?percent:int Patch.t ->
  ?parent:string Patch.t ->
  ?alarms:Icalendar.alarm list Patch.t ->
  t ->
  (t, [> `Msg of string ]) result

val of_ical_body :
  Icalendar.todo_prop list * Icalendar.alarm list ->
  (t, [> `Msg of string ]) result
(** Decode and validate one marker-free VTODO body. Physical document traversal
    remains owned by [Calendar_document]. *)

val to_ical_todo : t -> Icalendar.todo_prop list
val get_id : t -> string
val get_summary : t -> string option
val get_start_time : t -> Icalendar.date_or_datetime option
val get_due_time : t -> Icalendar.date_or_datetime option
val get_duration : t -> Ptime.Span.t option

val get_start_result :
  floating_tz:Timedesc.Time_zone.t ->
  t ->
  (Ptime.t option, Date.conversion_error) result

val get_due_result :
  floating_tz:Timedesc.Time_zone.t ->
  t ->
  (Ptime.t option, Date.conversion_error) result

val get_description : t -> string option
val get_categories : t -> string list
val get_status : t -> Icalendar.status option
val get_priority : t -> int option
val get_percent : t -> int option
val get_completed : t -> Ptime.t option
val get_alarms : t -> Icalendar.alarm list
val get_related_parent : t -> string option
val is_completed : t -> bool

val is_overdue_at :
  now:Ptime.t ->
  tz:Timedesc.Time_zone.t ->
  t ->
  (bool, Date.conversion_error) result

type todo_tree = { todo : t; children : todo_tree list }

val get_ancestors :
  all_todos:t list -> t -> (t list, [> `Msg of string ]) result

val expand_with_ancestors :
  all_todos:t list ->
  filtered_todos:t list ->
  (t list, [> `Msg of string ]) result

val build_todo_tree : t list -> (todo_tree list, [> `Msg of string ]) result
val validate_parent_graph : t list -> (unit, [> `Msg of string ]) result

val compute_alarm_fires_result :
  floating_tz:Timedesc.Time_zone.t ->
  from:Ptime.t option ->
  to_:Ptime.t ->
  t ->
  (t Alarm.fire list, [ `Msg of string ]) result
