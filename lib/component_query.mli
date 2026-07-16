(** Typed, timezone-explicit queries over all supported calendar components. *)

type text_field = Summary | Description | Location | Categories
type sort_field = Start | End | Summary_sort | Location_sort | Calendar | Type
type sort_spec = { field : sort_field; descending : bool }

(** A query result is either a persisted component or a bounded derived event
    occurrence. [stored_series] is the occurrence's exact immutable source
    target; it is not a stored representation of the occurrence. Occurrences
    remain nominal values and cannot be passed to storage APIs accepting
    [Component.t]. *)
type item =
  | Stored of Component.t
  | Occurrence of {
      stored_series : Component.t;
      occurrence : Event.Occurrence.t;
    }

val source : item -> Component_source.t

val stored : item -> Component.t option
(** The selected value only when it is itself persisted. *)

val occurrence : item -> Event.Occurrence.t option
val component_type : item -> Component_kind.t
val get_id : item -> string
val get_identity : item -> Component_identity.t
val get_target : item -> Component_target.t

val get_recurrence_id_property :
  item -> (Icalendar.params * Icalendar.date_or_datetime) option

val get_summary : item -> string option
val get_description : item -> string option
val get_categories : item -> string list
val get_location : item -> string option
val get_alarms : item -> Icalendar.alarm list
val get_status : item -> Icalendar.status option
val get_calendar_key : item -> string
val get_calendar_name : item -> string
val get_source_fingerprint : item -> string
val get_file : item -> Eio.Fs.dir_ty Eio.Path.t

val get_start_result :
  floating_tz:Timedesc.Time_zone.t ->
  item ->
  (Ptime.t option, [ `Msg of string ]) result

val get_end_result :
  floating_tz:Timedesc.Time_zone.t ->
  item ->
  (Ptime.t option, [ `Msg of string ]) result

type criteria = {
  calendars : string list;
  component_types : Component_kind.t list;
  text : string option;
  text_fields : text_field list;
  categories : string list;
  id : string option;
  statuses : Icalendar.status list;
  completed : bool option;
  overdue : bool option;
  recurring : bool option;
  has_alarm : bool option;
}

val no_criteria : criteria

val validate_criteria : criteria -> (unit, [> `Msg of string ]) result
(** Reject statuses inapplicable to the selected component types before a query
    can silently produce an empty result. *)

val status_of_string : string -> (Icalendar.status, [> `Msg of string ]) result
(** Parse one case-insensitive CLI/protocol status token at the boundary. *)

val timezone_of_name :
  string -> (Timedesc.Time_zone.t, [ `Msg of string ]) result

val run :
  timezone:Timedesc.Time_zone.t ->
  now:Ptime.t ->
  from:Ptime.t option ->
  to_:Ptime.t ->
  ?include_undated_todos:bool ->
  ?include_undated_journals:bool ->
  ?include_todo_ancestors:bool ->
  ?max_instances:int ->
  ?sort:sort_spec list ->
  ?limit:int ->
  criteria:criteria ->
  Component.t list ->
  (item list, [ `Msg of string ]) result
(** Query [components] with [from] inclusive and [to_] exclusive.

    DATE and floating values are interpreted in [timezone]. UTC and TZID values
    retain their own semantics. Conversion, corrupt todo graph, and bounded
    recurrence errors are returned rather than silently ignored.

    An undated VTODO/VJOURNAL is included only when the corresponding
    [include_undated_*] policy is true. Other undated component types are never
    in a temporal query. Filters are applied to expanded event occurrences. Todo
    ancestors, when requested, are added after matching and before the common
    sort/limit stage. *)

val run_unbounded :
  timezone:Timedesc.Time_zone.t ->
  now:Ptime.t ->
  ?include_todo_ancestors:bool ->
  ?sort:sort_spec list ->
  ?limit:int ->
  criteria:criteria ->
  Component.t list ->
  (item list, [ `Msg of string ]) result
(** Query component masters without a temporal range.

    Every matching non-recurring component and each recurring VEVENT master is
    considered exactly once. Recurrences are deliberately not expanded: an
    unbounded recurrence set is not finite in general. Date-constrained queries
    should use {!run}, whose expansion is bounded by the requested half-open
    range and [max_instances]. Undated todos and journals are included. *)
