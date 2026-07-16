(** Unified component handling for events, todos, and journals *)

type t
type body

val component_type : t -> Component_kind.t
val event_body : Event.t -> body
val todo_body : Todo.t -> body
val journal_body : Journal.t -> body
val event_of_body : body -> Event.t option
val body : t -> body
val identity_of_body : body -> Component_identity.t
val ical_components_of_body : body -> Icalendar.component list
val to_event : t -> Event.t option
val to_todo : t -> Todo.t option
val to_journal : t -> Journal.t option
val get_id : t -> string
val get_identity : t -> Component_identity.t

val get_recurrence_id_property :
  t -> (Icalendar.params * Icalendar.date_or_datetime) option

val get_source : t -> Component_source.t
val get_target : t -> Component_target.t
val get_summary : t -> string option
val get_description : t -> string option
val get_categories : t -> string list
val get_calendar_name : t -> string
val get_calendar_key : t -> string
val get_source_fingerprint : t -> string
val get_file : t -> Eio.Fs.dir_ty Eio.Path.t
val get_alarms : t -> Icalendar.alarm list

val get_start_result :
  floating_tz:Timedesc.Time_zone.t ->
  t ->
  (Ptime.t option, Date.conversion_error) result

val stored_views_of_decoded_components :
  ?authored_events:(Icalendar.event * Ptime.date option) list ->
  source:Component_source.t ->
  Icalendar.component list ->
  (t list, [> `Msg of string ]) result
(** Decoder-only seam constructing immutable stored views from the supported
    components of one already validated physical document. The complete calendar
    and its metadata remain owned by [Calendar_document]; repository and domain
    callers must not manufacture lifecycle state through this function. *)
