(** Core event-series functionality and data access. *)

type recurrence_error = [ `Msg of string ]

type t
(** One validated authored VEVENT master and its ordered persisted RECURRENCE-ID
    overrides. It is intentionally distinct from {!Occurrence.t}. *)

val make :
  date_until:Ptime.date option ->
  master:Icalendar.event ->
  overrides:Icalendar.event list ->
  (t, [> `Msg of string ]) result

val of_events_result :
  Icalendar.event list -> (t list, [> `Msg of string ]) result
(** Construct event series from marker-free authored events that have no
    DATE-valued UNTIL metadata. *)

val of_authored_events_result :
  (Icalendar.event * Ptime.date option) list ->
  (t list, [> `Msg of string ]) result
(** Group one document's VEVENTs by UID and validate each complete series once
    while retaining the semantic DATE-valued UNTIL associated with each physical
    event. Overrides cannot carry DATE UNTIL metadata. Missing or duplicate
    masters, invalid overrides, duplicate recurrence identities, and
    temporal-kind mismatches are rejected here. *)

val validate : t -> (unit, recurrence_error) result
val master : t -> Icalendar.event
val overrides : t -> Icalendar.event list
val authored_events : t -> Icalendar.event list
val date_until : t -> Ptime.date option

module Occurrence : sig
  type origin = Generated | Persisted_override

  module Reference : sig
    type t

    val uid : t -> string
    val recurrence_id : t -> Icalendar.date_or_datetime

    val recurrence_id_property :
      t -> Icalendar.params * Icalendar.date_or_datetime

    val occurrence_start : t -> Ptime.t
    val query_timezone : t -> Timedesc.Time_zone.t
  end

  type t
  (** A bounded, effective recurrence instance. This nominal type cannot be
      passed to component or calendar storage APIs that accept [Event.t]. *)

  val reference : t -> Reference.t
  val origin : t -> origin

  (* Effective RFC VEVENT payload after recurrence inheritance. This is a
     derived export/presentation value, never a writable stored component. *)
  val effective_ical_event : t -> Icalendar.event
  val get_summary : t -> string option
  val get_start_result : t -> (Ptime.t, Date.conversion_error) result
  val get_end_result : t -> (Ptime.t option, Date.conversion_error) result
  val get_location : t -> string option
  val get_description : t -> string option
  val get_categories : t -> string list
  val get_alarms : t -> Icalendar.alarm list
  val is_date : t -> bool
  val get_start_timezone : t -> string option
  val get_end_timezone : t -> string option
end

module Recurrence : sig
  val expand :
    ?max_instances:int ->
    floating_tz:Timedesc.Time_zone.t ->
    from:Ptime.t option ->
    to_:Ptime.t ->
    t ->
    (Occurrence.t list, recurrence_error) result
  (** Expand only an authored recurrence set. A non-recurring series returns an
      empty list; callers that query mixed stored data and occurrences must
      retain non-recurring series separately. *)

  val resolve_reference :
    floating_tz:Timedesc.Time_zone.t ->
    t ->
    Ptime.t ->
    (Occurrence.Reference.t, recurrence_error) result
  (** Resolve one nominal occurrence timestamp into a validated, series-bound
      mutation reference using the same timezone policy as the originating
      bounded query. Existing moved overrides resolve by their nominal
      occurrence identity, not their effective DTSTART. *)

  val validate_override :
    t ->
    Occurrence.Reference.t ->
    Icalendar.event ->
    (unit, recurrence_error) result

  val delete_occurrence :
    t -> Occurrence.Reference.t -> (t, recurrence_error) result

  val create_override :
    now:Ptime.t ->
    t ->
    Occurrence.Reference.t ->
    ?summary:string Patch.t ->
    ?start:(Icalendar.params * Icalendar.date_or_datetime) Patch.t ->
    ?end_:
      [ `Duration of Icalendar.params * Ptime.Span.t
      | `Dtend of Icalendar.params * Icalendar.date_or_datetime ]
      Patch.t ->
    ?location:string Patch.t ->
    ?description:string Patch.t ->
    ?categories:string list Patch.t ->
    ?alarms:Icalendar.alarm list Patch.t ->
    unit ->
    (Icalendar.event, recurrence_error) result
end

(** {2 Events} *)

val create :
  now:Ptime.t ->
  summary:string ->
  start:Icalendar.params * Icalendar.date_or_datetime ->
  ?end_:
    [ `Duration of Icalendar.params * Ptime.Span.t
    | `Dtend of Icalendar.params * Icalendar.date_or_datetime ] ->
  ?location:string ->
  ?description:string ->
  ?categories:string list ->
  ?recurrence:Icalendar.recurrence ->
  ?recurrence_params:Icalendar.Params.t ->
  ?recurrence_date_until:Ptime.date ->
  ?alarms:Icalendar.alarm list ->
  unit ->
  (t, [> `Msg of string ]) result
(** Create a new event with required properties.

    The start and end times can be specified as Icalendar.timestamp values,
    which allows for directly using any of the three RFC5545 time formats:
    - `Utc time: Fixed to absolute UTC time
    - `Local time: Floating local time (follows user's timezone)
    - `With_tzid (time, timezone): Local time with timezone reference *)

val edit_patch :
  now:Ptime.t ->
  ?summary:string Patch.t ->
  ?start:(Icalendar.params * Icalendar.date_or_datetime) Patch.t ->
  ?end_:
    [ `Duration of Icalendar.params * Ptime.Span.t
    | `Dtend of Icalendar.params * Icalendar.date_or_datetime ]
    Patch.t ->
  ?location:string Patch.t ->
  ?description:string Patch.t ->
  ?categories:string list Patch.t ->
  ?recurrence:Icalendar.recurrence Patch.t ->
  ?recurrence_params:Icalendar.Params.t ->
  ?recurrence_date_until:Ptime.date ->
  ?alarms:Icalendar.alarm list Patch.t ->
  t ->
  (t, [> `Msg of string ]) result
(** Lossless event edit. Every field explicitly distinguishes keeping, clearing,
    and setting its value. DTSTART cannot be cleared. *)

val get_id : t -> string
val get_summary : t -> string option

val get_start_result :
  floating_tz:Timedesc.Time_zone.t ->
  t ->
  (Ptime.t, Date.conversion_error) result

val get_end_result :
  floating_tz:Timedesc.Time_zone.t ->
  t ->
  (Ptime.t option, Date.conversion_error) result

val is_date : t -> bool
(** Returns true if either the start or end timestamp is specified as a date
    instead of a datetime. *)

val get_start_timezone : t -> string option
val get_end_timezone : t -> string option
val get_location : t -> string option
val get_description : t -> string option
val get_categories : t -> string list
val get_recurrence : t -> Icalendar.recurrence option

val has_recurrence_set : t -> bool
(** Whether the event has RRULE or non-empty RDATE recurrence membership. *)

val get_alarms : t -> Icalendar.alarm list

(** {2 Alarm fire times} *)

type alarm_owner = Series of t | Occurrence of Occurrence.t

val compute_alarm_fires_result :
  ?max_instances:int ->
  floating_tz:Timedesc.Time_zone.t ->
  from:Ptime.t option ->
  to_:Ptime.t ->
  t ->
  (alarm_owner Alarm.fire list, recurrence_error) result
