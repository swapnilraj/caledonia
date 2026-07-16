(** Journal entry handling *)

type t

val create :
  now:Ptime.t ->
  ?summary:string ->
  ?start:Icalendar.params * Icalendar.date_or_datetime ->
  ?description:string ->
  ?categories:string list ->
  ?status:Icalendar.status ->
  unit ->
  (t, [> `Msg of string ]) result

val edit :
  now:Ptime.t ->
  ?summary:string Patch.t ->
  ?start:(Icalendar.params * Icalendar.date_or_datetime) Patch.t ->
  ?description:string Patch.t ->
  ?categories:string list Patch.t ->
  ?status:Icalendar.status Patch.t ->
  t ->
  (t, [> `Msg of string ]) result

val of_ical_body :
  Icalendar.journal_prop list -> (t, [> `Msg of string ]) result
(** Decode and validate one marker-free VJOURNAL body. Physical document
    traversal remains owned by [Calendar_document]. *)

val to_ical_journal : t -> Icalendar.journal_prop list
val get_id : t -> string
val get_summary : t -> string option
val get_start_time : t -> Icalendar.date_or_datetime option

val get_start_result :
  floating_tz:Timedesc.Time_zone.t ->
  t ->
  (Ptime.t option, Date.conversion_error) result

val get_description : t -> string option
val get_categories : t -> string list
val get_status : t -> Icalendar.status option
