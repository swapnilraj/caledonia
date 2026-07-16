(** Identity local to one physical calendar document. Source location and
    fingerprint live in {!Component_source}; together they form a complete
    mutation target. *)

type t = {
  kind : Component_kind.t;
  uid : string;
  recurrence_id : Icalendar.date_or_datetime option;
}

val compare : t -> t -> int
val equal : t -> t -> bool
val of_ical_component : Icalendar.component -> t option
