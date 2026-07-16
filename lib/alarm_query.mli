(** Alarm expansion over stored components. This boundary attaches repository
    source context to pure event/todo alarm calculations without making domain
    bodies source-aware. *)

type fire = Component_query.item Alarm.fire

val run_result :
  floating_tz:Timedesc.Time_zone.t ->
  from:Ptime.t option ->
  to_:Ptime.t ->
  Component.t list ->
  (fire list, [ `Msg of string ]) result
