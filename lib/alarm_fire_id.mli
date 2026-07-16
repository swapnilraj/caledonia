(** Stable persisted identity of one alarm delivery attempt. The record is named
    even though alarm-state schema v2 retains its positional JSON encoding. *)

type t = {
  calendar_key : string;
  source_file : string;
  uid : string;
  recurrence_key : string;
  alarm_index : int;
  alarm_key : string;
  fire_time : Ptime.t;
}

val compare : t -> t -> int

val create :
  target:Component_target.t ->
  recurrence_key:string ->
  alarm_index:int ->
  alarm_key:string ->
  fire_time:Ptime.t ->
  t
