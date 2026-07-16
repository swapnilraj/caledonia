(** Testable delivery, retry, and persisted-state core for [alarm-daemon]. *)

type state
type watcher_event = Timer | Changed | Overflow | Watcher_error of string

type driver = {
  now : unit -> Ptime.t;
  wait : timeout:float -> watcher_event;
  sleep : float -> unit;
  load_state : unit -> state option;
  save_state : state -> (unit, [ `Msg of string ]) result;
  load_fires :
    from:Ptime.t ->
    to_:Ptime.t ->
    (Alarm_query.fire list, [ `Msg of string ]) result;
  notify : Alarm_query.fire -> (unit, [ `Msg of string ]) result;
  report : string -> unit;
}

val empty_state : watermark:Ptime.t -> state
val watermark : state -> Ptime.t
val fired_count : state -> int
val pending_count : state -> int

val max_delivery_attempts : int
(** A notifier failure is retried across scans and restarts up to this many
    total attempts. The final failure is reported and terminally deduplicated.
*)

val distinct_fire_identity : Alarm_query.fire -> Alarm_query.fire -> bool
(** [distinct_fire_identity left right] is true when the persisted delivery keys
    differ. Alarm ordinal and recurrence identity are part of the key. *)

val state_to_json : state -> Yojson.Safe.t
val state_of_json : Yojson.Safe.t -> state option
val load_state_file : string -> state option
val save_state_file : string -> state -> (unit, [ `Msg of string ]) result

val run :
  ?max_cycles:int ->
  grace:int ->
  rescan_interval:float ->
  driver ->
  (state, [ `Msg of string ]) result
(** Run the delivery loop. Production omits [max_cycles]; deterministic tests
    use it to stop after a finite number of watcher cycles.

    Failed delivery attempt counts are persisted. The watermark advances to the
    exact timestamp of the oldest unresolved fire; [load_fires] must treat its
    [from] bound as inclusive so that fire is retried. Successfully handled
    prefixes are therefore not replayed. Removed pending alarms disappear during
    reconciliation. Watcher failures are reported without terminating the loop;
    the next cycle performs a full scan while the backend retains or rebuilds
    its watch set. After [max_delivery_attempts], an alarm is reported as
    dropped and recorded as terminally handled so a permanent notifier failure
    cannot retain replay state forever. *)
