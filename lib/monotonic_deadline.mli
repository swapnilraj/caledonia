(** Small injected-clock deadlines for code which must not observe wall-clock
    corrections. *)

type t

val after : now:(unit -> float) -> float -> t
(** [after ~now seconds] returns a deadline [seconds] after the clock's current
    reading. [seconds] must be non-negative. *)

val reached : now:(unit -> float) -> t -> bool
(** [reached ~now deadline] compares [deadline] with the same monotonic clock
    used to create it. *)
