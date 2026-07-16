type event = Timer | Changed | Overflow
type t

val backend_name : string
val create : string -> (t, [> `Msg of string ]) result

val wait :
  clock:_ Eio.Time.clock ->
  timeout:float ->
  t ->
  (event, [> `Msg of string ]) result

val close : t -> unit

module For_test : sig
  val inject_overflow : t -> unit
  val inject_rebuild_failure : t -> string -> unit
  val inject_nested_rebuild_failure : t -> path:string -> string -> unit
  val watch_count : t -> int
  val generation : t -> int
  val descriptor_open : t -> bool
end
