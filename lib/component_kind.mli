(** The one shared kind of writable calendar component. *)

type t = Event | Todo | Journal [@@deriving sexp]

val equal : t -> t -> bool
val to_string : t -> string
