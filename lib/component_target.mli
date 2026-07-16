(** Complete immutable target for a stored component mutation. *)

type t

val create : source:Component_source.t -> identity:Component_identity.t -> t
val source : t -> Component_source.t
val identity : t -> Component_identity.t
