(** Typed failures from repository mutations. Boundary adapters are responsible
    for mapping these values to CLI exit codes and protocol-v1 error strings. *)

type t =
  | Conflict of string
  | Missing_target of string
  | Ambiguous_identity of string
  | Invalid_replacement of string
  | Path_violation of string
  | Io of string
  | Invalid_document of string
  | Unsupported of string

val message : t -> string
val is_conflict : t -> bool
