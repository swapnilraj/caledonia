(** Immutable source metadata for a component loaded from a physical calendar
    document. Draft bodies deliberately have no value of this type. *)

type t

val of_decoded_document :
  calendar_key:string ->
  ?display_name:string ->
  file:Eio.Fs.dir_ty Eio.Path.t ->
  fingerprint:string ->
  unit ->
  t
(** Decoder-only construction seam. Repository/document loading supplies a
    fingerprint derived from the physical bytes; domain draft construction must
    not call this function. *)

val calendar_key : t -> string

val display_name : t -> string
(** The effective presentation name. Construction applies [calendar_key] as the
    fallback, so persisted sources do not carry optional name state. *)

val file : t -> Eio.Fs.dir_ty Eio.Path.t
val fingerprint : t -> string
val equal : t -> t -> bool
