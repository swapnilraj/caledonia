(** Immutable decoded snapshot of one physical VCALENDAR. This is the sole owner
    of the codec document and its compatibility/opaque metadata. *)

type t

val decode :
  source:Component_source.t ->
  Calendar_codec.t ->
  (t, [> `Msg of string ]) result

val parse :
  source:Component_source.t -> string -> (t, [> `Msg of string ]) result

val source : t -> Component_source.t
val components : t -> Component.t list

val known_entries_of_body :
  Component.body ->
  (Calendar_codec.known_entry list, [> `Msg of string ]) result
(** Validate a writable domain body at the codec boundary while restoring its
    explicit semantic compatibility metadata. *)

val find :
  t -> Component_identity.t -> (Component.t, [> `Msg of string ]) result

val replace :
  t ->
  target:Component_identity.t ->
  replacement:Component.body ->
  (t, [> `Msg of string ]) result

val delete : t -> target:Component_identity.t -> (t, [> `Msg of string ]) result
val serialize : ?cr:bool -> t -> string
val has_entries : t -> bool

val timezone_entries : t -> Calendar_codec.known_entry list
(** Exact VTIMEZONE codec entries authored in this physical snapshot, including
    compatibility and nested opaque preservation metadata. *)
