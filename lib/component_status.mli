(** Shared text codec for the upstream RFC 5545 status algebra. *)

val bindings : (string * Icalendar.status) list
val to_string : Icalendar.status -> string
val of_string : string -> (Icalendar.status, [> `Msg of string ]) result
