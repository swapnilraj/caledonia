val validate : Icalendar.alarm -> (unit, [> `Msg of string ]) result
(** Validate the complete typed alarm domain: RFC 5545 action-specific fields,
    repeat/duration and trigger coupling, attachments, attendees, parameters,
    and IANA/X extensions. *)

val validate_all : Icalendar.alarm list -> (unit, [> `Msg of string ]) result
val max_binary_attachment_bytes : int
val max_uri_bytes : int
val max_content_line_value_bytes : int

val validate_binary_attachment : string -> (unit, [> `Msg of string ]) result
(** Validate a non-empty, canonically padded RFC 4648 base64 payload without
    allocating its decoded representation. *)

val validate_uri : field:string -> string -> (unit, [> `Msg of string ]) result
(** Validate a bounded, absolute ASCII RFC 3986 URI, including percent escapes
    and its scheme. *)

val validate_token :
  field:string -> string -> (unit, [> `Msg of string ]) result
(** Validate an RFC 5545 IANA token. *)

val validate_content_line_value :
  field:string -> string -> (unit, [> `Msg of string ]) result
(** Validate a bounded UTF-8 raw content-line value. Tabs are allowed; line
    breaks, NUL, and other control characters are rejected. *)

val validate_params : Icalendar.params -> (unit, [> `Msg of string ]) result
(** Validate typed alarm parameters, including URI-bearing values, extension
    names, and raw parameter text. *)

val validate_other_property :
  Icalendar.other_prop -> (unit, [> `Msg of string ]) result
(** Validate a typed IANA or X alarm extension without serialization. *)

val trigger :
  Icalendar.alarm ->
  Icalendar.params * [ `Duration of Ptime.Span.t | `Datetime of Ptime.t ]
(** Return the trigger independently of the alarm action. In particular,
    [ACTION:NONE] still has complete scheduling semantics. *)

val validate_references :
  has_start:bool ->
  has_end:bool ->
  Icalendar.alarm list ->
  (unit, [> `Msg of string ]) result
(** Enforce the component fields required by relative triggers. For VTODO, pass
    [has_end] according to DUE; for VEVENT, according to DTEND/DURATION. *)

val repeated_spans :
  ?max_repetitions:int ->
  Ptime.Span.t ->
  Icalendar.alarm ->
  (Ptime.Span.t list, [> `Msg of string ]) result
(** Return the initial relative offset followed by each RFC 5545 repeat. *)

val repeated_instants :
  ?max_repetitions:int ->
  Ptime.t ->
  Icalendar.alarm ->
  (Ptime.t list, [> `Msg of string ]) result
(** Return the initial fire time followed by each RFC 5545 repeat. *)

type 'owner fire = {
  fire_time : Ptime.t;
  owner : 'owner;
  alarm : Icalendar.alarm;
  alarm_index : int;
}
(** One scheduled alarm occurrence. The owner type is supplied by the domain
    calculation and attached to storage/query context only at its boundary. *)
