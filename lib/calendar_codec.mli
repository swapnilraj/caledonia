type t
(** An ordered, marker-free representation of one physical VCALENDAR.

    Supported child components remain typed [Icalendar] values. Unsupported
    component blocks are explicit opaque entries, in their authored position,
    and parser compatibility metadata is retained privately. *)

type known_entry
type opaque_entry
type entry = Known of known_entry | Opaque of opaque_entry

val create_known : properties:Icalendar.cal_prop list -> known_entry list -> t
(** Construct a marker-free document from entries already validated by
    {!make_known}. *)

val parse_document : string -> (t, string) result
(** Parse a physical VCALENDAR without exposing private parser markers.

    Top-level known and opaque components retain their authored order. Opaque
    blocks nested in a supported component are retained privately and restored
    by {!serialize}. *)

val properties : t -> Icalendar.cal_prop list
(** Marker-free VCALENDAR properties. *)

val entries : t -> entry list
(** Ordered top-level child entries. *)

val component : known_entry -> Icalendar.component
(** The marker-free supported component payload. *)

val rrule_date_untils : known_entry -> Ptime.date option list
(** DATE-valued UNTIL metadata, in the typed component's RRULE traversal order.
    A [None] entry denotes a DATE-TIME RRULE or an RRULE without UNTIL. *)

val make_known :
  ?rrule_date_untils:Ptime.date option list ->
  Icalendar.component ->
  (known_entry, string) result
(** Construct a marker-free known entry. Explicit metadata is required when a
    newly constructed typed RRULE uses DATE UNTIL, because the pinned AST alone
    cannot distinguish it from UTC midnight. *)

val opaque_name : opaque_entry -> string

(* The normalized component name of an opaque entry. *)

val has_opaque_entries : t -> bool

type rewrite = Keep | Delete | Replace of known_entry list

val rewrite_known : t -> f:(known_entry -> rewrite) -> (t, string) result
(** Rewrite supported slots without exposing opaque entries. A replacement's
    first entry occupies the original slot and inherits its nested opaque
    attachments; additional entries follow it. [Replace []] is rejected so
    deletion remains explicit. *)

val serialize : ?cr:bool -> t -> string
(** Serialize through the compatibility writer while preserving ordered opaque
    entries, nested opaque blocks, DATE-valued RRULE UNTIL semantics, repeated
    VALARMs, and canonical registered property names. *)

val parse_event_rrule :
  string ->
  (Icalendar.Params.t * Icalendar.recurrence * Ptime.date option, string) result
(** Parse one unfolded RRULE value into marker-free parameters, its typed
    recurrence, and the semantic distinction for a DATE-valued UNTIL. *)

val canonical_event_recurrence_lines :
  date_until:Ptime.date option ->
  Icalendar.event ->
  (string list, string) result
(** Canonical unfolded RRULE/RDATE/EXDATE lines for a marker-free VEVENT. *)

val parse_content_line_parameters :
  string -> (Icalendar.Params.t * (string * string) list, string) result
(** Parse one semicolon-separated content-line parameter fragment through the
    pinned parser and return both typed parameters and their canonical authored
    name/value spellings. This keeps parser probing inside the codec boundary.
*)

val canonical_parameter_binding : Icalendar.Params.b -> string * string
(** Serialize one typed parameter binding through the pinned writer and return
    its canonical name and encoded value. *)

module Legacy : sig
  val parse : string -> (Icalendar.calendar, string) result
  val to_ics : ?cr:bool -> Icalendar.calendar -> string
  val date_until_of_params : Icalendar.Params.t -> string option
  val has_opaque_components : Icalendar.calendar -> bool

  (** Test/transition-only adapters for the pre-document augmented-calendar
      representation. Production code must use the abstract codec APIs. This
      quarantine is retained only for compatibility characterization fixtures.
  *)
end
[@@deprecated
  "Test-only pre-document compatibility adapter; remove in the next major \
   release after the pinned parser characterization fixtures migrate"]

val canonical_recurrence_id_line :
  Icalendar.params * Icalendar.date_or_datetime -> string
(** Canonical unfolded RECURRENCE-ID content line, byte-compatible with the
    pinned writer used by alarm-state schema v2. *)

val canonical_alarm_block : Icalendar.alarm -> string
(** Canonical unfolded VALARM block, byte-compatible with the pinned writer used
    by alarm-state schema v2. *)
