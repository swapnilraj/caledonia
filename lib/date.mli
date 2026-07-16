(** Explicit, timezone-aware calendar calculations.

    iCalendar DATE and floating values are not instants. Callers must choose the
    timezone in which those values are interpreted whenever an instant is
    required. Unknown TZIDs remain losslessly represented by
    [Icalendar.date_or_datetime] and are reported by conversion functions;
    embedded VTIMEZONE definitions are retained by the component/document layer
    but are not yet evaluated by this module. *)

type conversion_error =
  [ `Unknown_timezone of string
  | `Nonexistent_local_time of string
  | `Ambiguous_local_time of string
  | `Out_of_range of string ]

val validate_date_or_datetime_params :
  property:string ->
  Icalendar.params ->
  Icalendar.date_or_datetime ->
  (unit, [> `Msg of string ]) result
(** Validate the complete RFC parameter/value contract of a DATE or DATE-TIME
    property. DATE requires VALUE=DATE; other VALUE parameters must agree with
    the typed value when present. TZID is valid only for a timezone-bearing
    local DATE-TIME and must agree with its embedded timezone; the parser
    representation where TZID has already been moved into that typed value is
    also valid. Unrelated standard parameters are rejected, while unknown IANA
    and X parameters are retained. *)

val validate_integral_seconds :
  property:string -> Ptime.Span.t -> (unit, [> `Msg of string ]) result
(** Reject spans that the iCalendar serializer cannot represent losslessly as an
    integer machine-sized number of seconds. *)

val validate_duration_params :
  property:string ->
  Icalendar.params ->
  Ptime.Span.t ->
  (unit, [> `Msg of string ]) result
(** Validate DURATION property parameters. RFC DURATION properties have no
    standard parameters; unknown IANA and X extension parameters are retained.
*)

val string_of_conversion_error : conversion_error -> string

val rfc3339_utc : Ptime.t -> string
(** Serialize a known UTC instant with [Z], never RFC 3339's [-00:00]
    unknown-offset marker. *)

val local_timezone : unit -> Timedesc.Time_zone.t
(** Resolve the system timezone for an application boundary such as CLI option
    handling, falling back deterministically to UTC. This is a function, not
    mutable process state; calculations still require an explicit [~tz]. *)

val ptime_of_ical_result :
  floating_tz:Timedesc.Time_zone.t ->
  Icalendar.date_or_datetime ->
  (Ptime.t, conversion_error) result
(** Resolve an authored value to an instant. [floating_tz] is the caller's
    explicit policy for both floating and DATE values. Unknown TZIDs are errors.
    Per RFC 5545, repeated wall times select the first occurrence and
    forward-shift gap times use the UTC offset in force immediately before the
    gap. *)

val compare_ical_time :
  floating_tz:Timedesc.Time_zone.t ->
  Icalendar.date_or_datetime ->
  Icalendar.date_or_datetime ->
  (int, conversion_error) result
(** Compare authored temporal values. DATE, floating, UTC, and two values with
    the same TZID are ordered directly in their shared representation; in
    particular, an unknown/custom same-TZID range does not require timezone
    database resolution. Mixed representations are resolved to instants and can
    return a typed conversion error. *)

val timedesc_to_ptime_result : Timedesc.t -> (Ptime.t, conversion_error) result

val ptime_to_timedesc_result :
  tz:Timedesc.Time_zone.t -> Ptime.t -> (Timedesc.t, conversion_error) result

val ptime_to_timedesc : tz:Timedesc.Time_zone.t -> Ptime.t -> Timedesc.t
(** Deterministic compatibility wrapper with an explicit timezone. New
    calculation paths should use {!ptime_to_timedesc_result}. *)

val start_of_day_result :
  tz:Timedesc.Time_zone.t -> Ptime.t -> (Ptime.t, conversion_error) result

val next_midnight_result :
  tz:Timedesc.Time_zone.t -> Ptime.t -> (Ptime.t, conversion_error) result
(** The exclusive upper bound of the local calendar day containing the input. It
    is intentionally not defined as input plus 24 hours. *)

val today_result :
  tz:Timedesc.Time_zone.t -> now:Ptime.t -> (Ptime.t, conversion_error) result

val add_days_result :
  tz:Timedesc.Time_zone.t ->
  Ptime.t ->
  int ->
  (Ptime.t, conversion_error) result

val add_weeks_result :
  tz:Timedesc.Time_zone.t ->
  Ptime.t ->
  int ->
  (Ptime.t, conversion_error) result

val add_months_result :
  tz:Timedesc.Time_zone.t ->
  Ptime.t ->
  int ->
  (Ptime.t, conversion_error) result

val add_years_result :
  tz:Timedesc.Time_zone.t ->
  Ptime.t ->
  int ->
  (Ptime.t, conversion_error) result

val start_of_week_result :
  tz:Timedesc.Time_zone.t -> Ptime.t -> (Ptime.t, conversion_error) result

val convert_relative_date_formats :
  tz:Timedesc.Time_zone.t ->
  now:Ptime.t ->
  today:bool ->
  tomorrow:bool ->
  week:bool ->
  month:bool ->
  unit ->
  ((Ptime.t * Ptime.t) option, conversion_error) result
(** Return inclusive local-date endpoints for CLI shorthand. The application
    boundary converts the second endpoint exactly once to local next midnight.
*)

val parse_date :
  tz:Timedesc.Time_zone.t ->
  now:Ptime.t ->
  string ->
  [ `To | `From ] ->
  (Ptime.t, [> `Msg of string ]) result
(** Parse an ISO date/partial date or relative expression. [now] is always
    injected, even for absolute inputs, so the API can never consult an ambient
    clock. [`To] returns midnight at the inclusive final local date; callers
    needing a range convert it once with {!next_midnight_result}. *)

val parse_time : string -> (int * int * int, [> `Msg of string ]) result

val parse_date_time :
  tz:Timedesc.Time_zone.t ->
  now:Ptime.t ->
  date:string ->
  time:string ->
  [ `To | `From ] ->
  (Ptime.t, [> `Msg of string ]) result
