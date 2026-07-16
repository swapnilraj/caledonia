val sanitize_terminal : string -> string
(** Replace terminal control/format characters with visible question marks,
    while retaining trusted LF separators and Unicode emoji joiners. *)

val sanitize_terminal_line : string -> string
(** Like {!sanitize_terminal}, but also replaces LF so an untrusted value cannot
    create rows or columns in human-oriented output. *)

val format_date : ?tz:Timedesc.Time_zone.t -> Ptime.t -> string
val format_opt : string -> ('a -> string) -> 'a option -> string
val display_width : string -> int
val pad_to_width : ?color:string -> int -> string -> string
val max_width : ('a -> string) -> 'a list -> int
val parse_color : string -> (int * int * int) option
val colorize : ?color:string -> string -> string
val format_alarm_trigger : Ptime.Span.t -> string

(* Format the total alarm trigger independently of its action. *)
val format_alarm_trigger_text : Icalendar.alarm -> string
val format_alarm_short : Ptime.Span.t -> string
val format_alarms : Icalendar.alarm list -> string
val format_alarms_short : Icalendar.alarm list -> string
