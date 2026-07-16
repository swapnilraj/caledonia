(** Terminal and machine-output boundary for CLI commands. *)

type color_policy = [ `Auto | `Always | `Never ]

val color_enabled : color_policy -> bool

val print_terminal_stdout_line : string -> unit
(** Print one terminal-safe line to standard output. *)

val print_terminal_stderr_line : string -> unit
(** Print one terminal-safe line to standard error. *)

val print_error : string -> string -> unit
(** [print_error label message] prints a terminal-safe labelled diagnostic. *)

val validate_human_items :
  tz:Timedesc.Time_zone.t ->
  Caledonia_lib.Component_query.item list ->
  (unit, [ `Msg of string ]) result
(** Validate conversions that human output requires before printing anything. *)

val print_items :
  documents:Caledonia_lib.Calendar_document.t list ->
  format:[ `Text | `Entries | `Json | `Csv | `Ics | `Sexp ] ->
  tz:Timedesc.Time_zone.t ->
  ?now:Ptime.t ->
  ?get_color:(string -> string option) ->
  color:color_policy ->
  Caledonia_lib.Component_query.item list ->
  (unit, [ `Msg of string ]) result
(** Serialize and print stored components and nominal query occurrences. ICS and
    embedded machine-format ICS use the supplied immutable document snapshots
    for exact timezone context. *)
