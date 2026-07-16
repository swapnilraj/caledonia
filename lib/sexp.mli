(** Stable S-expression protocol-v1 boundary.

    The concrete request types are public because clients construct and decode
    protocol frames directly. Wire serialization remains versioned by the
    request and response envelopes below; parsing and presentation helpers used
    only to implement that format stay private to {!Sexp}. *)

type search_field = Summary | Description | Location | Categories
[@@deriving sexp]

type query_request = {
  from : string option; [@sexp.option]
  to_ : string;
  timezone : string option; [@sexp.option]
  calendars : string list; [@default []]
  text : string option; [@sexp.option]
  search_in : search_field list; [@default []]
  categories : string list; [@default []]
  id : string option; [@sexp.option]
  statuses : string list; [@default []]
  overdue : bool option; [@sexp.option]
  has_alarm : bool option; [@sexp.option]
  recurring : bool option; [@sexp.option]
  limit : int option; [@sexp.option]
}
[@@deriving sexp]

type time_kind = Date | Utc | Floating | Tzid of string [@@deriving sexp]

type calendar_time_input = { kind : time_kind; value : string }
[@@deriving sexp]

type event_end_input = Dtend of calendar_time_input | Duration_seconds of int
[@@deriving sexp]

type alarm_action = Audio | Display | Email | None_action [@@deriving sexp]
type alarm_relation = Start | End [@@deriving sexp]

type alarm_trigger =
  | Relative of { seconds : int; related : alarm_relation }
  | Absolute of string
[@@deriving sexp]

type alarm_attachment = Uri of string | Binary of string [@@deriving sexp]
type alarm_parameter = { name : string; value : string } [@@deriving sexp]

type alarm_attendee = { uri : string; parameters : alarm_parameter list }
[@@deriving sexp]

type alarm_other_property =
  | Iana of { name : string; value : string; parameters : alarm_parameter list }
  | X of {
      namespace : string;
      name : string;
      value : string;
      parameters : alarm_parameter list;
    }
[@@deriving sexp]

type alarm_input = {
  action : alarm_action;
  trigger : alarm_trigger;
  trigger_parameters : alarm_parameter list; [@default []]
  repeat : int option; [@sexp.option]
  duration_seconds : int option; [@sexp.option]
  duration_parameters : alarm_parameter list; [@default []]
  repeat_parameters : alarm_parameter list; [@default []]
  summary : string option; [@sexp.option]
  summary_parameters : alarm_parameter list; [@default []]
  description : string option; [@sexp.option]
  description_parameters : alarm_parameter list; [@default []]
  attendees : string list; [@default []]
  attendee_values : alarm_attendee list; [@default []]
  attachment : alarm_attachment option; [@sexp.option]
  attachment_parameters : alarm_parameter list; [@default []]
  other : alarm_other_property list; [@default []]
}
[@@deriving sexp]

type recurrence_input = { rrule : string } [@@deriving sexp]

type create_event_request = {
  calendar : string;
  summary : string;
  start : calendar_time_input;
  end_ : event_end_input option; [@sexp.option]
  location : string option; [@sexp.option]
  description : string option; [@sexp.option]
  categories : string list; [@default []]
  recurrence : recurrence_input option; [@sexp.option]
  alarms : alarm_input list; [@default []]
}
[@@deriving sexp]

(** The protocol re-exports the domain patch algebra with protocol-v1 codecs; it
    does not define a competing update type. *)
type 'a patch = 'a Patch.t = Keep | Clear | Set of 'a [@@deriving sexp]

type edit_event_request = {
  id : string;
  calendar_key : string;
  file : string;
  source_fingerprint : string option; [@sexp.option]
  summary : string patch; [@default Keep]
  start : calendar_time_input patch; [@default Keep]
  end_ : event_end_input patch; [@default Keep]
  location : string patch; [@default Keep]
  description : string patch; [@default Keep]
  categories : string list patch; [@default Keep]
  recurrence : recurrence_input patch; [@default Keep]
  alarms : alarm_input list patch; [@default Keep]
  occurrence_start : string option; [@sexp.option]
  occurrence_timezone : string option; [@sexp.option]
}
[@@deriving sexp]

type delete_event_request = {
  id : string;
  calendar_key : string;
  file : string;
  source_fingerprint : string option; [@sexp.option]
  occurrence_start : string option; [@sexp.option]
  occurrence_timezone : string option; [@sexp.option]
}
[@@deriving sexp]

type request =
  | Handshake
  | ListCalendars
  | Query of query_request
  | Refresh
  | CreateEvent of create_event_request
  | EditEvent of edit_event_request
  | DeleteEvent of delete_event_request
[@@deriving sexp]

type response_payload =
  | Hello of {
      protocol_version : int;
      server_version : string;
      capabilities : string list;
    }
  | Calendars of string list
  | Events of {
      events : Component_query.item list;
      occurrence_timezone : string option;
      documents : Calendar_document.t list;
    }
  | Empty

type protocol_error = { code : string; message : string; retryable : bool }
[@@deriving sexp_of]

type response = Ok of response_payload | Error of protocol_error
[@@deriving sexp_of]

type request_envelope = {
  version : int;
  request_id : string;
  request : request;
}
[@@deriving sexp]

type response_envelope = {
  version : int;
  request_id : string;
  response : response;
}
[@@deriving sexp_of]

type wire_request = Request of request_envelope [@@deriving sexp]
type wire_response = Response of response_envelope [@@deriving sexp_of]

val protocol_version : int
val protocol_error : ?retryable:bool -> code:string -> string -> protocol_error
val valid_request_id : string -> bool

val parse_wire_request :
  Sexplib.Sexp.t -> (request_envelope, string * protocol_error) result

val wire_response : request_id:string -> response -> wire_response

val generate_query_params :
  now:Ptime.t ->
  query_request ->
  ( Component_query.criteria
    * Ptime.t option
    * Ptime.t
    * int option
    * Timedesc.Time_zone.t,
    [ `Msg of string ] )
  result

val event_wire_sexp : ?occurrence_timezone:string -> Event.t -> Sexplib.Sexp.t
(** Serialize source-independent event fields. Physical source fields and
    [source_ics] require immutable document context and are added by
    {!stored_event_wire_sexp} or response serialization. *)

val stored_event_wire_sexp :
  documents:Calendar_document.t list ->
  ?occurrence_timezone:string ->
  Component.t ->
  Sexplib.Sexp.t

val sexp_of_response_payload : response_payload -> Sexplib.Sexp.t
