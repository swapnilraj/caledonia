# Caledonia server protocol version 1

`caled server` reads and writes one UTF-8 S-expression per line. Standard
output contains protocol frames only; diagnostics are written to standard
error. Strings use Sexplib escaping, including empty strings, quotes,
backslashes, and newlines.

## Envelope and handshake

Every client frame carries an integer version, a request ID, and one request
payload. The complete UTF-8 request and terminating newline must fit within the
1,000,000-byte request-line limit. A request ID is 1–256 bytes of valid printable
UTF-8; control, format, surrogate, unassigned, and malformed code points are
rejected. Response frames are also limited to 1,000,000 UTF-8 bytes so every
valid server response fits the official Emacs client's frame bound. If a query
would exceed that ceiling, the server returns a small correlated
`response_too_large` error; clients should narrow the range or set `limit`.

```lisp
(Request
 ((version 1)
  (request_id "client-42")
  (request Handshake)))
```

The server correlates every response with the same ID:

```lisp
(Response
 ((version 1)
  (request_id "client-42")
  (response
   (Ok
    (Hello
     ((protocol_version 1)
      (server_version "0.5.0")
      (capabilities (event-query event-create event-edit-patch
                     event-delete occurrence-edit occurrence-delete)))))))))
```

`Handshake` must be the first request on each connection. A version mismatch
returns `unsupported_version`; sending another request first returns
`handshake_required`; an invalid request ID or a second handshake returns
`invalid_request`. Syntax, UTF-8, and line-limit errors use a valid ID recovered
from the bounded request prefix when that can be done safely; otherwise their
response ID is `unknown`. An oversized line is drained before the next request.

Errors are structured and do not masquerade as successful payloads:

```lisp
(Response
 ((version 1)
  (request_id "client-43")
  (response
   (Error
    ((code conflict)
     (message "calendar file changed since it was read")
     (retryable true))))))
```

Known codes include `invalid_request`, `not_found`, `ambiguous_identity`,
`conflict`, `handshake_required`, `unsupported_version`, and `internal_error`.
`retryable` is true only when reloading fresh state and retrying is meaningful.

## Calendar time and event end

Calendar time is never transported as a presentation string. It is one of:

```lisp
((kind Date)     (value "2026-07-15"))
((kind Utc)      (value "2026-07-15T12:30:45"))
((kind Floating) (value "2026-07-15T12:30:45"))
((kind (Tzid "Europe/London")) (value "2026-07-15T12:30:45"))
```

Seconds are required for lossless round trips. An event end distinguishes an
exclusive `DTEND` from `DURATION`:

```lisp
(Dtend ((kind Date) (value "2026-07-16")))
(Duration_seconds 3600)
```

For all-day events, the transported `DTEND` remains RFC 5545's exclusive date.

## Patch values

Every editable optional field uses one of these values:

```lisp
Keep
Clear
(Set VALUE)
```

Omitting or reopening an unchanged form therefore cannot clear or reconstruct
a value accidentally. `Clear` is distinct from an empty string.

Event mutations require full source identity: `calendar_key`, physical `file`,
`id` (UID), and snapshot `source_fingerprint`. Targeting one recurrence also
requires the paired `occurrence_start` and `occurrence_timezone` returned by the
query that selected it. Display names are never accepted as write identifiers.
A stale fingerprint returns a retryable `conflict` instead of overwriting newer
data.

```lisp
(Request
 ((version 1)
  (request_id "edit-1")
  (request
   (EditEvent
    ((id "event-uid")
     (calendar_key "personal")
     (file "/home/me/.calendar/personal/event.ics")
     (source_fingerprint "opaque-snapshot-token")
     (summary Keep)
     (start Keep)
     (end_ Keep)
     (location Clear)
     (description (Set "Bring documents"))
     (recurrence Keep)
     (alarms Keep)))))))
```

The server resolves this identity from current disk state for every write and
returns a canonical reloaded event carrying the new source fingerprint.
The fingerprint is an opaque equality token. Its algorithm and textual shape
are not protocol API and clients must never parse or synthesize it.

## Structured alarms

Alarm requests preserve action, START/END relation, signed seconds, absolute
triggers, repeat/duration, action-specific fields, binary or URI attachments,
attendee parameters, and IANA/X extension properties:

```lisp
((action Display)
 (trigger (Relative ((seconds -901) (related End))))
 (repeat 2)
 (duration_seconds 30)
 (description "Reminder"))

((action Email)
 (trigger (Absolute "2026-07-15T10:00:00Z"))
 (summary "Agenda reminder")
 (description "Reminder")
 (attendee_values
  (((uri "mailto:user@example.com")
    (parameters (((name "ROLE") (value "REQ-PARTICIPANT")))))))
 (attachment (Uri "https://example.test/agenda"))
 (attachment_parameters
  (((name "FMTTYPE") (value "application/pdf"))))
 (other
  ((X ((namespace "TRACE") (name "ID") (value "alarm-42")
       (parameters (((name "X-SCOPE") (value "private")))))))))

((action Audio)
 (trigger (Relative ((seconds -60) (related Start))))
 (attachment (Binary "AAEC/w=="))
 (attachment_parameters
  (((name "ENCODING") (value "BASE64"))
   ((name "VALUE") (value "BINARY")))))
```

Actions are `Audio`, `Display`, `Email`, and `None_action`. Relative alarm
seconds are signed: negative values fire before the related boundary. Every
parameter is represented explicitly as a canonical
`((name NAME) (value VALUE))` record;
`trigger_parameters`, `duration_parameters`, `repeat_parameters`,
`summary_parameters`, and `description_parameters` use the same list shape.
Parameter values must be one RFC 5545 parameter value or comma-separated value
list, using quotes where semicolons, colons, or commas are data. Control bytes,
unbalanced quotes, and unquoted structural delimiters are rejected. The server
verifies that parsing produces exactly the submitted name/value bindings, so a
value cannot smuggle an additional parameter into the synthetic validation
content line.

Standard parameters are also checked against the property on which they occur:
`TRIGGER` permits `VALUE` and `RELATED`; `DESCRIPTION` and `SUMMARY` permit
`ALTREP` and `LANGUAGE`; `ATTACH` permits `FMTTYPE`, `ENCODING`, and `VALUE`;
and `ATTENDEE` permits its RFC 5545 delegation, participation, directory,
language, and display-name parameter set. `DURATION` and `REPEAT` permit only
IANA/X extension parameters. IANA/X parameters remain available on every alarm
property, but cannot shadow a registered standard parameter.

`DISPLAY` requires `description` and prohibits `summary`, attachments, and
attendees. `AUDIO` prohibits `summary`, `description`, and attendees, and may
carry one attachment. `EMAIL` requires non-empty `summary`, `description`, and
at least one attendee. `None_action` prohibits action-specific fields including
`summary`. The legacy `attendees` list and structured `attendee_values` list are
aliases and cannot both be supplied in one alarm.
The server validates action constraints and required parameter/value-kind
pairs before constructing or serializing an alarm. Binary attachment values
use the standard RFC 4648 alphabet with canonical padding, are non-empty, and
are limited to 750,000 encoded bytes. They remain binary RFC 5545 `ATTACH`
values on disk. URI attachments and attendees must be absolute ASCII RFC 3986
URIs with a valid scheme and percent escapes and are limited to 8,192 bytes;
raw whitespace, controls, and unescaped non-ASCII bytes are rejected. The same
URI checks apply to URI-valued alarm parameters such as `ALTREP`, `DIR`,
`SENT-BY`, `MEMBER`, and delegation parameters. URI attachments may specify
`VALUE=URI` but not `ENCODING`; binary attachments require `VALUE=BINARY` and
`ENCODING=BASE64` when those parameters are explicit.

IANA property names and both parts of an X property name use only letters,
digits, and hyphens, with a 128-byte bound on the serialized name. The IANA
variant cannot shadow a standard `VALARM` property or carry an `X-` name.
Extension values are valid UTF-8, limited to 65,536 bytes, and cannot contain
line breaks, NUL, or other content-line control characters. Invalid alarm
input returns `invalid_request` before any calendar mutation; failed edit
requests leave the source file byte-for-byte unchanged.

Absolute trigger input accepts RFC 3339 `Z` and positive or negative numeric
offsets. Numeric offsets are normalized to the equivalent UTC instant before
RFC 5545 serialization because RFC 5545 DATE-TIME does not permit numeric
offset spellings. Inputs must use whole seconds because RFC 5545 DATE-TIME has
no fractional-second form. Seconds and the instant are preserved; the original
offset spelling is intentionally not an ICS round-trip invariant.

## Other requests

After the handshake, supported payloads are:

- `ListCalendars`
- `(Query QUERY-RECORD)`
- `(CreateEvent CREATE-RECORD)`
- `(EditEvent PATCH-RECORD)`
- `(DeleteEvent IDENTITY-RECORD)`
- `Refresh`, retained as a no-op for version-1 clients; reads already reload
  current disk state automatically.

Query responses use `(Events (...))`; calendar responses use
`(Calendars (...))`; mutations without a value use `Empty`. Event response
records include structured `start_value`, `end_value`, and `alarms_value`, plus
`categories_value`, `recurrence_value`, `recurrence_set_value`, `calendar_key`,
`source_fingerprint`, `file`, and `source_ics` for semantic inspection. For a
persisted recurring master, `source_ics` is a canonical VCALENDAR containing the
master and all retained same-UID RECURRENCE-ID overrides, but no unrelated
source siblings. It also contains every referenced VTIMEZONE definition and
rejects conflicting definitions using the same rules as CLI ICS export. Because
the version-1 response shape cannot fail during event serialization, that rare
conflict is represented explicitly as an empty `source_ics` plus a
`source_ics_error` field. A generated finite occurrence's `source_ics` contains
only that occurrence. Query responses calculate
`start_local` and `end_local` in the query's explicit timezone, independent of
the server process timezone.

Generated and persisted recurrence instances additionally include
`is_occurrence true`, typed `recurrence_id_value`, exact RFC 3339
`occurrence_start`, `occurrence_timezone`, and a canonical `series_master`
record. Clients use the occurrence record for “this occurrence” forms and the
master record for “all occurrences” forms; they must not synthesize occurrence
identity from presentation fields such as `start_local`.

`recurrence_value` is the editable structured RRULE when one exists.
`recurrence_set_value` is the complete list of canonical, unfolded `RRULE`,
`RDATE`, and `EXDATE` content lines, so an RDATE-only series is never presented
as non-recurring. The Emacs form displays this full set read-only and exposes a
separate explicit clear action; changing the RRULE replaces the displayed set.

Version 1 server queries are intentionally event-only, advertised by the
`event-query` capability. Clients must not infer TODO or journal support from
the CLI's broader component support.

## Query record

The exact query fields are:

```lisp
((from "2026-07-01")                 ; optional inclusive local date
 (to "2026-07-31")                  ; required inclusive local date
 (timezone "Europe/London")         ; optional IANA name
 (calendars (personal work))         ; stable calendar keys
 (text "planning")                  ; optional case-insensitive text
 (search_in (Summary Description Location Categories))
 (categories (project-x))
 (id "event-uid")                   ; optional exact UID
 (statuses (tentative confirmed))    ; optional VEVENT statuses
 (has_alarm true)                    ; optional
 (recurring true)                    ; optional
 (limit 50)))                        ; optional non-negative bound
```

Fields annotated optional are omitted, rather than sent with a null atom.
Unmentioned list fields default to `()`. The server interprets `to` as a local
calendar date and constructs an exclusive boundary at the following local
midnight, including DST transitions correctly.

Event-query statuses are limited to `tentative`, `confirmed`, and `cancelled`;
unknown or component-inapplicable tokens return `invalid_request`. The legacy
`overdue` field is not available for the event-only v1 query capability and, if
sent with either boolean value, returns `unsupported_capability` rather than an
empty success.

## Create, edit, and delete grammar

`CreateEvent` requires `calendar`, `summary`, and structured `start`. Optional
fields are `end_`, `location`, `description`, `categories`, `recurrence`, and
`alarms`. Recurrence is `((rrule "FREQ=..."))`; the value is validated as one
unfolded RFC 5545 RRULE content-line value.

`EditEvent` requires the identity fields shown above. Patchable fields are
`summary`, `start`, `end_`, `location`, `description`, `categories`,
`recurrence`, and `alarms`; omitted patches default to `Keep`.
`occurrence_start`, when supplied, is the exact RFC 3339 recurrence instance
returned by `Query`; `occurrence_timezone` is required with it and identifies
the local calendar interpretation used by that query. One-occurrence edits
reject recurrence-rule changes and values that RFC 5545 overrides cannot
represent. Every other event patch retains its normal `Keep`, `Clear`, or `Set`
meaning, including end/duration, categories, and alarms.

`DeleteEvent` contains `id`, `calendar_key`, `file`, `source_fingerprint`, and
optional paired `occurrence_start` and `occurrence_timezone`. A whole-series
delete omits both occurrence fields and atomically removes the recurrence master
and all retained same-UID RECURRENCE-ID overrides from its physical VCALENDAR.
Unrelated components and opaque siblings in that document are preserved. An
occurrence delete remains exact. Supplying only one occurrence field is an
invalid request.

Every response is exactly one line. Request IDs are opaque 1–256-byte printable
UTF-8 strings under the envelope policy above; clients must correlate responses
by ID and must not depend on response order.
