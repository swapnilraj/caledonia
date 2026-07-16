# Machine output version 1

`caled list`, `caled search`, and `caled show` share the formats below. Machine
formats never contain terminal colour or status prose. A command error is
written to standard error and exits nonzero; it is not encoded as an empty
successful result.

## JSON

JSON output is one array. An empty result is exactly `[]`. Each member is an
object with these required discriminator fields:

```json
{"schema_version":1,"component_type":"event"}
```

`component_type` is `event`, `todo`, or `journal`. Every object also contains:

- `identity`: `calendar_key`, `calendar_display_name`, source `file`, `uid`,
  nullable typed `recurrence_id`, and nullable `source_fingerprint`;
- nullable `summary` and `description`, a `categories` array, nullable typed
  `start`, nullable `status`, nullable structured `recurrence`, a canonical
  `recurrence_set` array, and an `alarms` array;
- nullable type-specific fields: `end`, `location`, `due`, `priority`,
  `percent_complete`, `completed`, and `parent`.

A calendar time is an object with `kind` and `value`. `kind` is `date`, `utc`,
`floating`, or `tzid`; `tzid` values also carry the exact `tzid`. `end`
distinguishes `dtend` from an iCalendar `duration` in integral seconds. Event
DTEND values and event/VTODO DURATION values use this same typed `end` shape.
VEVENT `recurrence` is the structured RRULE when one exists. `recurrence_set`
contains the complete canonical, unfolded `RRULE`, `RDATE`, and `EXDATE`
content lines, including RDATE-only schedules and exact VALUE/TZID parameters.
VEVENT RRULEs preserve the frequency, count/until limit, interval, and every
supported rule part on recurrence masters (for example, `show`). A
date-constrained `list` or `search` expands recurring events only inside the
requested half-open range, with a 100,000-instance work limit. Those expanded
instances are finite records: each carries a typed
`identity.recurrence_id`, has a null `recurrence` and empty `recurrence_set`,
and preserves the DTSTART temporal kind, wall clock, and TZID in that
recurrence identity. Alarms
preserve their array index, action, absolute or
relative trigger (including START/END relation), repeat information, summary,
action-specific fields, attachments, and extension properties. Binary alarm
attachments retain their RFC base64 payload and declare `encoding: "base64"`.
Every parameterized alarm value also carries a `parameters` array of exact
serialized name/value pairs; summaries, descriptions, repeat fields, extension
properties, attachments, and structured email attendees retain their own
parameter arrays. Each component includes `ics`, a canonical single-component
iCalendar representation that preserves supported properties and parameters
not promoted to dedicated JSON fields.

Recurring VTODO and VJOURNAL components are not emitted as partially expanded
results: loading them fails with an explicit unsupported-capability error.
`RECURRENCE-ID;RANGE=THISANDFUTURE` is likewise rejected. Recurring VEVENT,
exact-instance overrides, EXDATE, and RDATE are supported.

A `search` with no `--from`, `--to`, or date shortcut is a genuinely
date-unbounded component search. It returns every matching non-recurring
component and each matching recurring VEVENT master exactly once, including
components dated outside the recurrence expansion range of any practical
agenda. It does not expand recurrence instances: an unbounded recurrence can
be infinite. Consequently, such a recurring result has a null
`identity.recurrence_id` and retains its structured `recurrence`; add an
explicit date constraint when occurrence records are required.

Consumers must reject unsupported `schema_version` values and may ignore new
fields added to version 1. JSON and S-expression output are not affected by the
presentation timezone option: floating and TZID wall-clock values are preserved
as authored instead of being silently converted.

`caled alarms --format json` also emits one array. Each object has
`schema_version`, `fire_time` (RFC 3339), `trigger`, nullable `summary`,
`calendar`, stable `calendar_key`, and `component_id`. An empty alarm result is
exactly `[]`.
Component and alarm JSON ends at the closing `]`; the CLI does not append a
presentation newline.

## CSV

CSV uses RFC 4180 records, commas, CRLF line endings, doubled quotes, and a
header even when no rows match. The fixed version-1 header is:

```text
schema_version,component_type,id,calendar,summary,start,details_json
```

`start` is an RFC 3339 UTC value, ISO date, or exact floating/TZID wall-clock
value when present. `details_json` contains the complete versioned JSON object
so values not represented by the flat columns remain lossless. Output always
ends at a CRLF record boundary, including the header-only empty result.

## iCalendar

ICS output is one `VCALENDAR` stream containing all selected components and
the required timezone definitions. It is never a concatenation of independent
calendar envelopes. Because RFC 5545 requires a calendar component, an empty
selection produces an empty stream. A nonempty stream ends at the serializer's
CRLF record boundary; the CLI does not append another LF. Expanded recurrence
instances are emitted as finite VEVENT siblings with the same UID and distinct
typed `RECURRENCE-ID` values; they do not retain RRULE, EXDATE, or RDATE.
When the selected object is a persisted recurrence master, ICS output (including
the per-record JSON `ics` field) contains its complete authored same-UID series:
the master followed by its retained RECURRENCE-ID overrides. Unrelated source
siblings are excluded. A generated finite occurrence still emits exactly one
VEVENT.
Only definitions referenced by the exact selected components are considered;
unreferenced source VTIMEZONEs are omitted. When selected source documents carry
identical definitions for one referenced TZID, the definition is emitted once.
Conflicting definitions for a referenced TZID are an export error; the command
writes the diagnostic to stderr and emits no partial machine stream. A TZID with
no embedded definition remains authored as-is.

## S-expression

S-expression output is one list of component records serialized by Sexplib.
Each record is a deterministic conversion of the complete versioned JSON
object. Objects use `(object (field value) ...)`; arrays use `(array ...)`; and
scalar values are tagged as `(string ...)`, `(int ...)`, `(integer ...)`,
`(float ...)`, `(bool ...)`, or the atom `null`. This retains the JSON schema's
type distinctions while Sexplib provides standard escaping for quoted strings,
empty strings, backslashes, and embedded newlines. The empty result is exactly
`()`, with no trailing presentation newline.

## Human output and colour

`text` and `entries` are presentation formats, not stable machine schemas.
Untrusted fields are reduced to one terminal row before layout: C0/C1 controls,
tabs, line separators, and bidi or other unsafe format controls are visibly
replaced. The final formatter retains only its own structural LF separators.
Unicode grapheme clusters are measured with a best-effort terminal-width model,
so combining sequences and joined emoji align without deleting ZWJ, ZWNJ, or
emoji tag characters. Trusted colour is applied only after sanitization. Colour
is automatic only on a terminal; `--color` forces it, `--no-color` disables it,
and redirected output is plain by default. Machine formats preserve the
underlying string data and rely on their standard JSON, CSV, or Sexplib escaping
rather than presentation sanitization.
