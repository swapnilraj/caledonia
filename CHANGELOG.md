
### 0.5.0

- Make each physical VCALENDAR an abstract immutable document snapshot. Domain
  event, todo, and journal bodies are source-free; stored views carry one shared
  non-optional source, derive identity from the immutable body, and repository
  writes return values reloaded from the installed bytes.
- Separate authored event series, generated occurrences, and typed occurrence
  references. Bounded queries retain the exact stored series beside each
  occurrence, and occurrence mutation no longer accepts an unchecked timestamp.
- Move query, formatting, S-expression, and ICS presentation out of the domain
  modules. Production now has one component kind, identity, patch, query-item,
  and generic alarm-fire model.
- Complete a second model-simplicity pass: authored event series now have one
  public name (`Event.t`), authored temporal values use the canonical
  `Icalendar.date_or_datetime` algebra directly, document identities are
  derived rather than stored in a parallel index, source display names are
  total, and CLI sorting uses the query model without an adapter type. Remove
  the duplicate `Event.edit` and checked todo-graph aliases from the pre-1.0
  library API.
- Complete a whole-diff third audit: domain mutation clocks are explicit, UUID
  generation is process-wide, calendar/component projections are derived,
  alarm fires use the query-item model directly, the daemon core is
  presentation-free, protocol event responses use one internal payload, and
  shared CLI/property/lock utilities replace identical implementations.
- Fix whole-series Emacs edits from occurrences by retaining the stored target
  in `series_master`, and return a typed range error for maximum civil DATE
  overdue checks instead of raising.
- Contain ordered opaque entries, repeated alarms, DATE-valued RRULE metadata,
  and writer corrections behind the abstract codec/document boundary. The
  legacy augmented-calendar adapter is test-only and rejected in production by
  the architecture gate.
- Preserve complete calendar documents during transactional component edits.
- Add explicit patch semantics for setting, clearing, or retaining fields.
- Add server protocol version 1 with bounded request frames, strict correlated
  request IDs, structured errors, a handshake, and lossless Emacs edits.
- Enforce one alarm-validation boundary for protocol, library, and loaded ICS
  data, including RFC action fields, exact parameter bindings, bounded base64,
  absolute URIs, per-property parameter applicability, whole-second triggers
  and repeat intervals, and safe IANA/X extensions.
- Make JSON, CSV, ICS, and S-expression output valid for empty result sets.
- Make date-unbounded search return every matching component or recurring
  series master once; occurrence expansion now happens only for explicit date
  ranges and remains protected by its deterministic work limit.
- Validate the complete RFC 5545 VEVENT recurrence domain at create, edit, and
  load boundaries, including RRULE cardinality/ranges/cross-part constraints,
  UNTIL typing, and EXDATE, RDATE, and RECURRENCE-ID parameter applicability.
- Synchronize authored same-UID override series during master edits: ordinary
  edits preserve valid overrides, explicit recurrence replacement/clear removes
  them, and orphan or recurrence-bearing exact overrides are rejected.
- Require the submitted snapshot fingerprint for every existing-component and
  occurrence mutation; missing fingerprints now conflict without borrowing a
  newer cached snapshot.
- Validate status filters against selected component types and reject the
  unavailable event-protocol `overdue` filter as `unsupported_capability`.
- Reject malformed registered VEVENT, VTODO, and VJOURNAL properties that the
  pinned parser demotes to generic IANA extensions, while retaining unknown
  extension names and its narrowly documented property aliases.
- Introduce machine-output schema version 1. JSON and tagged S-expression
  records now expose typed component/temporal fields, while CSV carries the
  complete v1 JSON record in `details_json`; consumers must migrate from the
  pre-0.5 ad-hoc shapes.
- Make the alarm daemon use recursive inotify on Linux with a portable polling
  fallback, transactional watch-set rebuilds, reported/retried partial-tree
  failures, reconciliation scans, and persisted missed-alarm recovery.
- Align the default calendar directory at `~/.calendar/` and install the Emacs
  frontend with the package.
- Raise the minimum OCaml version to 5.1 and declare direct build/test
  dependencies. The pinned iCalendar library is now an unconditional runtime
  dependency, and the time layer builds against both Timedesc 2.x and 3.x.

### 0.4.0

- Emacs front end that communicates with a server mode via an S-expression protocol

### 0.3.1

- various bugfixes and tweaks

### 0.3.0

- timezone support

### 0.2.0

- show, add, delete, and edit commands

### 0.1.0

- list and search commands
