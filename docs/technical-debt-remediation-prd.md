# Caledonia Technical-Debt Remediation PRD

- Status: Complete
- Owner: Caledonia maintainers
- Target: next release from `vibing`
Last updated: 2026-07-15

Follow-up: the behavioral safety requirements in this PRD are complete. The
component/document ownership simplification is implemented and traced in
[`data-model-simplification-prd.md`](data-model-simplification-prd.md). That
follow-up preserves the safety behavior established here while completing the
structural intent of `STO-1`, `DOM-2`, and master/occurrence separation.

## 1. Objective

Make Caledonia safe to use as a read/write RFC 5545 client and establish a
repeatable engineering baseline. The remediation is complete only when edits
cannot silently destroy calendar data, time and recurrence semantics are
explicit and deterministic, machine interfaces round-trip without loss, the
documented installation works on supported platforms, and all acceptance tests
pass from a clean environment.

This PRD converts the repository and AI-commit audit into executable product
requirements. A green version of the existing tests alone is not sufficient:
the existing suite is dominated by happy-path expect tests and does not cover
the destructive and lossy paths identified by the audit.

## 2. Problem statement

Large, horizontally-scoped feature commits added todos, journals, occurrence
editing, alarms, a notification daemon, Emacs forms, and timezone presentation
without first establishing stable storage, identity, time, patch, protocol, or
output abstractions. Follow-up commits repaired visible symptoms but left the
underlying invariants implicit. The resulting failure modes include:

- losing sibling components when editing a todo or journal;
- persisting invalid all-day exclusions;
- corrupting files through in-place writes or concurrent edits;
- targeting the wrong calendar through display-name or UID ambiguity;
- changing valid event fields merely by opening and submitting an Emacs form;
- miscomputing recurrences, alarms, all-day ranges, and timezone conversions;
- emitting invalid or inconsistent JSON, CSV, and ICS;
- missing alarms after daemon downtime or watcher races;
- an unversioned, lossy server protocol;
- an opam package that cannot produce the advertised build on macOS and omits
  direct build/test dependencies;
- no CI, package-install, Emacs, portability, or data-integrity gates.

### 2.1 Commit-history traceability

The audit reviewed the complete `origin/vibing` history, not only the final
working tree. The clusters below explain where debt was introduced and why
later symptom-level fixes did not close the underlying invariants. Git records
the commits under the repository maintainer's author identity; this document
uses “AI-assisted wave” only to describe the development pattern reported for
this branch, not to infer authorship from Git metadata.

| Commit(s) | Feature wave | Debt/invariant traced from the diff |
| --- | --- | --- |
| `c1ce2fb`, `d5dc977` | Todo and journal support | component-local document wrappers, divergent validation/format/query paths, and due-only behavior (`STO-1`, `DOM-*`, `QRY-1`) |
| `aa0af82` | vdir colour metadata | display name, directory key, and presentation colour were coupled (`STO-6`, `OUT-6`) |
| `d79e5eb` | VALARM and notification daemon | fixed alarm horizon, platform-coupled watcher/notifier, no durable recovery or stable dedupe identity (`ALM-*`) |
| `188f2d8`, `4aa85bf` | Server protocol and Emacs frontend | unversioned/lossy S-expressions, stale cache writes, and form reconstruction instead of patches (`PRO-*`, `EMA-*`) |
| `4105f33` | Per-occurrence edit/delete | ambiguous master/override selection, invalid all-day exclusions, duplicate overrides, and unbounded expansion (`REC-*`) |
| `88b5a82`, `8224f97` | Event form recurrence/alarms | optional fields could not distinguish keep from clear; typed temporal and alarm semantics were flattened (`TIM-*`, `PRO-2`, `EMA-1`) |
| `48ed35c`, `4fbf467`, `3d8c528`, `177a95f` | all-day/timezone follow-up fixes | visible timezone symptoms were repaired while ambient mutable timezone and inclusive-day calculations remained (`TIM-2` through `TIM-7`) |
| `a577bd6`, `c6e6026`, `5cf153a`, `97e0d26` | regression/agenda fixes | repeated narrow repairs exposed the absence of shared query, transport, and range contracts (`QRY-1`, `EMA-3`, `EMA-5`) |
| `88c59ee`, `d1ac589` | pinned iCalendar fork | runtime correctness depended on an undocumented development revision (`PKG-3`) |

This mapping is traceability, not blame. Each requirement below is accepted
only through current behavior and regression evidence; reverting an entire
feature wave is not the remediation strategy.

## 3. Guiding invariants

The implementation must preserve these invariants across every code path.

1. **Physical-document preservation**: a component mutation starts from the
   latest complete VCALENDAR on disk and preserves every unmodified property
   and sibling component.
2. **Unambiguous identity**: a writable component is identified by calendar
   directory key, physical file, UID, and optional RECURRENCE-ID. Display names
   are presentation only.
3. **Transactional persistence**: a failed validation or write leaves the old
   file byte-for-byte available. Successful replacement is atomic within the
   filesystem's guarantees.
4. **Explicit temporal semantics**: DATE, UTC instant, TZID-local datetime, and
   floating datetime are distinct representations. Conversion requires an
   explicit timezone policy.
5. **Lossless patching**: updates express `Keep`, `Clear`, or `Set`; omission is
   never overloaded to mean two of those operations.
6. **Master/occurrence separation**: recurrence masters, generated occurrences,
   and persisted overrides are distinct and cannot be mixed accidentally.
7. **Semantic round trip**: reading and writing an unchanged component through
   the CLI, server, or Emacs form leaves its supported RFC semantics unchanged.
8. **Stable machine contracts**: JSON, CSV, ICS, and protocol output is valid in
   empty and non-empty cases, has a documented schema, and is not interleaved
   with diagnostics.
9. **Bounded work**: recurrence and alarm expansion always has an explicit
   range, count, or safety limit.
10. **Portable core**: core library, CLI, and tests build on macOS and Linux;
    platform-specific daemon capabilities degrade explicitly.

## 4. Scope and requirements

### 4.1 Storage, identity, and paths (P0)

- **STO-1 Complete-document mutation**: replace component-local wrapper
  calendars with a document model that retains the full parsed VCALENDAR.
  Todo, journal, event, master, and override edits/deletes must locate and
  replace only the exact target.
- **STO-2 Atomic writes**: write a sibling temporary file, flush it, parse and
  validate it, fsync it where supported, then rename it over the target. Clean
  up temporary files on every failure.
- **STO-3 Concurrency detection**: capture a file fingerprint when loading and
  reject an update if the target changed before replacement. Expose a
  conflict-specific error instead of overwriting external changes.
- **STO-4 Lock coordination**: serialize Caledonia writers per physical file.
  Locking must not weaken optimistic conflict detection for vdirsyncer or other
  external writers.
- **STO-5 Safe deletion**: remove the physical file only when the deleted
  component is the last component. Otherwise atomically rewrite the document.
  Deleting a VEVENT recurrence master also removes every retained same-UID
  RECURRENCE-ID override in that document; deleting one occurrence remains
  exact. Unrelated and opaque siblings are always preserved.
- **STO-6 Stable calendar identity**: retain both directory key and optional
  display name. All paths use the directory key. Duplicate display names are
  allowed and never used as write identifiers.
- **STO-7 Path confinement**: reject absolute names, separators, `.`/`..`, NUL,
  and any resolved path outside the configured calendar root. Existing unknown
  calendar names do not create directories except through an explicit create
  operation.
- **STO-8 Backups**: before the first replacement/deletion of a source file in a
  mutation, retain a recoverable backup according to a documented policy.

Acceptance:

- multi-component event/todo/journal fixtures survive every CRUD operation;
- injected parse, short-write, rename, and conflict failures preserve the old
  file and report an actionable error;
- path traversal and display-name ambiguity tests pass;
- no production code uses direct `O_TRUNC` replacement of a calendar file.

### 4.2 Patch and component domain model (P0/P1)

- **DOM-1 Typed patch algebra**: introduce `Keep | Clear | Set of 'a` and use it
  for all optional editable fields, including alarms, recurrence, description,
  location, categories, parent, end/duration, and completion metadata.
- **DOM-2 Shared document/component metadata**: event, todo, and journal values
  carry the same document identity and source metadata.
- **DOM-3 Component validation**: enforce component-specific status, priority,
  percent, due/start/end, parent, and alarm constraints on create and edit.
  Invalid values return errors and are never silently ignored.
- **DOM-4 Todo state machine**: centralize transitions among status,
  percent-complete, and completed timestamp. Every entry point produces the same
  valid state.
- **DOM-5 Parent graph safety**: reject missing parents, self-parenting, and
  cycles. Tree/ancestor functions terminate even for corrupt external input and
  surface the corruption.
- **DOM-6 Due-only todos**: range and overdue queries use due dates when DTSTART
  is absent and implement documented todo filters.
- **DOM-7 Registered-property integrity**: shared VEVENT, VTODO, and VJOURNAL
  validation rejects malformed registered properties that a permissive parser
  demotes to generic IANA extensions. Unknown extension names remain lossless;
  parser-specific aliases are explicit, component-scoped, and regression
  tested.

Acceptance:

- create/edit validation tables are exercised for every component type;
- clearing each optional field is tested through library, CLI, and protocol;
- todo status transitions and corrupt/cyclic parent fixtures are covered;
- due-only and date-only todo query behavior is specified and passing.

### 4.3 Time and range model (P0/P1)

- **TIM-1 Typed calendar time**: represent DATE, UTC, TZID, and floating values
  without encoding local wall time as a fake UTC `Ptime.t`.
- **TIM-2 Explicit timezone context**: remove ambient mutable timezone behavior
  from production calculation APIs. Parse, arithmetic, formatting, and query
  functions receive a timezone or an explicit local-time policy.
- **TIM-3 Calendar arithmetic**: day/week/month/year arithmetic and day bounds
  operate in the requested calendar timezone and remain correct through DST.
- **TIM-4 Half-open ranges**: all internal queries use `[from, to)` semantics.
  CLI inclusive date expressions are converted once at the boundary.
- **TIM-5 Floating policy**: accept and preserve floating values; document how
  they are interpreted for queries and display. Do not relabel them as TZID or
  UTC.
- **TIM-6 Unknown TZID and VTIMEZONE**: preserve unknown TZIDs, return typed
  conversion errors where an instant is required, and avoid inconsistent silent
  UTC fallback. Embedded VTIMEZONE support is either implemented or reported as
  an explicit unsupported conversion without corrupting the source.
- **TIM-7 Precision**: preserve seconds across CLI, protocol, and Emacs round
  trips. Numeric-offset RFC 3339 values accepted at protocol/CLI boundaries
  normalize to the same UTC instant: RFC 5545 DATE-TIME deliberately forbids
  numeric UTC offsets, so their original textual spelling is not an ICS
  round-trip invariant.

Acceptance:

- UTC, positive/negative offset instant equivalence, TZID, floating, DATE,
  unknown TZID, and DST transition matrices pass under multiple process
  timezones;
- all-day events retain exclusive DTEND after unchanged edits;
- no production test mutates global timezone state.

### 4.4 Recurrence and occurrence mutation (P0/P1)

- **REC-1 Typed exclusions**: EXDATE uses the same value kind and timezone
  semantics as DTSTART. All-day exclusions are emitted as `VALUE=DATE` and the
  selected local calendar date.
- **REC-2 Membership validation**: occurrence edit/delete verifies that the
  requested recurrence belongs to the selected series and is not already
  excluded or ambiguously overridden.
- **REC-3 Override identity**: persisted overrides require matching UID and a
  typed RECURRENCE-ID. Expansion never treats unrelated VEVENT siblings as
  overrides.
- **REC-4 Deterministic master selection**: identify the master explicitly and
  reject zero/multiple-master corruption instead of relying on component order.
- **REC-5 Override replacement**: editing the same occurrence replaces the exact
  override rather than appending duplicates. Override inheritance is complete
  for supported properties.
- **REC-6 Bounded expansion**: expansion is lazy or range-bounded from the
  caller's requested interval; unbounded series cannot allocate indefinitely.
- **REC-7 Query consistency**: list, search, alarm, server, and component queries
  use the same recurrence pipeline and apply filters at documented stages.
- **REC-8 Domain validation**: one validation boundary rejects nonpositive
  COUNT/INTERVAL values, duplicate or empty BY parts, values outside RFC 5545
  ranges, illegal cross-part/frequency combinations, fractional or
  kind-mismatched UNTIL values, and inapplicable registered RRULE, EXDATE,
  RDATE, and RECURRENCE-ID parameters before serialization or expansion.

Acceptance:

- DATE, UTC, TZID, floating, DST, multiple-EXDATE, duplicate override,
  unrelated sibling, invalid-membership, recurrence-domain boundary, and
  registered-parameter applicability fixtures pass;
- an all-day occurrence delete reparses successfully and leaves other instances
  queryable;
- pathological unbounded rules respect a deterministic work limit.

### 4.5 Alarms and daemon reliability (P0/P1)

- **ALM-1 Full trigger semantics**: support absolute triggers and relative
  triggers related to START or END for events and todos, including every
  `REPEAT`/`DURATION` fire. Preserve seconds and reject relative references
  whose required component boundary is absent.
- **ALM-2 Recurrence-aware horizon**: derive candidate events from the requested
  fire-time window plus the maximum relevant lead/lag; remove the hard-coded
  seven-day assumption.
- **ALM-3 No absolute duplication**: absolute alarms fire once, independent of
  recurrence expansion.
- **ALM-4 Missed-alarm recovery**: daemon restart queries a persisted last-check
  watermark with a bounded grace period and can deliver alarms missed during
  downtime. Successful notifications are deduplicated across restart and
  notifier failures are retried at least once. A desktop notifier and a local
  state-file rename cannot form one atomic transaction, so crash-boundary
  exactly-once delivery is explicitly not claimed.
- **ALM-5 Race-free watch loop**: establish watches before the initial scan or
  reconcile after watch establishment. Watch the same recursive scope as the
  loader and update watches incrementally.
- **ALM-6 Stable deduplication**: alarm identity includes calendar, file, UID,
  recurrence ID, and alarm identity. Retention is bounded and persisted when
  needed for restart correctness.
- **ALM-7 Failure handling**: notification failures, timeouts, malformed files,
  and watcher overflow are reported and retried according to policy; one bad
  notification cannot kill or hang the daemon. A failed inotify topology or
  overflow rebuild retains the old descriptor set, reports a typed error, and
  retries transactionally on the next safety timer. Nested directory watch or
  traversal failures reject the entire candidate snapshot instead of silently
  degrading recursive coverage.
- **ALM-8 Testable clock and notifier**: all `now`, sleep, watcher, and notifier
  behavior is injectable.
- **ALM-9 Platform abstraction**: Linux inotify/notify-send is one backend.
  Unsupported platforms build and return a clear capability error.

Acceptance:

- alarm variant matrix, END-relative todo alarms, long-lead alarms, downtime,
  startup race, recursive directories, retry, dedupe, and fake-clock tests pass;
- core/CLI tests build without inotify on macOS.

### 4.6 Query, sorting, and output contracts (P1)

- **QRY-1 One query pipeline**: consolidate range, type, recurrence, calendar,
  text, status, overdue, and alarm filtering for CLI and server use.
- **QRY-2 Correct sort keys**: each advertised key maps to its real value.
  Multi-key comparison respects user order and stable tie-breaking; component
  formatting does not repartition after sorting.
- **QRY-3 Documented type semantics**: todo/journal/event selection and due-only
  behavior match help and README examples. Search without a date constraint is
  a master/component query across all dates, including far-future data; it
  returns recurring VEVENT masters once rather than pretending an infinite
  recurrence expansion can be date-unbounded. Date-constrained search keeps
  half-open, range-bounded occurrence expansion and its deterministic work cap.
- **QRY-4 Filter-domain validation**: status tokens are validated against the
  selected component types before query execution. Protocol fields unavailable
  to its event-only capability return a structured unsupported-capability error
  instead of an empty successful result.
- **OUT-1 JSON**: emit typed, versioned objects; empty results are `[]`; errors
  use stderr/nonzero exit and never masquerade as JSON.
- **OUT-2 CSV**: use RFC 4180 escaping and one documented schema per command.
- **OUT-3 ICS**: emit one valid VCALENDAR envelope containing selected
  components without concatenated envelopes, together with the distinct
  VTIMEZONE definitions needed by components originating in different source
  calendars. Unreferenced definitions are omitted. Identical definitions for
  one referenced TZID are deduplicated; conflicting definitions are rejected
  before any output is written. An empty selection emits an empty iCalendar
  stream: RFC 5545's VCALENDAR grammar requires at least one component, so an
  empty envelope would be invalid.
- **OUT-4 S-expression**: use a complete serializer and a stable grammar; empty
  atoms, quotes, escapes, and multiline strings round-trip.
- **OUT-5 Terminal safety**: sanitize control sequences from untrusted calendar
  fields and calculate Unicode display width consistently.
- **OUT-6 Color control**: wire `--color` and `--no-color` consistently and honor
  non-terminal output.
- **OUT-7 Exit status**: missing IDs, invalid filters, parse failures, and write
  conflicts return nonzero statuses.

Acceptance:

- golden parsers validate JSON, CSV, ICS, and S-expressions including empty,
  Unicode, quotes, commas, newlines, and hostile terminal content;
- property-based comparator tests cover all advertised sort keys;
- CLI end-to-end tests assert stdout, stderr, and exit codes.

### 4.7 Server and Emacs round trip (P0/P1)

- **PRO-1 Versioned protocol**: add handshake/version, request IDs, structured
  success/error envelopes, and an explicit schema.
- **PRO-2 Structured temporal/alarm fields**: transmit exact DATE/datetime kind,
  TZID/floating/UTC semantics, seconds, DTEND versus DURATION, complete RRULE,
  and complete VALARM data rather than presentation strings.
- **PRO-3 No stale cache writes**: writes resolve identity against current disk
  state and conflict if changed. Cache refresh behavior is automatic and
  documented.
- **PRO-4 Complete component scope**: server queries and mutations support the
  same component types as the CLI or return an explicit capability error.
- **PRO-5 Protocol stream isolation**: diagnostics never enter stdout protocol
  frames. Parser errors become correlated responses.
- **EMA-1 Lossless forms**: unchanged submit is a semantic no-op for all-day
  DTEND, floating/TZID/UTC time, seconds, DTEND/DURATION, recurrence, alarms,
  optional text fields, categories, and calendar identity.
- **EMA-2 Clear operations**: empty user fields send explicit `Clear` patches.
- **EMA-3 Robust client transport**: correlate multiple outstanding responses,
  preserve partial frames correctly, reset state on restart, bound logs, use the
  documented timeout, and remove invalid error references/unsafe reads.
- **EMA-4 Compatibility**: declare the real minimum Emacs version, require used
  libraries such as `subr-x`, and byte-compile cleanly.
- **EMA-5 Agenda correctness**: empty ranges still render their requested dates;
  timezone discovery includes top-level UTC and nested zones.
- **EMA-6 Prototype cleanup**: remove or clearly quarantine the abandoned,
  incompatible `caledonia-event.el` prototype.

Acceptance:

- protocol encode/decode and interleaved request tests pass;
- ERT tests cover unchanged/edit/clear form round trips and transport restart;
- Emacs byte compilation is warning-clean at the declared minimum version;
- server end-to-end tests cover reload, conflict, malformed requests, and every
  supported component.

### 4.8 Packaging, documentation, and CI (P1/P2)

- **PKG-1 Satisfiable opam metadata**: declare the actual minimum OCaml version
  and every direct build/test dependency (`ppx_deriving`, `sexplib`,
  `ppx_expect`, etc.); remove unused dependencies.
- **PKG-2 Platform conditions**: make Linux-only dependencies conditional and
  ensure the package still builds the portable CLI on macOS.
- **PKG-3 Dependency provenance**: document the pinned `icalendar` fork and
  required fixes, or upstream them and use a released package. Pin metadata and
  commit claims must agree.
- **PKG-4 Version alignment**: synchronize dune-project, opam, Emacs package,
  changelog, and protocol versions.
- **PKG-5 License**: add the claimed MIT license file.
- **PKG-6 Defaults/docs**: choose one default calendar path and use it in code,
  README, CLI help, and examples. Correct server examples and supported sort
  keys.
- **PKG-7 Installed artifacts**: install the Emacs package and any required
  support files through the build/package definition.
- **PKG-8 Formatting**: pin an ocamlformat version and enforce it.
- **CI-1 Matrix**: CI runs clean opam install/build/test on supported OCaml
  versions and Linux/macOS, plus opam lint, formatting, and diff checks.
- **CI-2 Frontend gate**: CI byte-compiles and runs ERT for Emacs.
- **CI-3 Safety gate**: data-integrity, protocol, machine-output, and package
  tests are mandatory, not optional aliases.

Acceptance:

- a fresh checkout follows README commands without manual dependency installs;
- `opam install . --with-test`, package build, CLI smoke test, and test suite
  pass in the matrix;
- versions, changelog, license, default path, and help text agree.

## 5. Migration and compatibility

- Existing `.ics` files are never rewritten merely by being read.
- Unsupported or unknown properties/components are preserved during mutation.
- Protocol changes use a new version and fail clearly on mismatch; the Emacs
  client and server are upgraded together.
- CLI human output may improve, but machine output changes require documented
  schema/version notes in the changelog.
- Conflict and validation failures intentionally replace silent last-write-wins
  and silent-ignore behavior.

## 6. Delivery phases

1. **Baseline**: reproducible local toolchain, correct stale tests, PRD, and CI
   skeleton.
2. **Data safety**: storage transaction, identity, path confinement, complete
   component preservation, and destructive-path regression tests.
3. **Semantic core**: patch algebra, typed time/range model, recurrence,
   occurrences, todos, alarms, and unified query behavior.
4. **Interfaces**: valid output formats, CLI exit contracts, versioned protocol,
   lossless Emacs forms, and portable daemon backends.
5. **Release engineering**: metadata, dependencies, versions, license,
   documentation, install artifacts, CI matrix, and clean-environment proof.

Each phase must keep the prior phase's tests green. Safety fixes may temporarily
disable a mutation that cannot yet meet an invariant, but the final delivery
cannot leave required functionality disabled.

## 7. Definition of done

The remediation is done when all requirement IDs above are implemented or an
explicit, user-approved scope change removes them; all legacy and newly added
tests pass; clean installation and build work on Linux and macOS; Emacs tests
and byte compilation pass; no known P0/P1 audit finding remains reproducible;
and the changelog documents behavior and compatibility changes.

## 8. Completion evidence

All 72 requirement IDs passed final code audit with no reproducible P0 or P1
remaining. Acceptance was rerun from the settled tree on 2026-07-15:

- formatting, `@all`, the forced full suite, direct CLI integration, opam lint,
  YAML parsing, and diff hygiene passed on the host under UTC and Asia/Tokyo;
- strict Emacs warning-as-error byte compilation and all 25 ERT tests passed;
- a source snapshot without `.git`, `_build`, or the local switch passed
  `opam install . --with-test`, installed-artifact smoke tests, build, and tests;
- Debian 12 with OCaml 5.2 performed a clean dependency solve (including
  Timedesc 3.1.2 and Linux inotify), then passed formatting, `@all`, the full
  suite, direct CLI integration, package installation, and the real recursive
  watcher lifecycle/rebuild tests.

The configured GitHub OCaml/Linux/macOS and Emacs matrix remains an external
post-push gate. Explicitly unsupported future capabilities remain tracked in
`TODO.org`; they fail with typed/capability errors and do not silently degrade
or corrupt data.
