# Caledonia Data-Model Simplification PRD

- Status: Complete
- Owner: Caledonia maintainers
- Target: the first release after the technical-debt remediation branch
Last updated: 2026-07-16

## 1. Objective

Make Caledonia's data model explain the tool's real ownership and lifecycle
rules directly, without weakening any of the storage, RFC 5545, recurrence,
protocol, or alarm guarantees already established by the technical-debt
remediation.

The implementation must have one owner for each physical calendar document,
one shared representation of source identity, separate types for stored
components and generated occurrences, and no component-local copy of a full
document that must be synchronized with an edited component body.

This is a simplification project, not a request for a generic entity framework,
database, ORM, or second field-by-field implementation of RFC 5545.

## 2. Background and problem statement

The completed technical-debt remediation fixed the dangerous behavior:

- writes reload and validate the latest complete VCALENDAR;
- source fingerprints reject stale mutations;
- exact UID and RECURRENCE-ID matching preserves sibling components;
- unsupported properties and components survive supported mutations;
- recurrence expansion and occurrence editing are validated and bounded;
- protocol and machine-output boundaries preserve authored semantics.

Those behavioral protections had to remain while this project replaced the
component-local document-wrapper architecture left by the original `STO-1`
remediation.

At the start of this project, an event, todo, or journal retained both its
individual iCalendar body and the complete source `Icalendar.calendar`. Edit
functions updated both values. Every component loaded from one physical file
also carried the same source metadata independently. Generated event
occurrences reused `Event.t` and retained the master document even though their
effective event body might not exist in that document. Newly created, unsaved
values used the same type as persisted values, with
`source_fingerprint = None` acting as a lifecycle sentinel.

Although the earlier storage implementation protected those representations at
write time, the starting model was difficult to reason about:

1. A component body and its embedded calendar can disagree.
2. `Event.t` does not reveal whether it is a stored master, draft, generated
   occurrence, or effective override.
3. Source and component identity are repeatedly rebuilt as tuples, strings, or
   raw iCalendar conversions.
4. Domain constructors know filesystem layout and manufacture a synthetic
   one-component document before storage owns it.
5. Parser-compatibility information is hidden inside synthetic iCalendar
   properties and is safe only when serialized through the correct adapter.
6. Domain modules also own legacy query, formatting, serialization, and storage
   helpers, obscuring the small domain model underneath.

This debt had originated incrementally. The original event wrapper acquired a
file and full calendar; todo and journal support copied that pattern;
occurrence support reused the event wrapper for derived instances; protocol and
alarm features then reconstructed identity from the available getters and
serialized forms. The earlier behavioral remediation hardened each path but
intentionally deferred the architectural rewrite completed by this PRD.

## 3. Goals

1. Make physical-document ownership explicit and singular.
2. Make source metadata and writable component identity shared, named values.
3. Make drafts, stored components, event series, and generated occurrences
   distinct concepts that cannot be accidentally interchanged.
4. Keep the upstream `Icalendar` AST as the canonical supported RFC payload.
5. Keep parser workarounds and opaque source material behind one abstract
   document/codec boundary.
6. Make `Calendar_dir` own path allocation, disk reload, conflict detection,
   document mutation, and canonical post-write reload.
7. Remove duplicated patch, kind, alarm identity, query, and presentation
   representations where separation is not a real boundary requirement.
8. Reduce the public domain API to construction, validation, editing, and typed
   access to domain data.
9. Preserve all documented CLI, protocol v1, machine-output, alarm-state, and
   `.ics` compatibility unless this PRD explicitly permits a versioned change.
10. Deliver the migration incrementally with the full suite green after each
    phase.

## 4. Non-goals

- Do not introduce a relational database, ORM, repository cache, event-sourcing
  system, or persistent application-specific calendar format.
- Do not normalize every RFC 5545 property into a second Caledonia-owned object
  graph. The `Icalendar` AST remains the supported-property payload.
- Do not build a generic base-component class, deep functor hierarchy, dynamic
  property registry, or reflection system for event/todo/journal differences.
- Do not evaluate embedded custom VTIMEZONE definitions as part of this work.
  They remain losslessly preserved and explicitly unsupported where an instant
  cannot be resolved.
- Do not add recurring VTODO or VJOURNAL support.
- Do not add `RANGE=THISANDFUTURE` support.
- Do not redesign protocol v1 or machine-output v1 merely to mirror internal
  type names.
- Do not expand macOS daemon functionality; Linux remains the primary daemon
  deployment and the existing watcher interface remains valid.
- Do not rewrite or fork the upstream `icalendar` parser in this project.
- Do not pursue arbitrary line-count targets. Module boundaries and invalid
  state elimination are the complexity measures.

## 5. Guiding invariants

1. **One document owner**: only the document/storage layer owns a complete
   parsed physical VCALENDAR and parser-compatibility metadata.
2. **One canonical body**: a logical component value contains one supported RFC
   body or event series, not a second synchronized document representation.
3. **Explicit lifecycle**: drafts do not have a source file or fingerprint;
   stored components always do.
4. **Explicit derivation**: an event occurrence is derived from a series and is
   not a writable stored event.
5. **Stable targeting**: a write target is source location plus component kind,
   UID, and optional RECURRENCE-ID. Display name is never identity.
6. **Fresh mutation**: storage applies a validated replacement to the latest
   conflict-checked document from disk, never to a caller-maintained document
   cache.
7. **Lossless source preservation**: every unmodified calendar property,
   VTIMEZONE, supported sibling, and opaque component survives mutation.
8. **Codec containment**: private parser markers or compatibility annotations
   are never observable as normal domain or public iCalendar properties.
9. **Boundary separation**: wire DTOs may differ from domain values when their
   syntax or versioning requires it, but equivalent algebraic concepts do not
   get independent competing implementations.
10. **Typed failure**: validation, identity, conflict, unsupported-capability,
    and I/O failures remain distinguishable results and never become silent
    fallback behavior.

## 6. Target conceptual model

The names below are illustrative. Implementations may adjust module names while
preserving the ownership and type boundaries.

```text
Calendar_dir
  owns Document values, one per physical .ics file
    Document owns Source, ordered calendar content, and codec metadata
    Document exposes validated Stored_component views

Stored_component
  is an immutable validated view of Source + Identity + Body

Body
  is Event_series | Todo_body | Journal_body

Event_series
  owns one authored master and its persisted RECURRENCE-ID overrides

Event_occurrence
  owns series identity + recurrence identity + effective event body
  is a query/output value and is never a Stored_component

Domain edit
  Stored_component + Patch -> replacement Body

Storage edit
  stored target + replacement Body -> conflict-checked Document mutation
```

### 6.1 Shared source and identity

The model should provide the equivalent of:

```ocaml
module Source : sig
  type t

  val calendar_key : t -> string
  val display_name : t -> string
  val file : t -> Eio.Fs.dir_ty Eio.Path.t
  val fingerprint : t -> string
end

module Identity : sig
  type kind = Event | Todo | Journal

  type t = {
    kind : kind;
    uid : string;
    recurrence_id : Icalendar.date_or_datetime option;
  }
end
```

`Source.t` is a persisted snapshot and therefore has a non-optional
fingerprint. A stored component combines its source and local identity to form
the complete write target. The precise representation may remain abstract.

### 6.2 Draft, body, and stored values

A draft/body contains validated RFC component data but no Eio filesystem path,
display name, or fingerprint. Storage turns a body into a stored value.

```ocaml
type body =
  | Event of Event.t
  | Todo of Todo.Body.t
  | Journal of Journal.Body.t

type stored
```

The design may use separate modules rather than these exact variants. It must
not use `source_fingerprint option`, a dummy path, or a synthetic embedded
VCALENDAR to distinguish a draft from a stored component.

A stored view is constructed from one immutable document snapshot. Its body is
the validated logical value selected from that snapshot, not a separately
editable document copy. Editing the view returns a replacement body; it does not
mutate the view or manufacture a synchronized replacement document.

### 6.3 Document

`Document.t` is abstract outside the document/codec/storage layer. It owns:

- stable source metadata;
- VCALENDAR properties;
- ordered known and opaque child components;
- information needed to preserve unsupported source blocks;
- compatibility metadata required by the pinned parser/writer;
- validated logical views such as event series, todos, and journals.

Opaque child position and known-component order must be preserved sufficiently
for exact sibling replacement and semantic round trips. Codec metadata must not
be represented to consumers as ordinary `X-CALEDONIA-*` properties.

### 6.4 Event series and occurrences

An event series owns exactly one master and zero or more persisted overrides.
Construction rejects missing masters, duplicate masters, mismatched UIDs,
duplicate RECURRENCE-IDs, and recurrence value-kind mismatches.

An occurrence records:

- the source/series identity;
- the authored recurrence identity;
- its effective start/end and event properties after inheritance;
- whether it came from the master expansion or a persisted override, if that
  distinction is needed for mutation commands.

Occurrence edit/delete commands accept an occurrence reference plus the stored
series they were selected from. General component edit/delete APIs accept only
stored values.

### 6.5 Query and presentation views

Bounded queries may return a small explicit view sum such as:

```ocaml
type query_item =
  | Stored of Component.Stored.t
  | Occurrence of Event.Occurrence.t
```

The exact name is not mandatory. The result must allow common output and alarm
code to read source identity and display fields without pretending that an
occurrence is a physical component.

## 7. Requirements

Priority describes migration risk and sequencing, not optionality. P0 items
protect ownership, persistence, or lifecycle correctness and must land before
dependent API removal. P1 items are required to complete the model. P2 items
are cleanup with lower immediate risk but remain part of the definition of done
unless an explicit scope decision moves them to a dated follow-up.

### 7.1 Document ownership and source model (P0/P1)

- **DMO-1 Single document abstraction**: introduce an abstract document value
  for one physical `.ics` file. A complete parsed calendar is owned only by this
  layer.
- **DMO-2 Remove component-local calendars**: `Event`, `Todo`, and `Journal`
  domain records must not contain `Icalendar.calendar` or an equivalent full
  document snapshot that edit functions synchronize with their body.
- **DMO-3 Shared source metadata**: calendar key, display name, physical file,
  and source fingerprint are represented once by a common source type used by
  every stored component.
- **DMO-4 Non-optional persisted fingerprint**: every stored source has a
  fingerprint. Draft/body values have no source instead of a `None` sentinel.
- **DMO-5 Document grouping**: load operations retain the relationship between
  one physical document and its logical components. Query callers may request a
  flat projection without becoming owners of document mutation state.
- **DMO-6 Immutable snapshots**: a loaded document/source is an immutable
  snapshot. Domain editing produces a replacement body or mutation description
  and does not alter or simulate an updated source document.
- **DMO-7 VTIMEZONE context**: selected-component ICS export can obtain the
  required VTIMEZONE definitions through explicit document/source context. A
  component must not retain an entire calendar solely to make export possible.

Acceptance:

- no event/todo/journal record contains a full calendar field;
- loading a multi-component file produces one document owner and validated
  logical views of all supported components;
- editing a body leaves the loaded document snapshot unchanged;
- selected ICS export still includes exactly the required non-conflicting
  VTIMEZONE definitions;
- existing mixed-component and opaque-sibling preservation tests pass.

### 7.2 Identity and lifecycle (P0/P1)

- **DMI-1 One component kind**: define one shared event/todo/journal kind type.
  Storage, queries, output, protocol adapters, and tests do not define competing
  kind variants.
- **DMI-2 One local identity**: define one shared kind, UID, and optional
  RECURRENCE-ID identity. It is constructed and validated when a document is
  decoded, not rediscovered through serialization.
- **DMI-3 Complete write target**: a stored source plus component identity forms
  the complete write target. Calendar display names and summaries never enter
  identity.
- **DMI-4 Named alarm fire identity**: replace the alarm daemon's positional
  tuple and serialized-text recurrence discovery with a named record built from
  the shared source and occurrence/component identity.
- **DMI-5 Draft separation**: event, todo, and journal construction returns
  source-free validated bodies. Constructors do not accept `fs`, calendar-root
  paths, or generate physical paths.
- **DMI-6 Storage-owned creation**: `Calendar_dir` validates the calendar key,
  chooses a confined filename, constructs the physical document, performs the
  atomic create, and returns the canonical stored value.
- **DMI-7 Canonical reload result**: successful create/edit/delete returns the
  canonical post-write stored value or an explicit mutation result. Callers do
  not infer the new source fingerprint or patch their own cached component list.
- **DMI-8 Explicit deletion outcome**: deletion reports whether the document was
  removed or rewritten and returns any canonical surviving views needed by the
  caller. It does not encode deletion as a malformed or body-less component.

Acceptance:

- domain constructors have no Eio dependency;
- no lifecycle behavior branches on `source_fingerprint = None`;
- storage, protocol targeting, todo graph keys, and alarms use the shared
  identity or an explicit boundary encoding of it;
- the daemon reads existing state schema version 2 without replaying already
  handled alarms; any new state schema is versioned and migration-tested;
- CLI and server creation return canonical reloaded identities and fingerprints.

### 7.3 Event series and occurrence model (P0/P1)

- **DMR-1 Explicit event series**: a stored logical event owns one master and
  its same-UID persisted overrides. Non-recurring events are a series with no
  recurrence rule/set and no overrides.
- **DMR-2 Explicit occurrence type**: recurrence expansion returns occurrence
  values, not cloned `Event.t` stored wrappers.
- **DMR-3 Non-writable derived values**: general component storage APIs cannot
  accept a generated occurrence. Type boundaries, not runtime conventions,
  enforce this.
- **DMR-4 Typed occurrence reference**: occurrence edit/delete uses the series
  target, authored recurrence identity, selected query timezone policy, and
  expected source fingerprint explicitly.
- **DMR-5 Series-local validation**: master/override cardinality, UID agreement,
  RECURRENCE-ID uniqueness, and temporal-kind agreement are validated once when
  constructing or replacing a series.
- **DMR-6 Series replacement semantics**: editing master recurrence fields
  explicitly declares what happens to existing overrides. The current contract
  of removing stored overrides when the recurrence set is changed remains unless
  separately changed and documented.
- **DMR-7 Occurrence export semantics**: exporting a query occurrence emits the
  effective occurrence requested by the caller; exporting a stored series emits
  the authored master and overrides according to the existing output contract.
- **DMR-8 Alarm identity**: alarm expansion obtains recurrence identity directly
  from the occurrence/series model and never by serializing and scanning an
  event.

Acceptance:

- compile-time interfaces distinguish stored series from occurrences;
- all recurrence, moved-override, EXDATE, duplicate-override, and occurrence
  mutation tests pass without a component-local calendar;
- attempting to edit a generated occurrence through the general stored edit API
  is impossible through the public interface;
- protocol v1 occurrence edits and deletes retain their current wire behavior;
- alarm deduplication remains stable across restart and source reload.

### 7.4 Codec containment and losslessness (P0/P1)

- **DMC-1 Abstract codec document**: `Calendar_codec.parse` returns an abstract
  document/content value rather than exposing a semantically augmented raw
  `Icalendar.calendar` as the complete model.
- **DMC-2 Explicit opaque entries**: unsupported component blocks are stored as
  validated opaque document entries with their document position, not disguised
  as ordinary calendar properties above the codec boundary.
- **DMC-3 Explicit compatibility metadata**: DATE-valued RRULE UNTIL recovery,
  duplicate VALARM preservation, writer-name canonicalization, and similar
  pinned-library workarounds stay inside the codec/document layer.
- **DMC-4 One production serialization path**: persisted and public ICS output
  use the document/codec serializer. Production callers do not serialize a
  codec document directly with `Icalendar.to_ics`.
- **DMC-5 Marker non-observability**: internal marker names and authenticated
  payloads cannot appear through component getters, protocol source fields,
  machine output, alarm identity, or normal document-property iteration.
- **DMC-6 Semantic round trip**: parse/serialize preserves all supported RFC
  semantics and every opaque component. Reads alone never rewrite files.
- **DMC-7 Mutation preservation**: replacing or deleting one logical component
  preserves VCALENDAR properties, VTIMEZONE definitions, known siblings, opaque
  siblings, and their valid ordering.
- **DMC-8 Validation ownership**: lexical/parser compatibility validation lives
  in the codec; component-domain validation lives in event/todo/journal modules;
  storage invokes both without duplicating their rules.

Acceptance:

- no synthetic opaque-component or DATE-UNTIL property is observable outside
  codec/document internals;
- adversarial source text cannot spoof codec metadata;
- opaque-only, mixed known/opaque, repeated-alarm, DATE UNTIL, lowercase syntax,
  and registered-property regression matrices pass;
- public ICS, protocol `source_ics`, backups, and written files contain no
  private parser markers;
- the serializer preserves current CRLF, folding, and canonical property-name
  contracts.

### 7.5 Repository and mutation API (P0/P1)

- **DMS-1 Repository-owned current state**: create/edit/delete operations load
  any state required for mutation and graph validation themselves. They do not
  accept the caller's complete cached `Component.t list` as mutation state.
- **DMS-2 Original target plus replacement**: edit accepts an original stored
  target and a validated replacement body/series. It verifies identity cannot
  change unless an explicit move/rename operation is introduced later.
- **DMS-3 Existing transaction guarantees**: path confinement, advisory lock,
  optimistic fingerprint verification, temporary write, fsync, parse
  verification, backup, atomic install, and cleanup behavior remain unchanged.
- **DMS-4 Fresh global invariants**: todo parent-graph validation uses a fresh
  repository snapshot covering the affected calendar scope before committing a
  write.
- **DMS-5 No cache-update return convention**: storage does not require callers
  to replace all components sharing a file in an application-owned list. It
  returns canonical changed data or callers explicitly reload.
- **DMS-6 One component mutation engine**: event compatibility wrappers,
  occurrence operations, CLI commands, and protocol commands route through the
  same document mutation engine.
- **DMS-7 Typed mutation errors**: conflict, missing target, ambiguous identity,
  invalid replacement, path violation, and I/O errors remain distinguishable.
- **DMS-8 No behavior regression**: deleting a final known component retains a
  physical file when opaque siblings remain; deleting a recurrence master
  removes its persisted overrides and no unrelated component.

Acceptance:

- mutation APIs have no whole-repository component-list input;
- multi-component create/edit/delete and todo parent-graph tests use canonical
  repository results rather than manual cache surgery;
- all injected short-write, parse, rename, and concurrent-edit failures retain
  the old source and backup guarantees;
- direct CLI, server, and library mutation paths produce the same bytes and
  errors for equivalent input.

### 7.6 Domain and boundary API simplification (P1/P2)

- **DMA-1 One patch algebra**: library and protocol use `Patch.t` or a true type
  re-export with wire codecs. There is no independent second `Keep | Clear |
  Set` type plus conversion function.
- **DMA-2 Typed query statuses**: protocol and CLI parse status strings at their
  boundary. `Component_query.criteria` carries typed statuses and still
  validates applicability to selected component kinds.
- **DMA-3 Presentation separation**: human text, entries, JSON, CSV, ICS, and
  S-expression presentation live outside event/todo/journal domain modules.
  Domain modules expose typed data needed by presentation.
- **DMA-4 Query separation**: the shared `Component_query` pipeline is the
  production query API. Legacy event/component comparator and functional-filter
  APIs are removed or isolated in a temporary `Legacy` module.
- **DMA-5 Recurrence separation**: recurrence expansion, occurrence inheritance,
  and occurrence mutation move behind an explicit series/recurrence module
  boundary while event field construction and validation remain discoverable.
- **DMA-6 Alarm calculation separation**: common alarm fire identity and trigger
  calculation use shared types. Event and todo supply typed start/end context
  without defining competing fire record shapes.
- **DMA-7 Restricted raw escape hatches**: raw supported component bodies may be
  exposed for interoperability, but full codec documents and private metadata
  remain abstract. Escape-hatch names document whether they return a body,
  series, occurrence, or document.
- **DMA-8 Explicit public interfaces**: protocol/serialization modules have
  `.mli` files that expose supported boundary types and entry points rather than
  every parsing helper.
- **DMA-9 Compatibility quarantine**: compatibility functions required for one
  release live in a clearly named module, are deprecated, have a removal target,
  and are not used by production Caledonia code.

Acceptance:

- event/todo/journal public interfaces no longer expose format, query, storage,
  or full-calendar synchronization functions;
- production code has one query pipeline and one patch algebra;
- output snapshots and protocol v1 fixtures remain unchanged unless an approved
  versioned schema change is documented;
- `ocamldep` shows no Eio dependency from pure event/todo/journal domain
  construction modules;
- legacy adapters, if any, have dedicated tests and no production callers.

### 7.7 Documentation, tests, and maintainability gates (P1)

- **DMT-1 Model documentation**: add a short architecture document showing
  document ownership, component lifecycle, event series, query occurrences, and
  mutation flow.
- **DMT-2 Interface tests**: add compile-time or compile-failure fixtures that
  prove draft bodies lack stored-source operations and occurrences cannot be
  passed to stored mutation APIs.
- **DMT-3 Ownership tests**: test multi-component load, body edit, canonical
  storage replacement, and post-write reload as separate stages.
- **DMT-4 Boundary tests**: keep protocol, Emacs, JSON, CSV, S-expression, and
  ICS fixtures at their public boundary so internal type changes cannot alter
  them accidentally.
- **DMT-5 Compatibility tests**: verify old alarm daemon state, existing `.ics`
  files, and protocol v1 requests remain readable throughout the migration.
- **DMT-6 Dependency gates**: add a maintainable check that pure domain modules
  do not acquire filesystem/storage dependencies and that raw production
  serialization is confined to approved codec/output adapters.
- **DMT-7 No hidden incompleteness**: temporary adapters, unsupported branches,
  and deferred cleanup are listed in the PRD status and cannot be hidden behind
  a green test suite.
- **DMT-8 Clean-environment proof**: the completed model passes formatting,
  build, full tests, CLI integration, Emacs tests, package installation, and the
  Linux inotify daemon lifecycle gate from a clean source snapshot.

Acceptance:

- architecture and contributor documentation agree with the implemented API;
- the full existing safety suite and all new model tests pass;
- no requirement is marked complete solely because old behavior tests are
  green;
- source and installed-package smoke tests exercise creation, edit, occurrence
  edit, deletion, query, machine output, and alarm-state reload.

## 8. Required public behavior compatibility

### 8.1 Calendar files

- Reading never rewrites a file.
- Supported mutations remain semantic, not byte-for-byte, round trips; unrelated
  and opaque content remains lossless according to the existing contract.
- File placement remains compatible with vdir-style calendar directories.
- Existing backups, locks, and hidden temporary-file conventions remain valid.
- No private compatibility marker may be persisted.

### 8.2 CLI and machine output

- Existing command names, flags, exit classifications, and successful output
  behavior remain stable.
- JSON, CSV, ICS, and S-expression version 1 schemas remain stable.
- A deliberate machine-schema change requires a new documented schema version,
  changelog entry, and old-version compatibility decision.

### 8.3 Protocol and Emacs

- Protocol version 1 request and response grammar remains stable.
- Existing source identity fields remain accepted as the boundary encoding of
  the new shared source/identity model.
- Internal replacement of the protocol patch representation must serialize and
  parse the exact existing `Keep`, `Clear`, and `(Set VALUE)` forms.
- Emacs occurrence and unchanged-form behavior remains lossless.
- Any unavoidable wire incompatibility requires protocol version 2; it must not
  be slipped into this migration under version 1.

### 8.4 Alarm daemon

- Existing state schema version 2 remains readable.
- Stable fire identity remains semantically equivalent for existing events,
  occurrences, alarm indexes, and fire times.
- If the named identity record requires a new persisted form, write a new schema
  version only after implementing and testing version 2 migration.
- Linux inotify remains the primary watcher backend behind the existing general
  watcher interface.

### 8.5 OCaml library API

Caledonia is pre-1.0, so removal of misleading library functions is allowed,
but it must be intentional:

- document breaking module/API changes in the changelog;
- provide a temporary `Legacy` adapter only where downstream migration cannot
  reasonably happen atomically;
- do not retain ambiguous `to_ical_calendar`, occurrence-as-event, or
  filesystem-aware domain constructor APIs indefinitely for source
  compatibility;
- migrate every in-repository caller before marking the corresponding old API
  deprecated or removed.

## 9. Delivery plan

### Phase 0: Baseline and architecture contract

1. Keep the current full suite green and record the exact commands.
2. Add the architecture document and final concrete type/API proposal.
3. Inventory all constructors and consumers of event/todo/journal records,
   `to_ical_calendar`, source getters, occurrence clones, patch conversions,
   query filters, formatting functions, and alarm fire keys.
4. Add missing characterization tests before moving ownership.

Exit gate: no production path involved in the migration lacks a behavior test.

### Phase 1: Shared values

1. Introduce shared `Source`, `Identity`, and `Kind` modules.
2. Replace private identity tuples and reconstruction helpers.
3. Introduce a named alarm fire identity while retaining state-v2 decoding.
4. Re-export or directly codec `Patch.t` at the protocol boundary.
5. Parse query statuses into typed values at CLI/protocol boundaries.

Exit gate: behavior and public formats are unchanged; there is one identity and
one patch algebra in production.

### Phase 2: Abstract document and codec state

1. Introduce `Document.t` around parsed physical content.
2. Represent opaque entries and compatibility metadata explicitly inside the
   document/codec layer.
3. Migrate loading, selected ICS export, and serialization to the document API.
4. Prevent direct production serialization of codec documents with the upstream
   writer.

Exit gate: private markers are unobservable and all storage/round-trip tests
pass through `Document.t`.

### Phase 3: Body, draft, and stored lifecycle

1. Extract source-free event/todo/journal body constructors and editors.
2. Make storage own filenames, synthetic VCALENDAR envelopes for new files,
   atomic creation, and canonical reload.
3. Replace component-local metadata fields with the shared stored source.
4. Remove component-local full calendars.
5. Change mutation APIs from cached-list plus modified wrapper to stored target
   plus replacement body.

Exit gate: no domain constructor depends on Eio, no stored fingerprint is
optional, and no component owns a full document.

### Phase 4: Event series and occurrences

1. Group authored masters and overrides into validated series.
2. Return explicit occurrence values from bounded expansion.
3. Migrate query, output, server, Emacs, occurrence mutation, and alarms to the
   occurrence API.
4. Remove stored-event cloning for generated instances.

Exit gate: occurrences cannot enter general stored mutation APIs and all
recurrence/alarm behavior remains green.

### Phase 5: API responsibility cleanup

1. Move presentation out of event/todo/journal modules.
2. Remove or quarantine legacy filter, comparator, query, and compatibility
   wrappers.
3. Move common alarm-fire calculation and identity into the alarm boundary.
4. Add explicit interfaces for protocol and serialization modules.
5. Update contributor documentation and changelog.

Exit gate: the public domain surface contains only domain responsibilities and
there are no production callers of legacy adapters.

### Phase 6: Clean verification and release readiness

1. Run formatting, build, full tests, CLI integration, Emacs checks, opam lint,
   and source/install smoke tests.
2. Run the Linux inotify watcher lifecycle/rebuild tests in a clean Linux
   environment.
3. Review every requirement ID against code and test evidence.
4. Remove temporary adapters whose documented migration window ends in this
   release; explicitly track any approved remainder.

Exit gate: the definition of done below is satisfied with no hidden P0/P1 data
model finding.

## 10. Risks and mitigations

| Risk | Impact | Mitigation |
| --- | --- | --- |
| Moving opaque source blocks changes ordering or loses data | Data corruption | Characterization fixtures for opaque-only and interleaved documents before changing the codec |
| Removing full calendars from components loses VTIMEZONE export context | Invalid or incomplete ICS output | Make document/source context an explicit input to selection export and retain conflict tests |
| Event series grouping changes recurrence behavior | Missing, duplicated, or moved occurrences | Reuse current recurrence validators and run the full recurrence/override matrix after every phase |
| Separating occurrences changes protocol/Emacs selection | Broken occurrence edits | Retain protocol v1 occurrence reference fields and add adapter-level equivalence tests |
| Repository-owned validation causes extra scans | Performance regression | Measure representative directories; optimize explicit document indexes only if demonstrated, without adding hidden ownership |
| Todo graph validation sees incomplete scope | Invalid parent relationships | Define and test the exact fresh calendar scope required before mutation |
| Alarm identity migration replays notifications | Duplicate user notifications | Preserve state-v2 parsing and prove old/new key semantic equivalence with restart fixtures |
| API cleanup retains two systems indefinitely | More complexity than before | Time-box `Legacy` adapters and prohibit production callers before completion |
| Big-bang refactor obscures behavioral regressions | Difficult review and rollback | Deliver the six phases independently with all prior gates green |

## 11. Requirement traceability

| Review finding | Primary requirements |
| --- | --- |
| Component body plus synchronized full calendar | DMO-1, DMO-2, DMO-6, DMS-2 |
| Event master/draft/occurrence lifecycle conflation | DMI-5, DMR-1 through DMR-4 |
| Repeated and reconstructed identity | DMI-1 through DMI-4 |
| Filesystem-aware domain construction | DMI-5, DMI-6, DMS-1 |
| Private codec state hidden as properties | DMC-1 through DMC-5 |
| Caller-owned component-list cache mutation | DMS-1, DMS-5, DMS-6 |
| Domain modules own query and presentation | DMA-3 through DMA-5 |
| Duplicate patch/kind/fire representations | DMI-1, DMI-4, DMA-1, DMA-6 |
| Untyped query statuses | DMA-2 |
| Broad public helper exposure | DMA-7 through DMA-9 |

This PRD refines the architectural intent of `STO-1`, `DOM-2`, and the
master/occurrence separation invariant from the technical-debt remediation
PRD. It does not reopen the completed behavioral requirements except where a
regression would violate their existing acceptance tests.

## 12. Definition of done

The data-model simplification is complete only when all of the following are
true:

1. Every `DMO-*`, `DMI-*`, `DMR-*`, `DMC-*`, `DMS-*`, `DMA-*`, and `DMT-*`
   requirement is implemented or removed through an explicit user-approved
   scope decision.
2. Exactly one abstract document layer owns each complete parsed physical
   calendar and its codec metadata.
3. Event, todo, and journal domain values do not retain full calendars or
   independently repeated source fields.
4. Draft bodies have no storage identity; stored components always have a
   non-optional source fingerprint.
5. Event series and generated occurrences are distinct public types, and a
   generated occurrence cannot be passed to a general stored mutation API.
6. Domain creation and editing are filesystem-independent.
7. Storage owns fresh document reload, targeting, conflict detection, graph
   validation, mutation, and canonical post-write results.
8. Private parser compatibility state is unobservable outside the abstract
   codec/document implementation.
9. Production has one component kind, one identity model, one patch algebra,
   one query pipeline, and named alarm fire identity.
10. Legacy query, formatting, full-calendar, and filesystem-aware constructor
    APIs have no production callers and are removed or have a dated, tested
    compatibility removal plan.
11. Existing `.ics`, protocol v1, machine-output v1, Emacs, alarm-state-v2, and
    CLI behavior passes its compatibility suite.
12. Formatting, build, full tests, direct CLI integration, Emacs checks, opam
    installation, installed-artifact smoke tests, and clean Linux inotify gates
    all pass from the settled source tree.
13. The architecture document, public interfaces, changelog, and implementation
    describe the same ownership and lifecycle model.
14. A final code audit finds no reproducible P0/P1 data-model ownership,
    lifecycle, identity, or codec-containment debt.

## 13. First-pass completion evidence

All 56 requirements are complete in
[`data-model-requirements.json`](data-model-requirements.json), with concrete
code, interface, test, and documentation evidence for each ID. An independent
final audit found no remaining P0/P1 ownership, lifecycle, identity,
recurrence, codec-containment, repository, or boundary-model debt.

Final verification on 2026-07-15 covered:

- `ocamlformat`, `dune @fmt`, a clean `@all` build, and the forced full suite;
- the 56-requirement architecture gate and all eight negative interface
  fixtures;
- direct CLI integration, version, and help checks;
- UTC and Asia/Tokyo recurrence, storage, export, and server matrices;
- strict Emacs warning-as-error byte compilation and all 25 ERT tests;
- opam lint, dependency solving, a tested reinstall, and installed binary and
  Emacs artifact smoke tests;
- GitHub Actions YAML syntax and workflow-shape validation;
- a parallel full Linux suite and the real recursive inotify watcher lifecycle
  on Debian 12 with OCaml 5.2.1 and `inotify.2.6` selected.

The Linux run exposed a nondeterministic concurrency test that used filesystem
timing as a barrier. It was replaced with a one-shot pre-verification test hook,
so the affected-calendar fingerprint race is now exercised deterministically.
No `.corrected`, `.elc`, temporary container, or other gate artifact remained.

## 14. Second simplicity audit

A complete follow-up inventory on 2026-07-16 reviewed every type declaration,
public interface, module dependency, persistence projection, query value, and
boundary DTO after the first migration had settled. The core ownership model
was sound, but seven residual simplification violations remained. They are part
of this PRD rather than an untracked cleanup list.

The follow-up deliberately does not replace explicit lifecycle types with
untyped tuples or push versioned wire DTOs into the domain. “Fewer types” is
useful only when two types describe the same concept. Distinct document,
stored-component, occurrence, patch, wire-request, and alarm-state values remain
required because they enforce real ownership or compatibility boundaries.

### 14.1 Additional requirements

- **DMO-8 Derivable document indexes** (P1): `Calendar_document.t` must not
  retain a positionally parallel identity list beside the codec entries.
  Identity is derived directly from each immutable known entry during a rewrite,
  eliminating the index-length invariant and its exception paths.
- **DMI-9 Total source display name** (P1): a decoded source has exactly one
  effective display name. The calendar key is applied as the fallback when the
  source is constructed, rather than storing `display_name option` plus a second
  `calendar_name` accessor and propagating both concepts through components and
  queries.
- **DMI-10 One component-kind algebra everywhere** (P1): internal validation
  uses `Component_kind.t`; it must not define a private polymorphic
  `Event | Todo | Journal` variant that mirrors the shared kind.
- **DMR-9 One public event-series type name** (P1): the authored event series is
  exposed as `Event.t`. `Event.Series.t` must not be a type-equal second public
  name with duplicate accessors. `Event.Occurrence.t` remains distinct and
  non-writable.
- **DMA-10 One temporal-value algebra** (P1): lossless authored calendar values
  use `Icalendar.date_or_datetime` throughout the domain. The flattened
  `Date.calendar_time` mirror and its encode/decode conversions are removed;
  `Date` supplies resolution and comparison operations directly over the
  canonical upstream value.
- **DMA-11 One query-sort algebra** (P1): `Query_args` parses directly into
  `Component_query.sort_spec`. It must not define a second polymorphic sort
  field record and convert it immediately before every query.
- **DMA-12 Minimal truthful public surface** (P1): remove type-equal,
  behavior-identical, or unused public aliases, including the optional-field
  `Event.edit` path beside `Event.edit_patch`, duplicate checked todo graph
  names, unused stored-series/display-name accessors, and unused algebra helper
  functions. Every retained public operation must describe one distinct
  capability or compatibility boundary.

Acceptance for the seven requirements:

- architecture tests reject reintroduction of `Date.calendar_time`,
  `Event.Series`, a document `known_identities` field, private component-kind
  variants, and CLI-local sort records;
- event/todo/journal values retain authored DATE, UTC, floating, and TZID
  distinctions without a conversion DTO;
- source construction stores a non-optional effective display name and machine
  output v1 remains byte/schema compatible;
- recurrence, todo hierarchy, sorting, storage mutation, protocol, output, and
  alarm tests pass through the reduced APIs;
- a clean build and full suite pass with all 63 ledger requirements complete.

### 14.2 Accepted irreducible boundary types

The audit explicitly accepts the following apparently similar types because
they encode different contracts:

- protocol `calendar_time_input`, `search_field`, and request records remain
  version-1 wire DTOs, not domain values;
- `Component_target.t` remains the source-plus-identity write capability rather
  than making source-free bodies writable;
- `Component_query.item` remains `Stored | Occurrence` because persistence and
  derivation are different lifecycles;
- the alarm watcher event and daemon driver event remain adapter and core
  boundaries with different error ownership;
- codec raw blocks, known entries, and opaque entries remain private physical
  preservation state;
- `Calendar_codec.Legacy` remains a deprecated characterization-only test seam
  until the pinned parser fixtures no longer require its augmented parse view;
  production use remains prohibited by the architecture gate.

### 14.3 Second-pass completion evidence

All seven follow-up requirements are complete, bringing the machine-readable
ledger to 63 of 63. The final source contains one document-entry identity path,
one effective source display name, one component-kind algebra, one public event
series type, the upstream iCalendar temporal algebra, one query-sort algebra,
and no retained duplicate convenience APIs identified by this audit.

Verification on 2026-07-16 covered:

- formatting, a clean full build, and the forced host test suite;
- all 63 architecture requirements and eight compile-failure interface
  fixtures;
- the full suite under both UTC and Asia/Tokyo;
- strict Emacs warning-as-error byte compilation and all 25 ERT tests;
- opam lint and dependency solving;
- a Debian 12/OCaml 5.2 full build and forced suite with the Linux inotify
  backend selected; and
- a final forbidden-pattern inventory, `git diff --check`, and generated-file
  cleanup.

## 15. Whole-diff third audit

The second-pass completion claim was reopened on 2026-07-16 after a complete
worktree and call-graph reconstruction plus a context-free independent review.
The new audit found correctness and simplicity gaps that the earlier
substring-heavy gate had not proved. The governing follow-up PRD is
[`whole-diff-simplicity-prd.md`](whole-diff-simplicity-prd.md), and the evidence
map is [`whole-diff-data-flow-audit.md`](whole-diff-data-flow-audit.md).

The third audit adds these requirements:

- **DMW-1** whole-diff call graph;
- **DMW-2** derived stored identity;
- **DMW-3** derived deterministic snapshots;
- **DMW-4** explicit domain clocks and shared identifiers;
- **DMW-5** one alarm/query owner model;
- **DMW-6** presentation-free daemon core;
- **DMW-7** one event response model and target-carrying series master;
- **DMW-8** total alarm triggers and typed temporal failure;
- **DMW-9** shared temporal-scope parsing;
- **DMW-10** shared boundary text codecs;
- **DMW-11** shared patch/property utilities;
- **DMW-12** shared command/result/lock utilities;
- **DMW-13** truthful lifecycle and external-concurrency contract; and
- **DMW-14** explicit upstream commit adjudication.

The earlier statement that every retained public operation was a distinct
capability was too strong. Decoder construction seams create abstract snapshot
values for fixtures and library decoders; they are not unforgeable security
capabilities. Repository writes remain protected by path confinement, exact
identity, and source fingerprints.

The earlier statement that a multi-file todo snapshot remains fresh until
commit was also stronger than POSIX can guarantee against a process that
ignores advisory locks. The implemented guarantee is serialization among
cooperating Caledonia writers, exact target-file compare-and-swap, and sibling
fingerprint revalidation immediately before the target operation. A later
external sibling write is a new external commit.

All 77 requirements are complete after the third audit's focused regressions,
architecture ratchets, full call-graph documentation, and final verification.
