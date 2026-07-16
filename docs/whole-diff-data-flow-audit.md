# Whole-Diff Data-Flow and Call-Graph Audit

- Date: 2026-07-16
- Branch: `codex/tech-debt-remediation`
- Fork point: `177a95f1e0e5cd63a027206f860604bcb08e9e3e`
- Compared upstream: `origin/vibing` through `5c4e02c`

## Scope and method

This audit was reconstructed without treating the earlier PRD or its 63/63
ledger as proof. It inventories the complete dirty worktree, public interfaces,
Dune module graph, executable entry points, protocol and Emacs callers, storage
side effects, tests, and the 11 upstream commits after the fork point.

At the start of this pass the tracked diff from the fork point was 51 files,
13,818 additions, and 5,339 deletions, with 69 untracked files. The upstream
branch was 11 commits ahead and the worktree contained no local commit after the
fork point. This matters: the deliverable is the entire working tree, not a
small commit range.

A second reviewer received no conversation history and independently rebuilt
the graph. That review reproduced the main ownership model and found the broken
Emacs whole-series target, maximum-DATE exception, external-writer guarantee
overstatement, and provenance overstatement. Those findings are incorporated
below.

## System in one page

```text
                           read
calendar root -> .ics bytes -> Calendar_codec.t
                                |
                                v
                         Calendar_document.t
                         source + codec + validated projection
                                |
                                v
                   Component.t = source + domain body
                         identity is derived
                                |
              +-----------------+------------------+
              |                                    |
              v                                    v
       Component_query.item                 replacement body
       Stored | Occurrence                         |
              |                                    v
       +------+------+                     Calendar_dir mutation
       |      |      |                     reload/CAS/validate/
       v      v      v                     atomic write/reload
     CLI    server  alarms                         |
   output    wire   daemon                         v
                                            canonical Component.t
```

The shortest correct explanation is:

1. The codec owns physical preservation.
2. A document proves the physical form decodes to valid supported bodies.
3. A stored component is an immutable source plus one source-free body.
4. A query returns that stored value or a bounded derived event occurrence.
5. Presentation adapters serialize query items.
6. Mutations submit a replacement body to the repository, which reloads and
   rewrites the authoritative document.

## Canonical types and why they exist

| Type | Owns | Does not own |
| --- | --- | --- |
| `Calendar_codec.t` | ordered known/opaque entries, calendar properties, parser compatibility metadata | Eio path, query state, presentation |
| `Calendar_document.t` | one source snapshot, codec, validated component projection | mutable cache, query occurrence |
| `Component_source.t` | calendar key, display name, physical path, non-optional fingerprint | body or identity |
| `Event.t` | authored master plus persisted overrides and DATE-UNTIL metadata | source, file, generated occurrence |
| `Todo.t` / `Journal.t` | validated upstream RFC property lists | source or document |
| `Component.t` | source plus Event/Todo/Journal body | cached identity, embedded calendar |
| `Event.Occurrence.t` | effective bounded instance and nominal reference | persistence capability |
| `Component_query.item` | stored component or occurrence with stored-series context | second alarm-specific owner model |
| `Component_target.t` | source plus derived local identity | replacement body |
| `Alarm_fire_id.t` | stable persisted delivery identity | presentation strings |
| protocol request records | version-1 syntax and optional wire fields | domain ownership |

The document's codec and component projection are intentionally both retained.
The projection is the result of a validation operation that may fail and is
created only with the immutable document. It is not independently updated.

## Read and decode call graph

```text
Calendar_dir.get_documents
  -> list_calendar_names
       -> Eio.Path.read_dir -> filter confined directories -> sort
  -> get_calendar_documents(calendar_key)
       -> get_display_name (I/O fallback only)
       -> load_documents_recursive (sorted, no symlink traversal)
            -> Eio.Path.load
            -> content fingerprint
            -> Component_source decoder snapshot
            -> Calendar_document.parse
                 -> Calendar_codec.parse_document
                 -> Calendar_document.decode
                      -> Component.stored_views_of_decoded_components
                           -> Event.of_authored_events_result
                           -> Todo.of_ical_body
                           -> Journal.of_ical_body
                           -> duplicate identity rejection
```

Strict CLI/server loads fail the operation on a malformed supported document.
The alarm daemon uses `get_components_tolerant`: it walks the same sorted,
non-symlink topology, reports each malformed or unreadable file, and continues
with other documents. This difference is a deliberate availability boundary.

## Query call graph

```text
list/search/server Query
  -> Query_args.resolve_temporal_scope or Sexp query decoder
  -> Calendar_dir.get_documents
  -> flatten Calendar_document.components
  -> Component_query.validate_criteria
  -> invariant filters on stored components
  -> bounded path:
       recurring Event.t -> Event.Recurrence.expand -> Occurrence values
       non-recurring/todo/journal -> Component.get_start_result
     unbounded path:
       masters only; no infinite recurrence expansion
  -> dynamic filters (text/status/completion/overdue/alarm)
  -> checked todo ancestor expansion when requested
  -> one decorate/sort/limit stage
  -> Component_query.item list
```

Temporal bounds are half-open internally: `from <= instant < to_`. CLI
`--to` remains an inclusive civil-date interface; the shared resolver turns
it into the next local midnight. Search defaults to `Unbounded`; list and
alarms default to one local month. A lower-only range extends one calendar
month, not a fixed duration.

Recurring event masters are expanded only for bounded queries and retain their
stored series in every occurrence item. This context is necessary for source
fingerprint, VTIMEZONE export, and occurrence edit/delete.

## Output call graph

```text
Component_query.item list
  -> Output.validate_human_items (typed start/end resolution)
  -> format selector
       text/entries -> human projections and terminal sanitization
       json/csv/sexp -> versioned boundary projections
       ics -> Calendar_export.to_ics
                -> select owning Calendar_document
                -> collect requested known entries
                -> collect required VTIMEZONE entries
                -> reject conflicting TZID definitions
                -> Calendar_codec serialization
```

Machine formats preserve authored DATE/UTC/floating/TZID forms rather than
flattening them to display instants. Human all-day output uses civil dates.
Terminal fields are sanitized separately from trusted row separators.

## Create call graph

```text
CLI add / server CreateEvent
  -> capture now once
  -> boundary parsing (date, time, recurrence, alarms, status)
  -> Event.create / Todo.create / Journal.create ~now
       -> Fresh_id.generate
       -> domain/property/alarm validation
       -> source-free body
  -> Calendar_dir.create_stored_component
       -> validate calendar key and confinement
       -> calendar advisory lock
       -> derive identity and safe basename
       -> Calendar_document.known_entries_of_body
       -> construct minimal physical VCALENDAR
       -> load documents and validate calendar-wide todo graph
       -> verify sibling snapshot fingerprints
       -> atomic exclusive create + parent sync
       -> parse canonical installed bytes
       -> return exact canonical Component.t
```

The domain receives `now`; it no longer reads `Ptime_clock.now`. UUID state
is initialized once for the process. Storage alone allocates the path and turns
the body into a stored snapshot.

## Edit call graph

```text
CLI edit
  -> Calendar_dir.get_components
  -> Command_common.find_unique_component(uid)
  -> parse Patch.Keep/Clear/Set values
  -> Event.edit_patch / Todo.edit / Journal.edit ~now
  -> replacement Component.body

server EditEvent
  -> exact uid + calendar_key + file lookup
  -> optional source fingerprint verification
  -> ordinary edit, or occurrence reference resolution + override creation

both
  -> Calendar_dir.replace_stored_component
       -> calendar lock
       -> derive and compare immutable identity
       -> confinement and target fingerprint check
       -> parse current target document
       -> Calendar_document.replace exact known entry/series
       -> validate candidate calendar-wide todo graph
       -> verify loaded sibling fingerprints
       -> atomic target compare-and-swap replacement
       -> canonical reload
```

Domain editing returns a body; it never updates a caller-owned document. A
no-op patch returns the original domain body and does not change DTSTAMP.

## Delete call graph

```text
CLI delete -> unique UID lookup
server DeleteEvent -> exact source lookup + fingerprint
  -> stored deletion:
       Calendar_document.delete(identity)
       -> delete file when no entries remain
       -> otherwise rewrite survivors
  -> occurrence deletion:
       Event.Recurrence.resolve_reference
       -> Event.Recurrence.delete_occurrence
       -> replacement event series
  -> calendar-wide todo graph validation
  -> snapshot verification
  -> atomic delete/rewrite
  -> explicit File_deleted | Document_rewritten outcome
```

Deleting one supported component preserves sibling supported components,
calendar properties, VTIMEZONEs, and opaque physical blocks.

## Recurrence graph

```text
Event.t(master, overrides)
  -> validate recurrence set and override identities
  -> Recurrence.expand(floating_tz, from, to_, max_instances)
       -> recurrence library candidates
       -> EXDATE/RDATE and persisted-override reconciliation
       -> Event.Occurrence(reference, origin, effective event)

Occurrence.Reference
  -> uid
  -> authored RECURRENCE-ID value and parameters
  -> nominal occurrence start
  -> query timezone

edit one:
  resolve_reference -> create_override(~now, patches) -> add override
delete one:
  resolve_reference -> delete override or add EXDATE
```

DATE, UTC, floating, and TZID identities remain distinct. Unknown TZID is a
typed error. Expansion is bounded by the query window and work limit.

## Protocol/server graph

```text
stdin bytes
  -> maximum line/frame guard
  -> Sexplib parser
  -> Sexp.parse_wire_request
       -> protocol version + nonempty request_id checks
       -> handshake state
  -> handle_request(now captured per request)
       Handshake -> Hello/capabilities
       ListCalendars -> Calendar_dir
       Query -> Component_query
       Create/Edit/Delete -> domain + repository paths above
       Refresh -> Empty
  -> one Sexp.Events payload or other response payload
  -> response envelope with same request_id
  -> stdout frame
```

One internal `Events` variant now holds query items, an optional occurrence
timezone, and document context. The wire tag remains `Events`. Stored create
and edit results are `Stored` items with no occurrence timezone. Query
responses carry the timezone required to render occurrence-local fields.

An occurrence's nested `series_master` is serialized through the stored-event
serializer, so it carries calendar key, file, and fingerprint. This fixes the
Emacs “All events in series” path without changing the wire schema.

## Emacs graph

```text
interactive command / agenda refresh
  -> persistent caled server process
  -> request envelope with monotonic request id
  -> process filter accumulates frames
  -> response correlation
  -> alist event projection
       -> agenda rows / event form

form submit
  -> authored calendar/alarm/recurrence values retained in form state
  -> CreateEvent or EditEvent request
  -> on occurrence:
       this occurrence -> outer source + occurrence context
       all in series -> nested series_master complete source target
  -> refresh

delete
  -> stored target or occurrence context -> DeleteEvent -> refresh
```

The client translates response alarm values to request DTOs at its boundary.
That translation is required because request and response schemas have
different direction-specific fields; it is not a competing domain model.

## Alarm CLI and daemon graph

```text
stored components
  -> Alarm_query.run_result
       Event.compute_alarm_fires_result
       Todo.compute_alarm_fires_result
       attach Component_query.item owner
  -> sorted Alarm_query.fire list

alarms CLI
  -> Format_utils.format_alarm_trigger_text
  -> text/json/entries

daemon driver
  -> tolerant component load
  -> Alarm_query
  -> Alarm_daemon_core
       stable Alarm_fire_id
       watermark + fired set + pending attempts
       ACTION:NONE terminal dedupe without delivery
       notify(fire)
       retry/drop/persist
  -> alarm_daemon_cmd formats notification fields
  -> notify-send / osascript / stdout / disabled
```

The core does not depend on `Format_utils` and owns no notification DTO. This
keeps retry state independent of presentation policy.

## Watcher graph

```text
Alarm_watcher interface: create, wait, close, backend_name
  -> Linux selected build: alarm_watcher_inotify
       recursive watch installation
       changed/topology/overflow decoding
       topology rebuild after invalidation
       timer deadline
  -> portable selected build: alarm_watcher_poll
       timer-only safety scans

watcher event
  -> daemon core Timer | Changed | Overflow | Watcher_error
  -> every event leads to authoritative full reconciliation
```

Linux inotify is the primary deployment. The interface makes the portable
fallback explicit without forcing macOS-specific complexity into the core.

## Storage consistency contract

Cooperating Caledonia writers serialize on a per-calendar advisory lock. Target
files additionally use fingerprint compare-and-swap and atomic rename/delete.
Writes are flushed and parent directories are synchronized where supported.

Calendar-wide VTODO parent validation necessarily reads multiple physical
files. Sibling fingerprints are rechecked immediately before the target
operation and a detected change aborts. A process that ignores advisory locks
can write a sibling after that verification; POSIX offers no transaction across
arbitrary external writers. The correct contract is conflict detection and
serializability among cooperating writers, target-file CAS for all writers, and
best-effort sibling-snapshot detection—not prevention of future external
commits.

## Dependency judgment

The dependency graph is acyclic. The intended direction is:

```text
Patch/Fresh_id/kind/status/identity/source/target/errors
  -> Date/Alarm/domain Event|Todo|Journal
  -> Component
  -> Codec/Document/Export
  -> Query/Alarm query/Daemon core
  -> Calendar_dir and Sexp boundaries
  -> CLI/server/output/watcher/Emacs
```

The important correction in this audit is the removal of the daemon-core
dependency on presentation formatting. Domain modules remain source-, Eio-,
document-, protocol-, and output-free.

## Shared utilities accepted

- `Fresh_id.generate`: one process RNG state.
- `Patch.replace_in_list`: one Keep/Clear/Set property-list operation.
- `Property_validation.validate_singleton`: one duplicate/required policy.
- `Component_status`: one RFC status text codec without a new status type.
- `Component_kind.to_string`: one kind rendering.
- `Query_args.resolve_temporal_scope`: one CLI temporal policy.
- `Command_common`: unique component lookup, storage-error classification,
  and result-list traversal.
- `Calendar_dir.with_advisory_lock`: one Unix lock lifecycle.
- `Component_query.get_target`: one query-item-to-write-target derivation.
- `Format_utils.format_alarm_trigger_text`: one total presentation function.

Rejected utility ideas:

- no generic entity/repository abstraction;
- no generic RFC property registry or reflection layer;
- no shared “calendar time DTO” parallel to `Icalendar.date_or_datetime`;
- no common output/protocol record that would couple versioned schemas;
- no conversion of abstract source snapshots into a security capability system.

## Upstream reconciliation

| Commit | Decision | Reason |
| --- | --- | --- |
| `fd419cf` | Superseded | New output projections removed the old duplicate formatter branch. |
| `3be2cef` | Accepted/superseded | `Fresh_id` shares one generator across all producers. |
| `8e4f50d` | Rejected | Silent fallback on unknown TZID/invalid duration masks invalid authored data. |
| `90c92d3` | Accepted/superseded | Same shared generator covers Todo and Journal. |
| `2bcd10f` | Accepted/superseded | Boundary S-expression serializers replace domain parallel formatting. |
| `2e07136` | Accepted | TZID recurrence UNTIL is represented in UTC. |
| `8ad3c9d` | Accepted | Component naming is used throughout repository APIs/docs. |
| `5a79467` | Accepted/extended | Explicit time now reaches domain create/edit APIs. |
| `a4db933` | Accepted | Queries use half-open ranges; CLI inclusive dates adapt once. |
| `62c5694` | Accepted/superseded | Broader storage/output/CLI tests preserve its behavior intent. |
| `5c4e02c` | Accepted | All-day display is based on authored civil dates. |

Direct cherry-picking is inappropriate because every accepted change overlaps a
much larger rewrite in the dirty worktree. Integration is behavior-level and
ratcheted by the current tests.

## Findings disposition

Fixed:

- derived stored identity cache;
- derived calendar component cache;
- nondeterministic strict traversal and broad exception catches;
- hidden domain clocks and five UUID generators;
- alarm-query subject aliases;
- daemon presentation DTO/dependency;
- duplicate event response variants and duplicate stored source export;
- source-less series master;
- maximum DATE exception;
- optional wrapper around total alarm trigger;
- duplicated date range, status/kind mapping, property patching/singletons,
  command lookup/error/result traversal, and lock lifecycle.

Accepted and documented:

- document codec plus validated projection;
- occurrence plus stored series;
- public decoder/test seams are abstract snapshots, not unforgeable
  capabilities;
- external non-cooperating sibling writes cannot be atomically prevented.

Rejected:

- graceful degradation for unknown TZID/invalid authored duration;
- merging stored and occurrence types;
- flattening physical codec state into domain values;
- generic frameworks whose abstraction cost exceeds the repeated behavior.

## Verification matrix

Final verification completed on 2026-07-16:

- `dune build @all` and `dune build @fmt`;
- forced complete `dune runtest`;
- architecture ledger and negative interface fixtures;
- direct CLI integration;
- focused series-master and maximum-DATE regressions;
- Emacs warning-as-error byte compilation and all ERT tests;
- UTC and Asia/Tokyo full test matrices;
- opam lint/dependency solve and installed artifact smoke;
- Linux build with inotify selected and recursive watcher lifecycle;
- `git diff --check` and no generated residue.

The ledger was marked complete only after the applicable gates ran after the
last source change. The Linux proof used the final source tree in a clean
Debian 12 / OCaml 5.2 environment with `inotify.2.6` selected. The subsequent
review-navigation documentation changes were followed by a host `@all @fmt`
build, the forced suite, link/ledger checks, and `git diff --check`; they did not
change generated artifacts or runtime sources.
