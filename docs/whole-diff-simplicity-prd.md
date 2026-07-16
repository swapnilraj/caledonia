# Whole-Diff Simplicity Remediation PRD

- Status: Complete
- Owner: Caledonia maintainers
- Date: 2026-07-16
- Scope base: `177a95f1e0e5cd63a027206f860604bcb08e9e3e`

## Objective

Re-audit the entire technical-debt worktree from first principles and make the
runtime model explainable as one short path:

```text
.ics bytes -> document -> stored component -> query item -> boundary output
                                  |
                                  +-> replacement body -> repository commit
```

The audit must cover every executable entry point, every practical storage and
recurrence path, the alarm daemon and watcher boundary, protocol v1, and the
Emacs client. It must remove duplicate representations and hidden ambient
inputs without erasing types that enforce a real lifecycle or compatibility
boundary.

## Problems found

The preceding 63-requirement audit correctly removed the original embedded
calendar and occurrence/storage confusion, but its completion claim missed:

- a stored component cached an identity derivable from its immutable body;
- a calendar mutation snapshot cached components derivable from its documents;
- Event, Todo, and Journal read the process clock and owned separate UUID RNGs;
- the alarm layer renamed `Component_query.item` as a second “subject” model;
- the daemon core built presentation strings and a notification DTO;
- protocol responses had two internal event variants with the same wire tag;
- stored query serialization exported the same source twice;
- an occurrence's `series_master` lacked the source fields required for an
  Emacs whole-series edit;
- DATE-overdue calculation could raise at the maximum valid civil date;
- the alarm trigger was total but wrapped in an impossible `option`;
- date-range parsing, component lookup, storage-error classification, result
  traversal, advisory locking, property patching, singleton validation, status
  text, and component-kind text were duplicated;
- recursive strict loading was nondeterministic while tolerant loading sorted;
- metadata reads caught every exception rather than I/O failures;
- documentation overstated external-writer atomicity and public provenance;
- 11 newer `origin/vibing` commits had not been explicitly adjudicated.

## Design rules

1. Cache only information that cannot be derived cheaply and safely from the
   immutable owner.
2. Capture time at the CLI/server boundary and pass it into domain mutation.
3. Use one process-wide UUID generator, but do not add an ID-service framework.
4. Use `Component_query.item` directly wherever a value is stored-or-derived.
5. Keep the daemon core about scheduling, dedupe, retry, and persistence; keep
   notifier strings at the executable boundary.
6. Keep protocol DTOs when the wire contract requires them, but use one
   internal payload for one wire constructor.
7. A total domain accessor must not be made optional by a formatting adapter.
8. Reuse small utilities only where behavior and error policy are identical.
9. Keep distinct: physical codec entries, validated documents, stored
   components, event occurrences, write targets, versioned wire requests, and
   persisted alarm-fire identities.
10. State filesystem guarantees at the level the operating system can provide.

## Requirements

- **DMW-1 Whole-diff call graph**: document every CLI, server, storage,
  recurrence, output, alarm, watcher, and Emacs flow from entry point to side
  effect.
- **DMW-2 Derived stored identity**: a stored component contains source and
  body only; identity and target are derived from the immutable body.
- **DMW-3 Derived deterministic snapshots**: calendar mutation snapshots retain
  documents only, derive components on demand, traverse strict and tolerant
  directories deterministically, and narrow metadata fallback to I/O errors.
- **DMW-4 Explicit clocks and shared identifiers**: domain create/edit APIs
  require `now`; Event/Todo/Journal contain no ambient clock read; all UUID
  generation shares one lazy process generator.
- **DMW-5 One alarm/query owner model**: alarm fires directly own
  `Component_query.item`; target and source accessors live on that shared
  query value.
- **DMW-6 Presentation-free daemon core**: the core notifier callback receives
  an alarm fire, not a presentation DTO; executable code formats the desktop
  notification.
- **DMW-7 One event response model**: one internal `Events` payload serves
  stored and occurrence responses; stored values are wrapped as query items;
  `series_master` carries a complete source target.
- **DMW-8 Total alarms and typed temporal failure**: formatting consumes the
  total `Alarm.trigger`; maximum valid DATE overdue calculation returns a
  typed range error and never raises.
- **DMW-9 Shared temporal scope**: list, search, and alarms use one parser for
  shortcut conflicts, inclusive CLI end dates, half-open query bounds,
  one-sided ranges, and default policy.
- **DMW-10 Shared boundary text codecs**: component kind and RFC status text
  mappings have one implementation used by query, output, and CLI parsing.
- **DMW-11 Shared patch/property utilities**: list-property replacement and
  singleton validation have one behavior and one implementation.
- **DMW-12 Shared command/result/lock utilities**: executable lookup/error/result
  helpers and repository advisory-lock mechanics are not copied.
- **DMW-13 Truthful lifecycle and concurrency contract**: documentation must
  distinguish abstract snapshot values from unforgeable capabilities and must
  describe advisory-lock and non-cooperating external-writer limits.
- **DMW-14 Upstream commit adjudication**: every commit in
  `177a95f..origin/vibing` is accepted, superseded, or rejected with a
  behavior-level reason and retained test evidence.

## Acceptance criteria

- The module dependency graph remains acyclic.
- Domain modules have no filesystem, document, output, protocol, ambient clock,
  or source dependency.
- Stored identity and calendar-snapshot component caches cannot be reintroduced
  without failing the architecture gate.
- Protocol v1 wire tags and machine-output schema versions do not change.
- Whole-series Emacs edits receive UID, calendar key, file, and fingerprint.
- Unknown TZID and invalid authored temporal data remain typed failures; the
  upstream graceful-degradation commit is not adopted.
- Host build, forced tests, architecture checks, CLI integration, Emacs ERT,
  formatting, opam checks, timezone matrices, and Linux/inotify verification
  pass after the final integration.

## Explicitly accepted boundaries

`Calendar_document.t` retains both its codec and validated component
projection. Decoding that projection can fail, and the immutable constructor is
the single point that proves they agree; this is a materialized validation
result, not an independently editable cache.

`Component_query.Occurrence` retains its stored series because source,
fingerprint, export context, and mutation reference belong to the persisted
series, not to the derived effective VEVENT.

`Component_source.of_decoded_document` and the stored-view decoder seam are
public fixture/decoder surfaces, not security capabilities. Disk writes still
validate confinement, exact identity, and fingerprint. Turning the installed
library into a capability system solely to hide test construction would add
more model than it removes.

The calendar lock serializes cooperating Caledonia writers. Exact target-file
compare-and-swap rejects non-cooperating changes to the target. A multi-file
todo-graph scan detects sibling changes at verification, but POSIX filesystems
cannot atomically lock arbitrary non-cooperating writers across sibling files.
External modifications after verification are new external commits, not a
guarantee Caledonia can prevent.

## Upstream decision

The following upstream intentions are accepted and superseded by the worktree:

- `fd419cf`: duplicate timezone formatting was removed by the new output
  projection;
- `3be2cef` and `90c92d3`: one RNG state is implemented more broadly by
  `Fresh_id`;
- `2bcd10f`: machine S-expression output is centralized in boundary
  serializers rather than domain-specific parallel formatters;
- `2e07136`: TZID DTSTART recurrence UNTIL is normalized to UTC;
- `8ad3c9d`: repository naming and documentation use component terminology;
- `5a79467`: time is explicit, extended here through create/edit mutations;
- `a4db933`: internal temporal ranges are half-open;
- `62c5694`: its behavior tests survive in the larger storage/output/CLI
  suites;
- `5c4e02c`: all-day display uses authored civil dates.

`8e4f50d` is rejected. Falling back on unknown TZID or invalid duration hides
bad authored data and contradicts the typed-failure invariant. Existing strict
tests remain authoritative.

## Completion evidence

Implementation evidence is recorded in
`docs/whole-diff-data-flow-audit.md`, the architecture gate, the requirement
ledger, focused protocol/temporal regression tests, and the full verification
matrix. Completion does not imply atomic control over non-cooperating external
filesystem writers or that abstract snapshot values are cryptographic
capabilities.
