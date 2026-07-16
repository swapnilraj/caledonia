# Pull-request review guide

- Status: reviewer entry point for the 0.5.0 technical-debt remediation
- Branch at audit time: `codex/tech-debt-remediation`
- Review base: `177a95f1e0e5cd63a027206f860604bcb08e9e3e`
- Upstream compared: `origin/vibing` through `5c4e02c`

## Review outcome in one page

This is a cross-cutting safety and simplification change, not a feature-sized
patch. It replaces a model in which components carried partial copies of whole
calendars, occurrences could resemble writable stored values, boundary schemas
were spread through domain modules, and calendar mutations trusted too much
caller-owned state.

The implemented model is:

```text
.ics bytes
  -> Calendar_codec.t             physical and lossless compatibility state
  -> Calendar_document.t          one immutable snapshot per physical file
  -> Component.t                  source + source-free validated body
  -> Component_query.item         Stored | derived Event occurrence
  -> CLI / protocol / alarms

replacement body + stored target
  -> Calendar_dir                 lock, reload, conflict check, validate, write
  -> canonical Component.t        reloaded from installed bytes
```

The deliberate simplicity rule is not “use the fewest types.” There is one
type per real lifecycle or compatibility boundary and no second type for the
same fact. In particular, documents, stored components, occurrences, mutation
targets, protocol records, and persisted alarm identities remain distinct.

## What changed

- One abstract document/codec owner preserves VCALENDAR properties,
  VTIMEZONEs, opaque blocks, repeated fields, and pinned-parser workarounds.
- Event, todo, and journal bodies contain no source, path, document, query, or
  presentation state. Creation and editing receive `now` explicitly.
- A stored component contains only its source and body; kind, UID, recurrence
  identity, and write target are derived.
- Repository mutations reload current bytes, enforce confinement and exact
  identity, compare fingerprints, validate the affected calendar, install
  atomically, and return a canonical reload.
- Authored event series and generated occurrences have separate lifecycles.
  Occurrence writes require a typed reference and retain their stored series.
- Query, machine output, ICS export, server protocol, and Emacs transport are
  explicit boundary layers rather than methods on domain values.
- Alarm discovery uses the shared query-item model. The daemon core owns
  scheduling/retry/state, while the executable owns notification text.
- Linux uses recursive inotify behind a small watcher interface. The polling
  implementation is a portable reconciliation fallback, not the primary
  deployment path.
- Shared leaf utilities replace duplicated UUID, status/kind, patch,
  singleton, CLI result, temporal-range, and lock behavior without introducing
  a generic entity or repository framework.

## Recommended review order

Review by invariant rather than alphabetically. Each slice can be understood
before moving to the next one.

| Order | Slice | Principal files | What to establish |
| --- | --- | --- | --- |
| 1 | Contract and ratchets | [architecture](data-model-architecture.md), [ledger](data-model-requirements.json), [architecture test](../test/architecture/check_architecture.ml), [negative interfaces](../test/architecture/check_negative_interfaces.ml) | The target ownership model is explicit and regressions fail mechanically. |
| 2 | Leaf types | [`component_kind`](../lib/component_kind.mli), [`component_identity`](../lib/component_identity.mli), [`component_source`](../lib/component_source.mli), [`component_target`](../lib/component_target.mli), [`patch`](../lib/patch.mli), [`storage_error`](../lib/storage_error.mli) | Each type owns one fact; foundational modules do not import storage or presentation. |
| 3 | Domain lifecycle | [`event`](../lib/event.mli), [`todo`](../lib/todo.mli), [`journal`](../lib/journal.mli), [`date`](../lib/date.mli), [`alarm`](../lib/alarm.mli) | Bodies are source-free, time is explicit, recurrence is typed, and invalid temporal data returns errors. |
| 4 | Physical preservation | [`calendar_codec`](../lib/calendar_codec.mli), [`calendar_document`](../lib/calendar_document.mli), [`calendar_export`](../lib/calendar_export.mli) | The pinned parser is quarantined, rewrites preserve unowned content, and export receives document context explicitly. |
| 5 | Stored lifecycle | [`component`](../lib/component.mli), [`calendar_dir`](../lib/calendar_dir.mli), [storage tests](../test/test_storage.ml), [ownership tests](../test/test_repository_ownership.ml) | Callers submit a target and replacement body; the repository owns reload, validation, conflicts, and canonical results. |
| 6 | Query and presentation | [`component_query`](../lib/component_query.mli), [`output`](../bin/output.mli), [`query_args`](../bin/query_args.ml), [machine output contract](machine-output-v1.md) | Stored values and occurrences stay distinguishable, ranges are half-open internally, and schemas are versioned at the boundary. |
| 7 | Protocol and Emacs | [`sexp`](../lib/sexp.mli), [`server_cmd`](../bin/server_cmd.ml), [protocol v1](protocol-v1.md), [`caledonia.el`](../emacs/caledonia.el), [server E2E](../test/test_server_e2e.ml), [ERT](../emacs/test-caledonia.el) | Requests are correlated and bounded, source fingerprints survive round trips, and occurrence/series mutations target the right value. |
| 8 | Alarms and watcher | [`alarm_query`](../lib/alarm_query.mli), [`alarm_daemon_core`](../lib/alarm_daemon_core.mli), [`alarm_watcher`](../bin/alarm_watcher.mli), [daemon tests](../test/test_daemon.ml), [watcher tests](../test/test_watcher.ml) | Fire identity is stable, delivery is replay-bounded, presentation is outside the core, and inotify events lead to authoritative reconciliation. |
| 9 | Packaging and compatibility | [Dune project](../dune-project), [opam package](../caledonia.opam), [CI](../.github/workflows/ci.yml), [changelog](../CHANGELOG.md), [recovery](data-recovery.md) | The declared platform/dependency/test surface matches the implementation and operational contracts. |

For a function-level reconstruction after these slices, use the
[whole-diff data-flow audit](whole-diff-data-flow-audit.md). It traces read,
query, create, edit, delete, recurrence, output, server, Emacs, alarm, and
watcher call graphs.

## High-risk invariants and evidence

| Risk | Required behavior | Primary evidence |
| --- | --- | --- |
| Lossy `.ics` rewrite | Preserve calendar properties, opaque components, VTIMEZONEs, repeated alarms/properties, siblings, and meaningful order. | [`test_storage`](../test/test_storage.ml), [`test_calendar_export`](../test/test_calendar_export.ml), [`calendar_codec`](../lib/calendar_codec.ml) |
| Stale or wrong-file mutation | Confine paths; match calendar, file, fingerprint, kind, UID, and recurrence identity; reload current bytes before mutation. | [`test_storage`](../test/test_storage.ml), [`test_repository_ownership`](../test/test_repository_ownership.ml) |
| Recurrence corruption | Keep authored `RECURRENCE-ID` separate from moved effective start; preserve DATE/floating/UTC/TZID form; bound expansion. | [`test_occurrence`](../test/test_occurrence.ml), [`test_event`](../test/test_event.ml) |
| Timezone or range drift | Resolve with explicit timezone policy; use half-open internal bounds; adapt inclusive CLI end dates once; reject unknown evaluative TZIDs. | [`test_date`](../test/test_date.ml), [`test_query`](../test/test_query.ml), [CLI integration](../test/cli/cli_integration.ml) |
| Wire/schema drift | Keep protocol v1 and machine-output v1 tagged, typed, and lossless; reject malformed or oversized frames. | [`test_server_e2e`](../test/test_server_e2e.ml), [protocol v1](protocol-v1.md), [machine output v1](machine-output-v1.md) |
| Wrong occurrence edited in Emacs | Return typed recurrence identity, query timezone, fingerprint, and a complete stored `series_master` target. | [`test_server_e2e`](../test/test_server_e2e.ml), [`test-caledonia.el`](../emacs/test-caledonia.el) |
| Alarm replay or duplicate delivery | Persist a stable fire key, use a watermark and bounded retry/recovery windows, and reconcile on watcher failures/overflow. | [`test_daemon`](../test/test_daemon.ml), [`test_alarms`](../test/test_alarms.ml), [recovery guide](data-recovery.md) |
| Model regresses into aliases/caches | Reject stored identity caches, document component caches, parallel kind/time/sort models, and domain storage dependencies. | [77-requirement gate](../test/architecture/check_architecture.ml), [eight negative fixtures](../test/architecture/negative) |

## Compatibility and intentional changes

The following contracts are preserved:

- vdir-style layout and existing calendar content;
- CLI command names and successful human behavior;
- protocol version 1 grammar and correlation rules;
- machine-output schema version 1;
- alarm-state schema version 2 readability and no-replay behavior;
- occurrence edit/delete semantics and Emacs transport behavior.

Intentional changes that reviewers should not treat as accidental drift:

- pre-1.0 OCaml APIs that exposed complete calendars, duplicate aliases, hidden
  clocks, or occurrence-as-stored behavior were removed or narrowed;
- machine formats are now explicit version-1 contracts rather than the older
  ad-hoc shapes;
- malformed registered RFC properties, unknown TZIDs that must be evaluated,
  invalid durations, unsupported recurrence forms, and missing mutation
  fingerprints fail explicitly instead of degrading silently;
- `emacs/caledonia-event.el` was folded into the single protocol-aware
  `emacs/caledonia.el` client;
- the package version is 0.5.0, minimum OCaml is 5.1, and the pinned
  `icalendar.dev` dependency is unconditional.

## Concurrency guarantee

The per-calendar advisory lock serializes cooperating Caledonia writers. Exact
target-file fingerprint comparison detects non-cooperating changes to the
target. Calendar-wide todo validation also rechecks participating sibling
fingerprints immediately before the target operation.

This is not a filesystem transaction over arbitrary external processes. A
process that ignores the advisory lock can modify a sibling after verification;
that later write is a new external commit. The implementation must not claim it
can prevent it.

## Upstream `origin/vibing` decision

All 11 commits after the review base were examined. Ten behavior changes are
accepted or superseded by the larger implementation: shared RNG state, explicit
time, half-open ranges, all-day civil-date formatting, UTC recurrence UNTIL,
component terminology, centralized S-expression/output behavior, and the
associated tests. They overlap the rewrite and should not be cherry-picked.

`8e4f50d` is intentionally rejected because silently falling back for an
unknown TZID or invalid duration would hide invalid authored data. The exact
per-commit disposition is in the
[upstream reconciliation table](whole-diff-data-flow-audit.md#upstream-reconciliation).

## Verification evidence

The settled worktree was verified on 2026-07-16 with:

```sh
opam exec -- dune build @all @fmt
opam exec -- dune runtest --force
env TZ=UTC opam exec -- dune runtest --force
env TZ=Asia/Tokyo opam exec -- dune runtest --force
opam lint caledonia.opam

emacs -Q --batch -L emacs \
  --eval '(setq byte-compile-error-on-warn t)' \
  -f batch-byte-compile emacs/caledonia.el emacs/caledonia-evil.el
emacs -Q --batch -L emacs -l emacs/test-caledonia.el \
  -f ert-run-tests-batch-and-exit
```

The forced suite includes the 77-requirement architecture gate, eight
compile-failure interface fixtures, process-level CLI integration, server E2E,
storage/export/recurrence/query/alarm/daemon tests, and watcher tests. The ERT
suite has 25 tests.

A clean Debian 12 / OCaml 5.2 environment also completed `@all`, `@fmt`, and
the forced suite with `inotify.2.6` selected, including the real recursive
watcher lifecycle. A temporary-prefix install verified `caled --version` and
the installed Emacs artifacts. GitHub workflow YAML parsing, `git diff --check`,
and generated-artifact cleanup also passed.

CI repeats the supported OCaml matrix and Emacs 27.1/30.2 checks. Linux/inotify
is the deployment-critical path; the macOS job principally protects the
portable build and polling fallback.

## Reviewer checklist

- [ ] I can identify the single owner of physical calendar data, source data,
  domain bodies, occurrences, and wire DTOs.
- [ ] Every mutation reaches `Calendar_dir` with a stored target and source-free
  replacement, then returns a canonical reload.
- [ ] No occurrence can enter an ordinary stored mutation by type confusion.
- [ ] Rewrites retain content Caledonia does not semantically own.
- [ ] DATE, floating, UTC, and TZID values are not prematurely flattened.
- [ ] Protocol/output compatibility is asserted at versioned boundaries.
- [ ] Alarm scheduling and persistence do not depend on notification text.
- [ ] Linux watcher failures and overflow cause reconciliation rather than
  silent missed alarms.
- [ ] New shared utilities remove identical policy, not merely similar-looking
  code with different error semantics.
- [ ] Documentation claims match the actual advisory-lock and external-writer
  limits.

## Non-goals and known limitations

- CalDAV synchronization remains the responsibility of a tool such as
  vdirsyncer.
- Embedded custom VTIMEZONE blocks are preserved, but arbitrary embedded rules
  are not evaluated when the system timezone database lacks the TZID.
- Recurring VTODO/VJOURNAL and `RANGE=THISANDFUTURE` remain explicit unsupported
  capabilities.
- The polling watcher exists for portability; this change does not optimize
  macOS-specific filesystem watching.
- The parser compatibility layer remains until the pinned upstream library can
  pass the preservation matrix without it.
