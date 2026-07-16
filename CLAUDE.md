# Repository guide

Caledonia is an OCaml calendar client with CLI and Emacs frontends. It operates
on vdir-style directories of RFC 5545 `.ics` files and composes with CalDAV
synchronizers such as vdirsyncer.

Start broad reviews with `docs/pr-review-guide.md`. The current ownership
contract is `docs/data-model-architecture.md`, complete function-level flows
are in `docs/whole-diff-data-flow-audit.md`, and `docs/README.md` maps user,
operator, protocol, and historical design documents. `AGENTS.md` is the compact
task-routing guide for coding agents.

## Build and verification

Use the project-local opam environment so an unrelated system `dune` is not
selected:

```bash
opam exec -- dune build @all @fmt
opam exec -- dune runtest --force
opam install . --deps-only --with-test
opam install . --with-test
```

Warnings are build errors. Formatting is pinned by `.ocamlformat`; run
`opam exec -- dune fmt` only from a stable worktree, or use targeted
`opam exec -- ocamlformat -i FILE...` while other edits are in flight. Emacs
Lisp has strict byte-compilation and ERT coverage under `emacs/dune`.

## Architecture

- `lib/calendar_codec.ml` is the compatibility boundary around the pinned
  iCalendar library. It validates envelopes, preserves opaque/repeated content,
  and contains authenticated lexical round-trip workarounds.
- `lib/calendar_document.ml` owns one abstract immutable snapshot per physical
  VCALENDAR, including source metadata, ordered codec entries, and decoded
  stored views. `lib/calendar_export.ml` performs selected export with explicit
  immutable document context.
- `lib/calendar_dir.ml` owns confined vdir discovery, complete-document loads,
  graph validation, optimistic fingerprints, locks, backups, and atomic writes.
- `lib/event.ml`, `lib/todo.ml`, and `lib/journal.ml` validate typed component
  bodies without source or filesystem state. Event owns validated authored
  series, nominal occurrences, typed references, and recurrence mutation.
- `lib/component.ml` provides immutable stored views; `lib/component_query.ml`
  provides the single `Stored | Occurrence` query-item pipeline.
- `lib/date.ml` owns DATE/UTC/floating/TZID conversion and RFC DST policy using
  Timedesc. `lib/patch.ml` encodes Keep/Clear/Set mutation intent.
- `lib/alarm.ml`, `lib/alarm_query.ml`, and `lib/alarm_daemon_core.ml` own one
  generic fire model, source attachment, delivery identity, persisted
  retry/recovery state, and replay bounds.
- `lib/sexp.ml` defines server protocol version 1 and its typed wire schema.
- `bin/output.ml` owns JSON/CSV/ICS/S-expression schema version 1 and human
  presentation. `bin/*_cmd.ml` implement Cmdliner commands.
- `bin/alarm_watcher_inotify.ml` is the primary recursive Linux watcher behind
  `alarm_watcher.mli`; `alarm_watcher_poll.ml` is the portable fallback.
- `emacs/caledonia.el` is a correlated, bounded protocol client with typed event
  forms. `emacs/caledonia-evil.el` adds optional Evil bindings.

Eio provides structured concurrency and filesystem access. Tests use
`ppx_expect`, process-level CLI integration, server E2E tests, watcher/daemon
tests, and ERT. Calendar fixtures live under `test/calendar/`.

## Where changes belong

| Concern | Owning module or boundary |
| --- | --- |
| RFC body validation and source-free edits | `Event`, `Todo`, `Journal`, `Alarm` |
| Calendar-time resolution and comparison | `Date` |
| Physical/parser compatibility | `Calendar_codec` |
| Immutable one-file snapshot | `Calendar_document` |
| Filesystem reads and mutations | `Calendar_dir` |
| Stored/occurrence filtering and sorting | `Component_query` |
| Human and machine rendering | `bin/output` |
| Protocol grammar and transport | `Sexp`, `server_cmd`, then Emacs together |
| Alarm scheduling/retry state | `Alarm_daemon_core` |
| Notification text and backend | `alarm_daemon_cmd` |

## Configuration and invariants

`CALENDAR_DIR` selects the calendar root; the default is `~/.calendar/`. Each
direct child directory is a stable calendar key. Display names are presentation
only and are never write identities.

Mutations must preserve the full source document and go through Calendar_dir;
never write component-only ICS over an existing file. Optional edits use
`Patch.t`, not an absent or empty value to mean “unchanged.” Machine formats and
protocol fields are versioned contracts; update their documentation and
compatibility tests whenever changing them.

The repository lock serializes cooperating Caledonia writers. Fingerprints
detect target-file conflicts and recheck calendar-wide todo snapshots, but no
claim is made that arbitrary external processes participate in an atomic
multi-file transaction.
