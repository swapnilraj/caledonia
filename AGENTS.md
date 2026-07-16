# Agent instructions

These instructions apply to the whole repository. Caledonia is a calendar data
tool: preserving authored data and keeping write identity exact matter more
than shortening a call path.

## Read before changing code

1. Read [CLAUDE.md](CLAUDE.md) for build commands, module ownership, and global
   invariants.
2. Use the [documentation map](docs/README.md) to find the contract for the
   boundary you are changing.
3. For storage, domain-model, recurrence, query, alarm, or protocol work, read
   [data-model-architecture.md](docs/data-model-architecture.md).
4. For broad refactors or reviews, use the
   [PR review guide](docs/pr-review-guide.md) and the complete
   [data-flow audit](docs/whole-diff-data-flow-audit.md).

## Model to preserve

```text
.ics -> codec -> document -> stored component -> stored/occurrence query item
                     |               |
                     |               +-> boundary output
                     +<- repository mutation <- target + replacement body
```

- The codec/document layer alone owns complete physical VCALENDAR state.
- Event, todo, and journal bodies are source-free.
- A stored component owns one source and one body; identity is derived.
- An occurrence is a derived query value, never a stored mutation input.
- `Calendar_dir` owns reload, confinement, fingerprint checks, graph
  validation, locking, atomic installation, and canonical reload.
- Wire and machine-output DTOs stay at their versioned boundaries.
- Linux inotify is the primary watcher; the polling backend is a portable
  fallback behind `alarm_watcher.mli`.

Do not introduce generic entity/repository frameworks, parallel calendar-time
or kind algebras, cached derivable identity, or caller-owned document mutation
state. A new type is justified when it enforces a real lifecycle, ownership, or
compatibility boundary.

## Put changes in the owning layer

| Change | Owner |
| --- | --- |
| RFC body validation or source-free edit | `lib/event`, `lib/todo`, `lib/journal`, `lib/alarm` |
| Temporal resolution or comparison | `lib/date` |
| Pinned-parser workaround or physical preservation | `lib/calendar_codec` |
| One-file immutable snapshot | `lib/calendar_document` |
| Filesystem discovery or mutation | `lib/calendar_dir` |
| Stored/occurrence filtering and sorting | `lib/component_query` |
| Human or machine rendering | `bin/output` |
| Protocol grammar | `lib/sexp`, then `bin/server_cmd` and Emacs together |
| Alarm scheduling, retry, persisted state | `lib/alarm_daemon_core` |
| Desktop notification text/backend | `bin/alarm_daemon_cmd` |

## Verification

Run the smallest focused test while iterating, then before handoff run:

```sh
opam exec -- dune build @all @fmt
opam exec -- dune runtest --force
git diff --check
```

Protocol/Emacs changes also require warning-as-error byte compilation and ERT.
Temporal changes require UTC and a non-UTC full test run. Watcher or packaging
changes require a Linux environment with the inotify implementation selected.
Update the matching contract, changelog, and architecture evidence in the same
change; do not mark ledger work complete without code, interface, test, and
documentation evidence.
