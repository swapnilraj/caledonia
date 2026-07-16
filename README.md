# Caledonia

Caledonia is a calendar client with command-line and Emacs front-ends.
It operates on a [vdir](https://pimutils.org/specs/vdir/) directory of [`.ics`](https://datatracker.ietf.org/doc/html/rfc5545) files as managed by tools like [vdirsyncer](https://github.com/pimutils/vdirsyncer), which allows it to interact with CalDAV servers.

The command-line has the `list`, `search`, `show`, `add`, `delete`, and `edit`
subcommands, with explicit support for DATE values, UTC and floating local
times, and IANA TZIDs.

Todos may use either an absolute due time or a duration from their start time:

```sh
caled add --type todo "Focus block" --calendar work \
  --date 2026-07-15 --time 09:00 --timezone Europe/London --duration 2h
caled edit <todo-id> --clear duration --due 2026-07-15 --due-time 12:00
```

`--duration` accepts compound weeks, days, hours, minutes, and seconds (for
example `1h30m`). It requires `--date`/`DTSTART` and is mutually exclusive
with `--due`. `caled edit <todo-id> --clear duration` removes it explicitly.

An example `list` invocation is,

```
$ caled list
personal   2025-04-04 Fri 13:00 - 14:00 (America/New_York) New York 8am meeting      054bb346-b24f-49f4-80ab-fcb6040c19a7
family     2025-04-06 Sun 21:00 - 22:00 (UTC)              Family chat @Video call   3B84B125-6EFC-4E1C-B35A-97EFCA61110E
work       2025-04-09 Wed 15:00 - 16:00 (Europe/London)    Weekly Meeting            4adcb98dfc1848601e38c2ea55edf71fab786c674d7b72d4c263053b23560a8d
personal   2025-04-10 Thu 11:00 - 12:00 (UTC)              Dentist                   ccef66cd4d1e87ae7319097f027f8322de67f758
family     2025-04-13 Sun 21:00 - 22:00 (UTC)              Family chat @Video call   3B84B125-6EFC-4E1C-B35A-97EFCA61110E
personal   2025-04-15 Tue - 2025-04-17 Thu                 John Doe in town          33cf18ec-90d3-40f8-8335-f338fbdb395b
personal   2025-04-15 Tue 21:00 - 21:30 (UTC)              Grandma call              8601c255-65fc-4bc9-baa9-465dd7b4cd7d
work       2025-04-16 Wed 15:00 - 16:00 (Europe/London)    Weekly Meeting            4adcb98dfc1848601e38c2ea55edf71fab786c674d7b72d4c263053b23560a8d
personal   2025-04-19 Sat                                  Jane Doe's birthday       7hm4laoadevr1ene8o876f2576@google.com
family     2025-04-20 Sun 21:00 - 22:00 (UTC)              Family chat @Video call   3B84B125-6EFC-4E1C-B35A-97EFCA61110E
personal   2025-04-22 Tue 21:00 - 21:30 (UTC)              Grandma call              8601c255-65fc-4bc9-baa9-465dd7b4cd7d
work       2025-04-23 Wed 15:00 - 16:00 (Europe/London)    Weekly Meeting            4adcb98dfc1848601e38c2ea55edf71fab786c674d7b72d4c263053b23560a8d
family     2025-04-27 Sun 21:00 - 22:00 (UTC)              Family chat @Video call   3B84B125-6EFC-4E1C-B35A-97EFCA61110E
personal   2025-04-29 Tue 21:00 - 21:30 (UTC)              Grandma call              8601c255-65fc-4bc9-baa9-465dd7b4cd7d
work       2025-04-30 Wed 15:00 - 16:00 (Europe/London)    Weekly Meeting            4adcb98dfc1848601e38c2ea55edf71fab786c674d7b72d4c263053b23560a8d
```

The Emacs client is defined in [./emacs](./emacs) and communicates with
`caled server` using the versioned, request-correlated S-expression protocol
documented in [docs/protocol-v1.md](docs/protocol-v1.md).

Scripts should use the stable [machine-output version 1](docs/machine-output-v1.md)
contract rather than parsing human-oriented output.

See [TODO](./TODO.org) for future plans.

## Installation

With [opam](https://opam.ocaml.org/),

```
$ opam install . --with-test
```

With [Nix](https://nixos.org/),

```
$ nix shell 'git+https://tangled.sh/@ryan.freumh.org/caledonia?ref=main'
```

Caledonia currently pins `icalendar.dev` to RyanGibb/icalendar commit
`0722a633ed8df8f5ec2fa78d29aa7a8b31687f39`. The fork contains parser fixes
needed to retain repeated RFC 5545 properties such as multiple `EXDATE`
values, but it still deduplicates equal `VALARM` blocks, reverses some repeated
fields, rejects unknown component types, and writes `PERCENT`, `RELATED`, and
`RESOURCE` instead of the standard `PERCENT-COMPLETE`, `RELATED-TO`, and
`RESOURCES` names. All production parsing and serialization therefore passes
through `Calendar_codec`, which isolates these workarounds, preserves validated
unknown blocks opaquely, removes private parse markers, and emits canonical
property names. A shared component boundary also rejects malformed registered
properties that the fork would otherwise demote to generic IANA extensions;
unregistered IANA/X extensions and the documented `PERCENT-COMPLETE`,
`RELATED`, and `RESOURCE` compatibility forms remain supported only where the
corresponding component field is valid. Regression tests cover parse/write/parse
stability. The exact fork revision is recorded in `caledonia.opam`; both the pin
and codec workarounds should be removed once an upstream release passes that
matrix.

## Configuration

Caledonia looks for calendars in the directory specified by the `CALENDAR_DIR`
environment variable or in `~/.calendar/` by default.

Recurring VEVENT series, exact-instance overrides, EXDATE, and RDATE are
supported. Recurring VTODO/VJOURNAL and `RECURRENCE-ID;RANGE=THISANDFUTURE` are
rejected with an explicit unsupported-capability error so queries and alarms do
not silently return incomplete schedules.

VEVENT recurrence values are validated before they can be serialized or
expanded. Invalid RRULE counts, intervals, BY-part ranges or combinations,
UNTIL representations, and registered parameters on RRULE, EXDATE, RDATE, or
RECURRENCE-ID cause the containing operation or calendar load to fail with a
typed validation error. Unknown IANA and X parameters remain supported.

Unknown TZIDs and embedded custom `VTIMEZONE` definitions are preserved when
calendar data is rewritten, but custom rules cannot yet be evaluated into
instants. Operations that require that conversion return an explicit error;
see the tracked limitation in [TODO.org](TODO.org).

## Tests

The project includes a test suite that can be run with `opam exec -- dune runtest`.
The Emacs frontend is checked with:

```sh
emacs -Q --batch -L emacs -f batch-byte-compile \
  emacs/caledonia.el emacs/caledonia-evil.el
emacs -Q --batch -L emacs -l emacs/test-caledonia.el \
  -f ert-run-tests-batch-and-exit
```

The portable command-line client builds on Linux and macOS. On Linux,
`alarm-daemon` watches the full calendar tree with inotify and reconciles with
periodic scans. The watcher mirrors the loader's visible-file scope, so hidden
state, lock, backup, and temporary files cannot trigger a feedback rescan;
other platforms use the same scan as a portable fallback. The
notifier backend is configurable; see `caled alarm-daemon --help`.
The backup retention, restore procedure, alarm watermark, retry, and
deduplication policies are documented in
[docs/data-recovery.md](docs/data-recovery.md).

Internally, one immutable document snapshot owns each physical `.ics` file.
Event, todo, and journal bodies contain no filesystem state; stored components
add one shared source and derive identity from their immutable body; generated
occurrences remain non-writable query values. The enforced ownership model is documented in
[docs/data-model-architecture.md](docs/data-model-architecture.md).

## Development and review

The [documentation map](docs/README.md) routes contributors to the current
contracts. Reviewers of the 0.5.0 remediation should begin with the
[PR review guide](docs/pr-review-guide.md); it gives the recommended review
order, compatibility and risk matrix, upstream-commit decisions, verification
evidence, and links to the complete call graphs. Coding agents should also read
[AGENTS.md](AGENTS.md) and the detailed [repository guide](CLAUDE.md) before
changing an ownership or wire boundary.

## Thanks

To [Patrick](https://patrick.sirref.org/) for suggesting the name, and all the
developers of the dependencies used, especially
[icalendar](https://github.com/robur-coop/icalendar).
