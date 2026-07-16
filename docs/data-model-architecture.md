# Caledonia Data-Model Architecture

- Status: Implemented architecture contract after whole-diff third audit
- Source of requirements: `docs/data-model-simplification-prd.md`
- Machine-readable status: `docs/data-model-requirements.json`
- Last updated: 2026-07-16

## Purpose

This document defines the ownership and dependency boundaries for the data-model
simplification. It describes the implemented architecture and the dependency
direction enforced by the build and architecture tests. The requirement ledger
records the concrete evidence for each accepted requirement; behavioral tests
alone are not treated as architectural evidence.

The design has four values with deliberately different lifecycles:

- a `Document` is one immutable snapshot of one physical `.ics` file;
- a domain body or event series is validated RFC data without storage metadata;
- a stored component is a validated view of a body plus its document source;
  local identity is derived from the immutable body;
- an event occurrence is a derived query value and is never a stored component.

## Ownership model

```text
Calendar_dir / repository
  |
  +-- owns Document snapshots, one per physical .ics file
        |
        +-- Source (calendar key, display name, file, fingerprint)
        +-- VCALENDAR properties and VTIMEZONE entries
        +-- ordered known and opaque child entries
        +-- private parser/writer compatibility metadata
        +-- validated logical projection
              |
              +-- Stored Event.t (one authored series)
              +-- Stored Todo.Body
              +-- Stored Journal.Body

Event.t --bounded expansion--> Event.Occurrence

Stored component + Patch --> replacement source-free body/series
Stored target + replacement --> repository document mutation --> canonical reload
```

Only the document/codec/storage boundary may own a complete parsed physical
calendar. A domain value may expose a raw supported component body for explicit
interoperability, but it must not expose a complete calendar, opaque codec
entries, or parser compatibility metadata.

## Normative invariants

1. One `Document` owns each parsed physical VCALENDAR and all codec metadata.
2. Source metadata is represented once by an abstract, immutable source value.
3. Drafts and bodies have no source. Stored values always have a non-optional
   source fingerprint.
4. A local component identity is shared `kind + UID + optional RECURRENCE-ID`.
   It is derived from the immutable stored body. Source plus local identity is a
   complete write target.
5. Display names, summaries, and query presentation fields never participate in
   write identity.
6. `Event.t` owns one master and all same-document, same-UID authored
   overrides. Series construction performs cardinality and temporal validation.
7. `Event.Occurrence` retains both authored recurrence identity and effective
   event fields. It cannot be supplied to a general stored mutation function.
8. Domain editing returns a replacement body or series and never mutates or
   simulates a replacement document snapshot.
9. Repository mutation reloads and conflict-checks current state itself. It does
   not accept an application-owned component cache as mutation state.
10. Opaque components, VTIMEZONE definitions, unmodified known siblings, and
    their meaningful order survive supported mutations.
11. Private parser compatibility state is not observable through domain getters,
    property iteration, machine output, protocol source fields, or alarm keys.
12. Wire DTOs may preserve versioned syntax, but production has one kind, one
    identity model, one patch algebra, and one alarm-fire identity model.

## Module responsibilities and dependency direction

| Layer | Owns | May depend on | Must not own or depend on |
| --- | --- | --- | --- |
| Foundational algebra | `Patch`, component kind, local identity, typed storage errors | OCaml stdlib and narrow value libraries such as `Icalendar`/`Ptime` where required | Eio, repository/storage, full documents, presentation |
| Shared target and fire identity | abstract source plus local identity target; named fire identity | foundational algebra and abstract source/target accessors | direct Eio operations, repository mutation, full documents, presentation |
| Domain | event/todo/journal bodies, series validation, typed field access and edits | foundational algebra, dates, RFC validation, alarms | Eio paths, `Calendar_dir`, source metadata, full calendars, CLI/protocol formatting |
| Recurrence | occurrence expansion, inheritance, occurrence references and mutations | domain series and date/time policy | filesystem state, stored mutation entry points |
| Codec/document | lexical compatibility, ordered known/opaque entries, full VCALENDAR serialization | upstream iCalendar library and domain validators | CLI/protocol semantics, caller cache mutation |
| Repository/storage | source snapshots, path confinement, reload, graph validation, locking, atomic mutation, canonical reload | document/codec and domain replacements | presentation and wire DTO syntax |
| Query/presentation | typed query criteria, `Stored | Occurrence` views, text/JSON/CSV/ICS/S-expression output | stored views, occurrences, explicit document export context | document mutation ownership |
| Boundary adapters | CLI and protocol parsing, exact versioned encodings, Emacs transport | query/presentation and repository APIs | competing domain algebra or raw document ownership |

Dependencies point downward through this table. In particular, a new body or
series construction module must not import Eio, `Calendar_dir`, or
`Component_source`. `Component_source` is intentionally storage-side because it
contains an Eio path; its existence does not make domain construction impure.

## Document and codec boundary

The target `Document.t` is abstract. Conceptually it owns:

```ocaml
type entry =
  | Known of known_component
  | Opaque of opaque_component

type t
```

The actual representation may include physical positions, indexes, and private
compatibility records. Those details must not be encoded as ordinary public
calendar properties above the codec boundary.

The codec is responsible for:

- envelope and lexical validation;
- case normalization required by the pinned parser;
- DATE-valued RRULE UNTIL recovery;
- preservation of physically repeated VALARMs;
- writer property-name corrections;
- authenticated storage and restoration of opaque source blocks;
- CRLF, folding, and canonical serialization contracts.

Domain modules are responsible for supported component semantics. Storage calls
both validation layers and preserves their distinct error classes.

`X-CALEDONIA-CLEARED` requires explicit classification: it represents authored
override inheritance-clearing semantics, not an authenticated parser surrogate.
It must not be removed as though it were the DATE-UNTIL, alarm, or opaque codec
marker. Any change to that extension requires a separately documented data and
protocol compatibility decision.

## Stored lifecycle and mutation flow

Creation and mutation have different inputs but one repository-owned output:

```text
Create:
  source-free draft/body
    -> validate calendar key
    -> choose confined filename and VCALENDAR envelope
    -> atomic create
    -> parse installed bytes
    -> canonical Stored value

Edit:
  original Stored target + replacement body/series
    -> validate unchanged identity
    -> lock and reload current document
    -> verify expected fingerprint
    -> validate current affected-calendar invariants
    -> replace exact logical entry/series in ordered document
    -> serialize, verify, backup, atomically install
    -> canonical reload result

Delete:
  original Stored target
    -> same reload/conflict/validation path
    -> remove exact entry or complete event series
    -> Removed_document | Rewritten_document canonical result
```

Todo parent validation is calendar-scoped rather than file-scoped. Cooperating
Caledonia writers are serialized by the calendar lock, and every participating
snapshot fingerprint is revalidated immediately before the target operation.
The target file also has compare-and-swap protection. A non-cooperating process
can write a sibling after verification; POSIX provides no atomic transaction
over arbitrary external writers, so this is documented as a later external
commit rather than a freshness guarantee Caledonia can enforce.

The transaction implementation must retain the existing path-confinement,
advisory-lock, optimistic-fingerprint, temporary-write, fsync, parse/readback,
backup, atomic-install, and cleanup guarantees.

## Series, occurrences, and query views

Series grouping is scoped to one physical document. Equal UIDs in different
documents are unrelated stored series.

An occurrence contains the authored recurrence identity as well as its effective
start/end and inherited properties. For a moved override, the authored
`RECURRENCE-ID` remains the mutation target even though the effective start is
different. DATE and floating recurrence identities also retain the timezone
policy used by the selecting query.

Bounded query output uses an explicit sum:

```ocaml
type query_item =
  | Stored of Component.t
  | Occurrence of {
      stored_series : Component.t;
      occurrence : Event.Occurrence.t;
    }
```

Stored-series ICS export emits the authored master and persisted overrides under
the existing contract. Occurrence export emits only the requested effective
occurrence. Both receive immutable document export context explicitly so the
required non-conflicting VTIMEZONE definitions can be selected without embedding
a full calendar in each component.

## Alarm identity compatibility

The in-memory fire key is a named record, while persisted state schema v2 keeps
its established positional JSON syntax until a separately versioned migration is
approved. A new typed identity must remain semantically equivalent to the old
calendar/file/UID/recurrence/alarm-index/alarm/fire-time key.

The old recurrence and alarm digests were derived from canonical serialized
content. Migration must therefore retain enough authored recurrence parameters
and alarm ordering to recognize existing state-v2 keys without serializing and
scanning a complete event. Duplicate equal alarms remain distinct by ordinal.

## Boundary compatibility

The internal migration does not change:

- vdir-style file placement, backup/lock/temporary naming, or read-only behavior;
- CLI command names, flags, exit classifications, or successful output behavior;
- JSON, CSV, ICS, and S-expression schema version 1;
- protocol version 1 grammar, including `Keep`, `Clear`, and `(Set VALUE)`;
- protocol source identity fields or Emacs unchanged/occurrence behavior;
- alarm state schema version 2 readability and no-replay behavior;
- the general watcher interface, with Linux inotify as the primary deployment
  backend.

Library APIs are pre-1.0 and may be intentionally removed. Any temporary source
compatibility adapter belongs in a named `Legacy` module, has a removal target,
and has no production Caledonia caller before the migration is complete.

## Enforced migration ratchets

`test/architecture/check_architecture.ml` and
`test/architecture/check_negative_interfaces.ml` are wired into `dune runtest`.
They enforce:

- the requirement ledger contains exactly the 77 PRD IDs and internally
  consistent status counts;
- completed requirements cannot omit evidence;
- foundational algebra modules do not import filesystem, repository, codec, or
  presentation layers;
- no old `CEvent`/`CTodo`/`CJournal` kind algebra is reintroduced;
- the protocol patch remains a true `Patch.t` re-export;
- raw upstream parsing/writing is confined to `Calendar_codec`;
- production has no caller of `Calendar_codec.Legacy`;
- Event, Todo, and Journal have zero source, filesystem, codec, presentation,
  or S-expression dependency;
- repository mutation interfaces accept stored targets and source-free bodies,
  never occurrences or caller component lists;
- forbidden interface fixtures fail to compile when an occurrence is supplied
  to stored mutation or a domain body is treated as source-aware.

## Compatibility quarantine

`Calendar_codec.Legacy` is the only remaining augmented-calendar adapter. It is
formally deprecated, used solely by compatibility characterization tests,
prohibited in all production modules by the architecture gate, and scheduled
for removal with the next major release or earlier when the pinned upstream
parser can be tested entirely through the abstract document API. It is not an
alternate production document model.

## Delivery sequence and evidence policy

The implementation was delivered in this order:

1. architecture contract and boundary characterization;
2. shared leaf types and exact wire adapters;
3. abstract document and contained codec metadata;
4. source-free bodies and explicit event series;
5. stored views, grouped loading, and explicit export context;
6. repository-owned creation/mutation and canonical reload;
7. one occurrence/query/output/protocol/Emacs/alarm vertical migration;
8. legacy removal, public interfaces, dependency gates, and clean Linux proof.

All 77 ledger entries became `complete` only after their runtime
implementation, public interface, tests, compatibility evidence, cleanup, and
independent audits were present. The whole-diff call graph, accepted utility
boundaries, external-writer limits, and upstream commit decisions are recorded
in `docs/whole-diff-data-flow-audit.md`.
