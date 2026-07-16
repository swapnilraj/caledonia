# Data recovery and alarm-daemon state

## Calendar mutation backups

Before replacing or deleting an existing calendar file, Caledonia writes a
mode-0600 sibling backup containing the exact previous bytes and syncs it to
disk. Its name is:

```text
.caledonia-backup-<original-name>-<timestamp>-<random-suffix>
```

The new calendar is parsed before persistence, written to an exclusive
temporary file, synced, and atomically renamed. The ten newest backups for each
source file are retained; older backups are best-effort pruned only after a
successful mutation. A failed mutation leaves the source and its backup in
place.

Recurring overrides may contain the private `X-CALEDONIA-CLEARED` property.
Its comma-separated value records inherited field families that the user
explicitly cleared (for example `SUMMARY`, `LOCATION`, `DESCRIPTION`,
`CATEGORIES`, `END`, or `VALARM`). RFC 5545 cannot otherwise distinguish an
intentionally absent override field from a sparse override that inherits the
master's value. Caledonia preserves this marker in ICS, does not show it as a
user property, and updates it through the same `Keep`/`Clear`/`Set` patch rules
as the represented fields. Removing it by hand can cause a later override edit
to inherit the corresponding master value.

To recover, stop writers and vdirsyncer, inspect the newest matching backup,
copy it to a separate safe location, validate it as iCalendar, and then replace
the damaged source using an atomic rename. Do not edit the backup in place.

## Alarm state

`caled alarm-daemon` stores versioned JSON in
`<calendar-root>/.caledonia-alarm-state.json`. It records the last successful
scan watermark and fired identities. State replacement is atomic and synced.
Fired identities include the stable calendar key, physical file, UID,
recurrence start, alarm value, and fire time. Successful identities are retained
for the complete active replay interval beginning at the persisted watermark,
then pruned only after the watermark advances beyond them. There is no fixed
24-hour cutoff: a partial notifier failure can hold the replay watermark for
longer without allowing earlier successful notifications in that interval to
fire again. Alarm values and recurrence IDs are hashed from deterministic RFC
5545 serialization, and the alarm's ordinal is included so adjacent identical
`VALARM` blocks remain distinct across restarts.

The daemon establishes its watcher before its first scan. On restart it resumes
from the persisted watermark, bounded by `--grace` (300 seconds by default),
and periodically performs a full recursive reconciliation. Linux uses inotify;
other platforms use the portable polling backend. Inotify overflow triggers
watch reconciliation and a full scan. Watch-set rebuilds are transactional: a
replacement descriptor is populated before the live descriptor is swapped and
closed. Snapshot creation is all-or-nothing for the accessible directory tree:
resource exhaustion or a read/watch failure in any nested directory rejects the
partial snapshot, while symbolic links remain intentional skips. If rebuilding
after queue overflow, a topology change, or root replacement fails, the daemon
reports the failure, retains the previous usable watch set, performs a full
scan, and retries the snapshot on the next safety timer rather than terminating.

Daemon reconciliation is deliberately fault-isolated by file. An unreadable or
malformed calendar file is reported and skipped while other valid files remain
eligible for notification; interactive reads and mutations remain fail-closed.
The Linux watcher rejects a symlink calendar root and does not follow symlinked
directories or files discovered below it. This prevents a watched tree from
silently expanding outside the configured calendar root.

A notification is recorded after its notifier exits successfully. Failed
delivery attempt counts are persisted and survive restart. The daemon makes at
most three attempts per fire. During retries, the watermark advances to the
oldest unresolved fire instead of retaining the beginning of the whole scan,
so successfully handled prefixes do not make the replay window grow forever.
The scan's lower bound is inclusive, so a fire at that exact watermark is
retried. If reconciliation no longer returns a pending alarm (for example,
because the source removed it), the daemon removes that pending entry. If the
third attempt fails, the daemon reports the notification as dropped and records
it as terminally handled; it will not retry that fire forever. This gives
bounded at-least-once retry and best-effort deduplication across restarts; no
filesystem state file can make an external desktop notifier and the state
update one atomic transaction.

Notifier subprocess deadlines use the daemon's injected monotonic clock. Wall
clock corrections therefore cannot make the ten-second notifier timeout expire
early or late.

If the state file is missing or malformed, the daemon falls back to the grace
window. Deleting it intentionally causes that bounded replay and may duplicate
recent notifications. If state persistence fails, the daemon reports the
error; correct the directory permissions before restarting to preserve restart
deduplication.

State files are created with mode 0600. The temporary file is synced before
rename, the parent directory is synced where the platform supports directory
`fsync`, and failed writes remove their temporary file.

## Parser compatibility boundary

RFC 5545 component names, property names, parameter names, and registered
enumerated tokens are ASCII case-insensitive. The pinned `icalendar` parser
compares several of them case-sensitively, so Caledonia normalizes a private
parser-facing copy of supported content lines. This includes known `BEGIN` and
`END` component names, property and parameter names, standard enumerations such
as `VALUE=DATE-TIME`, `STATUS=CONFIRMED`, and `RELATED=START`, plus RRULE part
names and recognized frequency/weekday values.

Normalization happens after unsupported component blocks have been protected
as opaque data. It never case-folds text payloads, calendar addresses, URIs,
unknown parameter values, or case-sensitive TZIDs such as `Europe/London`.
Serialized supported components use the library's canonical spelling, while
unsupported component blocks retain their protected source content.

This is a narrow lexical compatibility layer, not an iCalendar repair tool.
Malformed content lines and arbitrary DATE-TIME, DURATION, URI, or extension
values are passed through for normal validation and may still be rejected.
