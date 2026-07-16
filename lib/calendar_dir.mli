(** Functions for managing vdir-style calendar directories containing [.ics]
    files. *)

type t
(** A calendar repository whose immediate subdirectories are stable calendar
    keys. *)

type deletion_outcome = File_deleted | Document_rewritten of Component.t list

module For_test : sig
  type failure =
    | Post_write_mismatch
    | Temporary_parse_failure
    | Rename_failure

  val inject_next : failure -> unit
  (** Inject one failure into the next matching existing-file replacement. This
      narrow test seam is never enabled by production code. *)

  val inject_before_snapshot_verification : (unit -> unit) -> unit
  (** Run one test callback immediately before the next calendar snapshot is
      verified. This provides deterministic concurrency tests without timing
      assumptions and is never enabled by production code. *)

  val clear : unit -> unit
end

val create :
  fs:Eio.Fs.dir_ty Eio.Path.t -> string -> (t, [> `Msg of string ]) result
(** Create a calendar_dir from a directory path. Returns Ok with the
    calendar_dir if successful, or Error with a message if the directory cannot
    be created or accessed. *)

val of_path : string -> t
(** Construct a directory handle without accessing or creating the path. This is
    intended for building side-effect-free command metadata such as help and
    version output; commands that access calendars must use a handle returned by
    [create]. *)

val create_calendar :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  string ->
  (unit, [> `Msg of string ]) result
(** Explicitly create a validated calendar directory key. Component creation
    never creates an unknown calendar implicitly. *)

val get_display_name : fs:Eio.Fs.dir_ty Eio.Path.t -> t -> string -> string
(** Get the display name for a calendar from its displayname file, falling back
    to the directory name if the file doesn't exist. *)

val get_color : fs:Eio.Fs.dir_ty Eio.Path.t -> t -> string -> string option
(** Get the color for a calendar from its color file. Returns None if the file
    doesn't exist or is empty. *)

val resolve_calendar_key :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  string ->
  (string, [> `Msg of string | `Not_found ]) result
(** Resolve only an exact directory key. Display names are presentation-only and
    are never accepted as write identities. *)

val list_calendar_names :
  fs:Eio.Fs.dir_ty Eio.Path.t -> t -> (string list, [> `Msg of string ]) result
(** List the stable directory keys available in the calendar repository. *)

(** Component operations *)

val get_calendar_documents :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  string ->
  (Calendar_document.t list, [> `Msg of string | `Not_found ]) result
(** Load immutable document snapshots for one calendar key. Each physical .ics
    file has exactly one document owner. *)

val get_documents :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  (Calendar_document.t list, [> `Msg of string ]) result
(** Load immutable document snapshots for the repository. *)

val get_calendar_components :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  string ->
  (Component.t list, [> `Msg of string | `Not_found ]) result
(** Get all components in a calendar. *)

val get_components :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  (Component.t list, [> `Msg of string ]) result
(** Get all components in all calendars. *)

val get_components_tolerant :
  report:(string -> unit) ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  (Component.t list, [> `Msg of string ]) result
(** Load components for the alarm daemon while reporting and skipping malformed
    or unreadable individual calendar files. Root-directory discovery errors
    remain fatal. Interactive reads continue to use [get_components] and fail
    closed. *)

(** Successful replacements and deletions create hidden sibling backups named
    [.caledonia-backup-<source>-<timestamp>-<uuid>]. The ten newest backups per
    source file are retained; older backups are removed after a successful
    mutation. Lock files are persistent names protected by advisory OS locks, so
    process termination releases the lock without creating a stale blocker. *)

val create_stored_component :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  calendar_key:string ->
  Component.body ->
  (Component.t, Storage_error.t) result
(** Repository-owned create. The repository loads fresh graph state, writes the
    new document, and returns the canonical post-write stored value. *)

val replace_stored_component :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  original:Component.t ->
  replacement:Component.body ->
  (Component.t, Storage_error.t) result
(** Repository-owned replacement. [original] is the immutable write target;
    [replacement] supplies the validated body. No caller cache is mutation
    state. *)

val remove_stored_component :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  Component.t ->
  (deletion_outcome, Storage_error.t) result
(** Repository-owned deletion with an explicit physical-document outcome. *)

val delete_occurrence :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  Component.t ->
  Event.Occurrence.Reference.t ->
  (Component.t, Storage_error.t) result
(** Delete a single occurrence of a recurring event by adding EXDATE. *)

val add_occurrence_override :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  t ->
  Component.t ->
  Event.Occurrence.Reference.t ->
  Icalendar.event ->
  (Component.t, Storage_error.t) result
(** Add a RECURRENCE-ID override VEVENT to an existing recurring event's .ics
    file. *)

val get_path : t -> string
