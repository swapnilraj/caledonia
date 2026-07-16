type body = Event of Event.t | Todo of Todo.t | Journal of Journal.t
type t = { source : Component_source.t; body : body }

module Identity_set = Set.Make (struct
  type t = Component_identity.t

  let compare = Component_identity.compare
end)

let component_type_body = function
  | Event _ -> Component_kind.Event
  | Todo _ -> Component_kind.Todo
  | Journal _ -> Component_kind.Journal

let event_body event = Event event
let todo_body todo = Todo todo
let journal_body journal = Journal journal

let event_of_body = function
  | Event event -> Some event
  | Todo _ | Journal _ -> None

let body t = t.body
let component_type t = component_type_body t.body

let body_components = function
  | Event event ->
      Event.authored_events event |> List.map (fun event -> `Event event)
  | Todo todo -> [ `Todo (Todo.to_ical_todo todo, Todo.get_alarms todo) ]
  | Journal journal -> [ `Journal (Journal.to_ical_journal journal) ]

let body_identity body =
  match body_components body with
  | component :: _ -> Component_identity.of_ical_component component
  | [] -> None

let identity_of_body body =
  match body_identity body with
  | Some identity -> identity
  | None -> invalid_arg "Supported component body has no writable UID"

let ical_components_of_body = body_components

let stored_of_decoded_body ~source body =
  match body_identity body with
  | Some _ -> Ok { source; body }
  | None -> Error (`Msg "Supported component body has no writable UID")

let to_event t = match t.body with Event event -> Some event | _ -> None
let to_todo t = match t.body with Todo todo -> Some todo | _ -> None

let to_journal t =
  match t.body with Journal journal -> Some journal | _ -> None

let body_id = function
  | Event e -> Event.get_id e
  | Todo t -> Todo.get_id t
  | Journal j -> Journal.get_id j

let get_id t = body_id t.body

let body_summary = function
  | Event e -> Event.get_summary e
  | Todo t -> Todo.get_summary t
  | Journal j -> Journal.get_summary j

let get_summary t = body_summary t.body

let body_description = function
  | Event e -> Event.get_description e
  | Todo t -> Todo.get_description t
  | Journal j -> Journal.get_description j

let get_description t = body_description t.body

let body_categories = function
  | Event e -> Event.get_categories e
  | Todo t -> Todo.get_categories t
  | Journal j -> Journal.get_categories j

let get_categories t = body_categories t.body
let get_source t = t.source
let get_calendar_name t = Component_source.display_name t.source
let get_calendar_key t = Component_source.calendar_key t.source
let get_source_fingerprint t = Component_source.fingerprint t.source
let get_file t = Component_source.file t.source

let body_alarms = function
  | Event e -> Event.get_alarms e
  | Todo t -> Todo.get_alarms t
  | Journal _ -> []

let get_alarms t = body_alarms t.body

let body_start_result ~floating_tz = function
  | Event event ->
      Event.get_start_result ~floating_tz event |> Result.map Option.some
  | Todo todo -> (
      match Todo.get_start_time todo with
      | Some _ -> Todo.get_start_result ~floating_tz todo
      | None -> Todo.get_due_result ~floating_tz todo)
  | Journal journal -> Journal.get_start_result ~floating_tz journal

let get_start_result ~floating_tz t = body_start_result ~floating_tz t.body

let body_ical_component = function
  | Event e -> `Event (Event.master e)
  | Todo t -> `Todo (Todo.to_ical_todo t, Todo.get_alarms t)
  | Journal j -> `Journal (Journal.to_ical_journal j)

let get_identity t = identity_of_body t.body

let get_recurrence_id_property component =
  let find properties =
    List.find_map
      (function `Recur_id (params, value) -> Some (params, value) | _ -> None)
      properties
  in
  match body_ical_component component.body with
  | `Event event -> find event.props
  | `Todo (properties, _) -> find properties
  | `Journal properties -> find properties
  | `Freebusy _ | `Timezone _ -> None

let get_target component =
  Component_target.create ~source:component.source
    ~identity:(get_identity component)

let stored_views_of_decoded_components ?authored_events ~source components =
  let events =
    components
    |> List.filter_map (function `Event event -> Some event | _ -> None)
  in
  let ( let* ) = Result.bind in
  let* events =
    match authored_events with
    | None -> Event.of_events_result events
    | Some authored when List.map fst authored = events ->
        Event.of_authored_events_result authored
    | Some _ ->
        Error
          (`Msg
             "Document VEVENT metadata does not match its typed component order")
  in
  let rec decode_todos accumulated = function
    | [] -> Ok (List.rev accumulated)
    | `Todo body :: rest ->
        let* todo = Todo.of_ical_body body in
        decode_todos (todo :: accumulated) rest
    | (`Event _ | `Journal _ | `Freebusy _ | `Timezone _) :: rest ->
        decode_todos accumulated rest
  in
  let rec decode_journals accumulated = function
    | [] -> Ok (List.rev accumulated)
    | `Journal body :: rest ->
        let* journal = Journal.of_ical_body body in
        decode_journals (journal :: accumulated) rest
    | (`Event _ | `Todo _ | `Freebusy _ | `Timezone _) :: rest ->
        decode_journals accumulated rest
  in
  let* todos = decode_todos [] components in
  let* journals = decode_journals [] components in
  let bodies =
    List.map event_body events @ List.map todo_body todos
    @ List.map journal_body journals
  in
  let rec collect seen accumulated = function
    | [] -> Ok (List.rev accumulated)
    | body :: rest ->
        let* stored = stored_of_decoded_body ~source body in
        let identity = get_identity stored in
        if Identity_set.mem identity seen then
          Error
            (`Msg "Document contains a duplicate writable component identity")
        else
          collect (Identity_set.add identity seen) (stored :: accumulated) rest
  in
  collect Identity_set.empty [] bodies
