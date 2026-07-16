type text_field = Summary | Description | Location | Categories
type sort_field = Start | End | Summary_sort | Location_sort | Calendar | Type
type sort_spec = { field : sort_field; descending : bool }

type item =
  | Stored of Component.t
  | Occurrence of {
      stored_series : Component.t;
      occurrence : Event.Occurrence.t;
    }

type criteria = {
  calendars : string list;
  component_types : Component_kind.t list;
  text : string option;
  text_fields : text_field list;
  categories : string list;
  id : string option;
  statuses : Icalendar.status list;
  completed : bool option;
  overdue : bool option;
  recurring : bool option;
  has_alarm : bool option;
}

let no_criteria =
  {
    calendars = [];
    component_types = [];
    text = None;
    text_fields = [];
    categories = [];
    id = None;
    statuses = [];
    completed = None;
    overdue = None;
    recurring = None;
    has_alarm = None;
  }

let ( let* ) = Result.bind

let timezone_of_name name =
  match Timedesc.Time_zone.make name with
  | Some timezone -> Ok timezone
  | None -> Error (`Msg (Printf.sprintf "Unknown timezone %S" name))

let conversion_error error = `Msg (Date.string_of_conversion_error error)
let lowercase = String.lowercase_ascii

let contains ~needle haystack =
  let expression = Re.(compile (no_case (str needle))) in
  Re.execp expression haystack

let equal_casefold left right = String.equal (lowercase left) (lowercase right)

let source = function
  | Stored component -> Component.get_source component
  | Occurrence { stored_series; _ } -> Component.get_source stored_series

let stored = function
  | Stored component -> Some component
  | Occurrence _ -> None

let occurrence = function
  | Stored _ -> None
  | Occurrence { occurrence; _ } -> Some occurrence

let component_type = function
  | Stored component -> Component.component_type component
  | Occurrence _ -> Component_kind.Event

let get_id = function
  | Stored component -> Component.get_id component
  | Occurrence { occurrence; _ } ->
      occurrence |> Event.Occurrence.reference |> Event.Occurrence.Reference.uid

let get_identity = function
  | Stored component -> Component.get_identity component
  | Occurrence { occurrence; _ } ->
      let reference = Event.Occurrence.reference occurrence in
      Component_identity.
        {
          kind = Component_kind.Event;
          uid = Event.Occurrence.Reference.uid reference;
          recurrence_id =
            Some (Event.Occurrence.Reference.recurrence_id reference);
        }

let get_target item =
  Component_target.create ~source:(source item) ~identity:(get_identity item)

let get_recurrence_id_property = function
  | Stored component -> Component.get_recurrence_id_property component
  | Occurrence { occurrence; _ } ->
      occurrence |> Event.Occurrence.reference
      |> Event.Occurrence.Reference.recurrence_id_property |> Option.some

let get_summary = function
  | Stored component -> Component.get_summary component
  | Occurrence { occurrence; _ } -> Event.Occurrence.get_summary occurrence

let get_description = function
  | Stored component -> Component.get_description component
  | Occurrence { occurrence; _ } -> Event.Occurrence.get_description occurrence

let get_categories = function
  | Stored component -> Component.get_categories component
  | Occurrence { occurrence; _ } -> Event.Occurrence.get_categories occurrence

let get_location = function
  | Stored component ->
      Option.bind (Component.to_event component) Event.get_location
  | Occurrence { occurrence; _ } -> Event.Occurrence.get_location occurrence

let get_alarms = function
  | Stored component -> Component.get_alarms component
  | Occurrence { occurrence; _ } -> Event.Occurrence.get_alarms occurrence

let get_calendar_key item = Component_source.calendar_key (source item)
let get_calendar_name item = Component_source.display_name (source item)
let get_source_fingerprint item = Component_source.fingerprint (source item)
let get_file item = Component_source.file (source item)
let status_of_string = Component_status.of_string

let status_component_types =
  [
    (`Tentative, [ Component_kind.Event ]);
    (`Confirmed, [ Component_kind.Event ]);
    ( `Cancelled,
      [ Component_kind.Event; Component_kind.Todo; Component_kind.Journal ] );
    (`Needs_action, [ Component_kind.Todo ]);
    (`Completed, [ Component_kind.Todo ]);
    (`In_process, [ Component_kind.Todo ]);
    (`Draft, [ Component_kind.Journal ]);
    (`Final, [ Component_kind.Journal ]);
  ]

let validate_criteria criteria =
  let selected_types =
    if criteria.component_types = [] then
      [ Component_kind.Event; Component_kind.Todo; Component_kind.Journal ]
    else criteria.component_types
  in
  let rec validate = function
    | [] -> Ok ()
    | status :: rest -> (
        match List.assoc_opt status status_component_types with
        | Some applicable
          when not
                 (List.exists
                    (fun component_type ->
                      List.mem component_type selected_types)
                    applicable) ->
            Error
              (`Msg
                 (Printf.sprintf
                    "status %S is not valid for the selected component types"
                    (Component_status.to_string status)))
        | Some _ -> validate rest
        | None -> assert false)
  in
  validate criteria.statuses

let event_status event =
  List.find_map
    (function `Status (_, status) -> Some status | _ -> None)
    (Event.master event).props

let get_status = function
  | Occurrence { occurrence; _ } ->
      let event = Event.Occurrence.effective_ical_event occurrence in
      List.find_map
        (function `Status (_, status) -> Some status | _ -> None)
        event.props
  | Stored component -> (
      match (Component.to_event component, Component.to_todo component) with
      | Some event, _ -> event_status event
      | _, Some todo -> Todo.get_status todo
      | _ -> Option.bind (Component.to_journal component) Journal.get_status)

let type_selected criteria component =
  criteria.component_types = []
  || List.mem (Component.component_type component) criteria.component_types

let calendar_selected criteria component =
  criteria.calendars = []
  || List.exists
       (String.equal (Component.get_calendar_key component))
       criteria.calendars

let identity_selected criteria component =
  match criteria.id with
  | None -> true
  | Some id -> String.equal id (Component.get_id component)

let recurring_selected criteria component =
  match criteria.recurring with
  | None -> true
  | Some expected -> (
      match Component.to_event component with
      | Some event -> Bool.equal expected (Event.has_recurrence_set event)
      | None -> not expected)

let invariant_selected criteria component =
  type_selected criteria component
  && calendar_selected criteria component
  && identity_selected criteria component
  && recurring_selected criteria component

let field_matches text item = function
  | Summary ->
      Option.fold ~none:false ~some:(contains ~needle:text) (get_summary item)
  | Description ->
      Option.fold ~none:false ~some:(contains ~needle:text)
        (get_description item)
  | Location ->
      Option.fold ~none:false ~some:(contains ~needle:text) (get_location item)
  | Categories -> List.exists (contains ~needle:text) (get_categories item)

let text_selected criteria item =
  match criteria.text with
  | None -> true
  | Some text ->
      let fields =
        match criteria.text_fields with
        | [] -> [ Summary; Description; Location; Categories ]
        | fields -> fields
      in
      List.exists (field_matches text item) fields

let categories_selected criteria item =
  criteria.categories = []
  || List.exists
       (fun requested ->
         List.exists (equal_casefold requested) (get_categories item))
       criteria.categories

let status_selected criteria item =
  criteria.statuses = []
  ||
  match get_status item with
  | None -> false
  | Some status -> List.mem status criteria.statuses

let completion_selected criteria item =
  match criteria.completed with
  | None -> true
  | Some expected -> (
      match item with
      | Occurrence _ -> false
      | Stored component -> (
          match Component.to_todo component with
          | Some todo -> Bool.equal expected (Todo.is_completed todo)
          | None -> false))

let alarm_selected criteria item =
  match criteria.has_alarm with
  | None -> true
  | Some expected -> Bool.equal expected (get_alarms item <> [])

let overdue_selected ~now ~timezone criteria item =
  match criteria.overdue with
  | None -> Ok true
  | Some expected -> (
      match item with
      | Occurrence _ -> Ok (not expected)
      | Stored component -> (
          match Component.to_todo component with
          | None -> Ok (not expected)
          | Some todo ->
              let* overdue =
                Todo.is_overdue_at ~now ~tz:timezone todo
                |> Result.map_error conversion_error
              in
              Ok (Bool.equal expected overdue)))

let fully_selected ~now ~timezone criteria item =
  if
    text_selected criteria item
    && categories_selected criteria item
    && status_selected criteria item
    && completion_selected criteria item
    && alarm_selected criteria item
  then overdue_selected ~now ~timezone criteria item
  else Ok false

let in_range ~from ~to_ instant =
  Ptime.compare instant to_ < 0
  &&
  match from with
  | None -> true
  | Some lower -> Ptime.compare instant lower >= 0

let component_start ~timezone component =
  Component.get_start_result ~floating_tz:timezone component
  |> Result.map_error conversion_error

let component_end ~timezone component =
  match Component.to_event component with
  | Some event ->
      Event.get_end_result ~floating_tz:timezone event
      |> Result.map_error conversion_error
  | None -> (
      match Component.to_todo component with
      | Some todo -> (
          let* due =
            Todo.get_due_result ~floating_tz:timezone todo
            |> Result.map_error conversion_error
          in
          match (due, Todo.get_duration todo) with
          | Some due, _ -> Ok (Some due)
          | None, None -> Ok None
          | None, Some duration -> (
              let* start =
                Todo.get_start_result ~floating_tz:timezone todo
                |> Result.map_error conversion_error
              in
              match
                Option.bind start (fun start -> Ptime.add_span start duration)
              with
              | Some end_ -> Ok (Some end_)
              | None -> Error (`Msg "VTODO end is out of range")))
      | None -> Ok None)

let get_start_result ~floating_tz = function
  | Stored component -> component_start ~timezone:floating_tz component
  | Occurrence { occurrence; _ } ->
      Event.Occurrence.get_start_result occurrence
      |> Result.map Option.some
      |> Result.map_error conversion_error

let get_end_result ~floating_tz = function
  | Stored component -> component_end ~timezone:floating_tz component
  | Occurrence { occurrence; _ } ->
      Event.Occurrence.get_end_result occurrence
      |> Result.map_error conversion_error

let temporal_candidates ~timezone ~from ~to_ ~include_undated_todos
    ~include_undated_journals ~max_instances components =
  let rec loop accumulated = function
    | [] -> Ok (List.rev accumulated)
    | component :: rest -> (
        match Component.to_event component with
        | Some event ->
            if Event.has_recurrence_set event then
              let* occurrences =
                Event.Recurrence.expand ~floating_tz:timezone ~max_instances
                  ~from ~to_ event
              in
              let occurrences =
                List.map
                  (fun occurrence ->
                    Occurrence { stored_series = component; occurrence })
                  occurrences
              in
              loop (List.rev_append occurrences accumulated) rest
            else
              let* instant = component_start ~timezone component in
              let included =
                Option.fold ~none:false ~some:(in_range ~from ~to_) instant
              in
              loop
                (if included then Stored component :: accumulated
                 else accumulated)
                rest
        | None ->
            let* instant = component_start ~timezone component in
            let is_included =
              match instant with
              | Some instant -> in_range ~from ~to_ instant
              | None ->
                  include_undated_todos
                  && Option.is_some (Component.to_todo component)
                  || include_undated_journals
                     && Option.is_some (Component.to_journal component)
            in
            loop
              (if is_included then Stored component :: accumulated
               else accumulated)
              rest)
  in
  loop [] components

type decorated = {
  item : item;
  start : Ptime.t option;
  end_ : Ptime.t option;
  ordinal : int;
}

let decorate ~timezone ~needs_start ~needs_end items =
  let rec loop ordinal accumulated = function
    | [] -> Ok (List.rev accumulated)
    | item :: rest ->
        let* start =
          if needs_start then get_start_result ~floating_tz:timezone item
          else Ok None
        in
        let* end_ =
          if needs_end then get_end_result ~floating_tz:timezone item
          else Ok None
        in
        loop (ordinal + 1) ({ item; start; end_; ordinal } :: accumulated) rest
  in
  loop 0 [] items

let compare_option compare left right =
  match (left, right) with
  | None, None -> 0
  | Some _, None -> -1
  | None, Some _ -> 1
  | Some left, Some right -> compare left right

let compare_type left right =
  match (component_type left, component_type right) with
  | Component_kind.Event, Component_kind.Event
  | Component_kind.Todo, Component_kind.Todo
  | Component_kind.Journal, Component_kind.Journal ->
      0
  | Component_kind.Event, _ -> -1
  | _, Component_kind.Event -> 1
  | Component_kind.Todo, Component_kind.Journal -> -1
  | Component_kind.Journal, Component_kind.Todo -> 1

let compare_field field left right =
  match field with
  | Start -> compare_option Ptime.compare left.start right.start
  | End -> compare_option Ptime.compare left.end_ right.end_
  | Summary_sort ->
      compare_option String.compare (get_summary left.item)
        (get_summary right.item)
  | Location_sort ->
      compare_option String.compare (get_location left.item)
        (get_location right.item)
  | Calendar ->
      String.compare (get_calendar_key left.item) (get_calendar_key right.item)
  | Type -> compare_type left.item right.item

let compare_decorated sort left right =
  let stable_tiebreak () = Int.compare left.ordinal right.ordinal in
  let rec by_specs = function
    | [] -> stable_tiebreak ()
    | spec :: rest ->
        let value = compare_field spec.field left right in
        let value = if spec.descending then -value else value in
        if value = 0 then by_specs rest else value
  in
  by_specs sort

let take count values =
  let rec loop remaining accumulated = function
    | _ when remaining = 0 -> List.rev accumulated
    | [] -> List.rev accumulated
    | value :: rest -> loop (remaining - 1) (value :: accumulated) rest
  in
  loop count [] values

type stored_todo = { component : Component.t; todo : Todo.t }

let todos_by_calendar components =
  let rec append key stored = function
    | [] -> [ (key, [ stored ]) ]
    | (candidate, group) :: rest when String.equal candidate key ->
        (candidate, group @ [ stored ]) :: rest
    | group :: rest -> group :: append key stored rest
  in
  components
  |> List.filter_map (fun component ->
      Option.map (fun todo -> { component; todo }) (Component.to_todo component))
  |> List.fold_left
       (fun groups stored ->
         append (Component.get_calendar_key stored.component) stored groups)
       []
  |> List.map snd

let validate_todo_groups components =
  let rec loop = function
    | [] -> Ok ()
    | group :: rest ->
        let* () =
          Todo.validate_parent_graph
            (List.map (fun stored -> stored.todo) group)
        in
        loop rest
  in
  loop (todos_by_calendar components)

let expand_todo_groups ~all_todos ~filtered_todos =
  let selected_ids = Hashtbl.create (List.length filtered_todos) in
  List.iter
    (fun component ->
      Hashtbl.replace selected_ids
        (Component.get_calendar_key component, Component.get_id component)
        ())
    filtered_todos;
  let rec loop accumulated = function
    | [] -> Ok (List.rev accumulated)
    | group :: rest ->
        let selected =
          List.filter
            (fun stored ->
              Hashtbl.mem selected_ids
                ( Component.get_calendar_key stored.component,
                  Todo.get_id stored.todo ))
            group
        in
        let* expanded =
          Todo.expand_with_ancestors
            ~all_todos:(List.map (fun stored -> stored.todo) group)
            ~filtered_todos:(List.map (fun stored -> stored.todo) selected)
        in
        let expanded =
          List.map
            (fun todo ->
              List.find
                (fun stored ->
                  String.equal (Todo.get_id stored.todo) (Todo.get_id todo))
                group
              |> fun stored -> stored.component)
            expanded
        in
        loop (List.rev_append expanded accumulated) rest
  in
  loop [] (todos_by_calendar all_todos)

let selected_todos ~criteria components =
  let todo_type_selected =
    criteria.component_types = []
    || List.mem Component_kind.Todo criteria.component_types
  in
  if todo_type_selected then
    components
    |> List.filter (calendar_selected criteria)
    |> List.filter (fun component ->
        Option.is_some (Component.to_todo component))
  else []

let select_sort_and_limit ~timezone ~now ~include_todo_ancestors ~sort ~limit
    ~criteria ~components ~all_todos candidates =
  let rec filter accumulated = function
    | [] -> Ok (List.rev accumulated)
    | item :: rest ->
        let* selected = fully_selected ~now ~timezone criteria item in
        filter (if selected then item :: accumulated else accumulated) rest
  in
  let* selected = filter [] candidates in
  let* selected =
    if include_todo_ancestors then (
      let filtered_todos =
        List.filter_map
          (function
            | Stored component when Option.is_some (Component.to_todo component)
              ->
                Some component
            | Stored _ | Occurrence _ -> None)
          selected
      in
      let* expanded = expand_todo_groups ~all_todos ~filtered_todos in
      let selected_todo_ids = Hashtbl.create (List.length filtered_todos) in
      List.iter
        (fun component ->
          Hashtbl.replace selected_todo_ids
            (Component.get_calendar_key component, Component.get_id component)
            ())
        filtered_todos;
      let added_ancestors =
        expanded
        |> List.filter (fun component ->
            not
              (Hashtbl.mem selected_todo_ids
                 ( Component.get_calendar_key component,
                   Component.get_id component )))
        |> List.map (fun component -> Stored component)
      in
      let source_key component =
        ( Component.get_calendar_key component,
          snd (Component.get_file component),
          Component.component_type component,
          Component.get_id component )
      in
      let source_ranks = Hashtbl.create (List.length components) in
      List.iteri
        (fun index component ->
          Hashtbl.replace source_ranks (source_key component) index)
        components;
      Ok
        (List.stable_sort
           (fun left right ->
             Int.compare
               (Hashtbl.find source_ranks
                  ( get_calendar_key left,
                    snd (get_file left),
                    component_type left,
                    get_id left ))
               (Hashtbl.find source_ranks
                  ( get_calendar_key right,
                    snd (get_file right),
                    component_type right,
                    get_id right )))
           (selected @ added_ancestors)))
    else Ok selected
  in
  let needs field = List.exists (fun spec -> spec.field = field) sort in
  let* decorated =
    decorate ~timezone ~needs_start:(needs Start) ~needs_end:(needs End)
      selected
  in
  let decorated = List.stable_sort (compare_decorated sort) decorated in
  let decorated =
    match limit with None -> decorated | Some count -> take count decorated
  in
  Ok (List.map (fun value -> value.item) decorated)

let validate_limit limit =
  if Option.fold ~none:false ~some:(fun value -> value < 0) limit then
    Error (`Msg "query limit must not be negative")
  else Ok ()

let run ~timezone ~now ~from ~to_ ?(include_undated_todos = false)
    ?(include_undated_journals = false) ?(include_todo_ancestors = false)
    ?(max_instances = 100_000)
    ?(sort = [ { field = Start; descending = false } ]) ?limit ~criteria
    components =
  let* () = validate_criteria criteria in
  if
    Option.fold ~none:false
      ~some:(fun lower -> Ptime.compare lower to_ >= 0)
      from
  then Error (`Msg "query range must have from < to")
  else if max_instances <= 0 then
    Error (`Msg "recurrence expansion limit must be positive")
  else
    let* () = validate_limit limit in
    let all_todos = selected_todos ~criteria components in
    let* () = validate_todo_groups all_todos in
    let invariant = List.filter (invariant_selected criteria) components in
    let* temporal =
      temporal_candidates ~timezone ~from ~to_ ~include_undated_todos
        ~include_undated_journals ~max_instances invariant
    in
    select_sort_and_limit ~timezone ~now ~include_todo_ancestors ~sort ~limit
      ~criteria ~components ~all_todos temporal

let run_unbounded ~timezone ~now ?(include_todo_ancestors = false)
    ?(sort = [ { field = Start; descending = false } ]) ?limit ~criteria
    components =
  let* () = validate_criteria criteria in
  let* () = validate_limit limit in
  let all_todos = selected_todos ~criteria components in
  let* () = validate_todo_groups all_todos in
  let masters = List.filter (invariant_selected criteria) components in
  select_sort_and_limit ~timezone ~now ~include_todo_ancestors ~sort ~limit
    ~criteria ~components ~all_todos
    (List.map (fun component -> Stored component) masters)
