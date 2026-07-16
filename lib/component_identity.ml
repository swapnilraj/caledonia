type t = {
  kind : Component_kind.t;
  uid : string;
  recurrence_id : Icalendar.date_or_datetime option;
}

let compare = compare
let equal left right = compare left right = 0

let uid_of_props props =
  List.find_map (function `Uid (_, uid) -> Some uid | _ -> None) props

let recurrence_id_of_props props =
  List.find_map
    (function `Recur_id (_, value) -> Some value | _ -> None)
    props

let recurrence_id_of_event (event : Icalendar.event) =
  List.find_map
    (function `Recur_id (_, value) -> Some value | _ -> None)
    event.props

let of_ical_component = function
  | `Event (event : Icalendar.event) ->
      Some
        {
          kind = Component_kind.Event;
          uid = snd event.uid;
          recurrence_id = recurrence_id_of_event event;
        }
  | `Todo (props, _) ->
      Option.map
        (fun uid ->
          {
            kind = Component_kind.Todo;
            uid;
            recurrence_id = recurrence_id_of_props props;
          })
        (uid_of_props props)
  | `Journal props ->
      Option.map
        (fun uid ->
          {
            kind = Component_kind.Journal;
            uid;
            recurrence_id = recurrence_id_of_props props;
          })
        (uid_of_props props)
  | `Freebusy _ | `Timezone _ -> None
