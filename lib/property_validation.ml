let component_name = function
  | Component_kind.Event -> "VEVENT"
  | Component_kind.Todo -> "VTODO"
  | Component_kind.Journal -> "VJOURNAL"

let validate_singleton ~component ~required name values =
  match values with
  | [] when required ->
      Error
        (`Msg
           (Printf.sprintf "%s requires exactly one %s"
              (component_name component) name))
  | [] | [ _ ] -> Ok ()
  | _ ->
      Error
        (`Msg
           (Printf.sprintf "%s contains duplicate %s" (component_name component)
              name))

let allowed_pinned_aliases = function
  | Component_kind.Event -> [ "RELATED"; "RESOURCE" ]
  | Component_kind.Todo -> [ "PERCENT-COMPLETE"; "RELATED"; "RESOURCE" ]
  | Component_kind.Journal -> [ "RELATED" ]

(* These are the registered component-property names understood by the pinned
   parser. If parsing a value fails, its final [otherprop] branch can otherwise
   turn the line into an [Iana_prop] and make a registered field look like an
   extension. Keep this list explicit and covered by raw-input tests whenever
   the parser pin changes. *)
let reserved_component_property_names =
  [
    "ATTACH";
    "ATTENDEE";
    "CATEGORIES";
    "CLASS";
    "COMMENT";
    "COMPLETED";
    "CONTACT";
    "CREATED";
    "DESCRIPTION";
    "DTEND";
    "DTSTAMP";
    "DTSTART";
    "DUE";
    "DURATION";
    "EXDATE";
    "GEO";
    "LAST-MODIFIED";
    "LOCATION";
    "ORGANIZER";
    "PERCENT";
    "PERCENT-COMPLETE";
    "PRIORITY";
    "RDATE";
    "RECURRENCE-ID";
    "RELATED";
    "RELATED-TO";
    "REQUEST-STATUS";
    "RESOURCE";
    "RESOURCES";
    "RRULE";
    "SEQUENCE";
    "STATUS";
    "SUMMARY";
    "TRANSP";
    "UID";
    "URL";
  ]

(* Compatibility is deliberately limited to the three misspellings produced
   or unparsed by the pinned fork. Calendar_codec writes their RFC names. An
   alias is allowed only on components where the corresponding typed property
   is supported by the domain model. *)
let validate_component_properties ~component properties =
  let aliases = allowed_pinned_aliases component in
  let rec validate = function
    | [] -> Ok ()
    | `Iana_prop (name, _, _) :: rest ->
        let name = String.uppercase_ascii name in
        if
          List.mem name reserved_component_property_names
          && not (List.mem name aliases)
        then
          Error
            (`Msg
               (Printf.sprintf
                  "%s %s could not be parsed as a valid registered property"
                  (component_name component) name))
        else validate rest
    | _ :: rest -> validate rest
  in
  validate properties

let validate_event_properties (properties : Icalendar.event_prop list) =
  validate_component_properties ~component:Component_kind.Event properties

let validate_todo_properties (properties : Icalendar.todo_prop list) =
  validate_component_properties ~component:Component_kind.Todo properties

let validate_journal_properties (properties : Icalendar.journal_prop list) =
  validate_component_properties ~component:Component_kind.Journal properties
