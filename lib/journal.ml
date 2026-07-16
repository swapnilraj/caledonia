open Icalendar

type t = { props : journal_prop list }

let get_id t =
  List.find_map (function `Uid (_, id) -> Some id | _ -> None) t.props
  |> Option.value ~default:""

let ( let* ) = Result.bind

let valid_status = function
  | `Draft | `Final | `Cancelled -> true
  | `Needs_action | `Completed | `In_process | `Tentative | `Confirmed -> false

let validate_status = function
  | Some status when not (valid_status status) ->
      Error (`Msg "VJOURNAL status must be DRAFT, FINAL, or CANCELLED")
  | _ -> Ok ()

let validate_singleton name values =
  Property_validation.validate_singleton ~component:Component_kind.Journal
    ~required:false name values

let validate_required_singleton name values =
  Property_validation.validate_singleton ~component:Component_kind.Journal
    ~required:true name values

let validate_journal_props props =
  let* () = Property_validation.validate_journal_properties props in
  let uids =
    List.filter_map (function `Uid (_, value) -> Some value | _ -> None) props
  in
  let timestamps =
    List.filter_map
      (function `Dtstamp (_, value) -> Some value | _ -> None)
      props
  in
  let statuses =
    List.filter_map
      (function `Status (_, value) -> Some value | _ -> None)
      props
  in
  let starts =
    List.filter_map (function `Dtstart value -> Some value | _ -> None) props
  in
  let priorities =
    List.filter_map
      (function `Priority (_, value) -> Some value | _ -> None)
      props
  in
  let singleton_properties =
    List.filter_map
      (function
        | `Class _ -> Some "CLASS"
        | `Created _ -> Some "CREATED"
        | `Lastmod _ -> Some "LAST-MODIFIED"
        | `Organizer _ -> Some "ORGANIZER"
        | `Recur_id _ -> Some "RECURRENCE-ID"
        | `Rrule _ -> Some "RRULE"
        | `Seq _ -> Some "SEQUENCE"
        | `Summary _ -> Some "SUMMARY"
        | `Url _ -> Some "URL"
        | _ -> None)
      props
  in
  let rec validate_property_singletons seen = function
    | [] -> Ok ()
    | name :: _ when List.mem name seen ->
        Error (`Msg (Printf.sprintf "VJOURNAL contains duplicate %s" name))
    | name :: rest -> validate_property_singletons (name :: seen) rest
  in
  let invalid_properties =
    List.filter_map
      (function
        | `Duration _ -> Some "DURATION"
        | `Geo _ -> Some "GEO"
        | `Location _ -> Some "LOCATION"
        | `Priority _ -> Some "PRIORITY"
        | `Resource _ -> Some "RESOURCES"
        | _ -> None)
      props
  in
  let recurrence_properties =
    List.filter
      (function
        | `Rrule _ | `Recur_id _ | `Rdate _ | `Exdate _ -> true | _ -> false)
      props
  in
  let* () = validate_required_singleton "UID" uids in
  let* () = validate_required_singleton "DTSTAMP" timestamps in
  let* () =
    match uids with
    | [ uid ] when String.trim uid <> "" -> Ok ()
    | [ _ ] -> Error (`Msg "VJOURNAL UID must not be empty")
    | _ -> assert false
  in
  let* () = validate_singleton "STATUS" statuses in
  let* () = validate_singleton "DTSTART" starts in
  let* () =
    match starts with
    | [] -> Ok ()
    | [ (params, value) ] ->
        Date.validate_date_or_datetime_params ~property:"VJOURNAL DTSTART"
          params value
    | _ -> assert false
  in
  let* () = validate_singleton "PRIORITY" priorities in
  let* () = validate_property_singletons [] singleton_properties in
  let* () =
    if recurrence_properties = [] then Ok ()
    else
      Error
        (`Msg
           "Recurring VJOURNAL components are not supported; refusing to \
            return an incomplete schedule")
  in
  let* status =
    match statuses with
    | [] -> Ok None
    | [ value ] -> Ok (Some value)
    | _ -> assert false
  in
  let* () = validate_status status in
  match invalid_properties with
  | [] -> Ok ()
  | property :: _ ->
      Error (`Msg (Printf.sprintf "%s is not valid on VJOURNAL" property))

let with_updated_props _t props = { props }

let create ~now ?summary ?start ?description ?categories ?status () =
  let uuid = Fresh_id.generate () in
  let uid = (Params.empty, uuid) in
  let ( let* ) = Result.bind in
  let* () = validate_status status in
  let props = [ `Dtstamp (Params.empty, now); `Uid uid ] in
  let props =
    match summary with
    | Some s -> `Summary (Params.empty, s) :: props
    | None -> props
  in
  let props =
    match start with Some s -> `Dtstart s :: props | None -> props
  in
  let props =
    match description with
    | Some d -> `Description (Params.empty, d) :: props
    | None -> props
  in
  let props =
    match categories with
    | Some cats -> `Categories (Params.empty, cats) :: props
    | None -> props
  in
  let props =
    match status with
    | Some s -> `Status (Params.empty, s) :: props
    | None -> props
  in
  let* () = validate_journal_props props in
  Ok { props }

let edit ~now ?(summary = Patch.Keep) ?(start = Patch.Keep)
    ?(description = Patch.Keep) ?(categories = Patch.Keep)
    ?(status = Patch.Keep) t =
  if
    summary = Patch.Keep && start = Patch.Keep && description = Patch.Keep
    && categories = Patch.Keep && status = Patch.Keep
  then Ok t
  else
    let requested_status =
      Patch.apply status
        ~current:
          (List.find_map
             (function `Status (_, value) -> Some value | _ -> None)
             t.props)
    in
    let ( let* ) = Result.bind in
    let* () = validate_status requested_status in
    let props =
      List.filter (function `Dtstamp _ -> false | _ -> true) t.props
      |> fun props -> `Dtstamp (Params.empty, now) :: props
    in
    let props =
      props
      |> Patch.replace_in_list
           (function `Summary _ -> true | _ -> false)
           (fun value -> `Summary (Params.empty, value))
           summary
      |> Patch.replace_in_list
           (function `Dtstart _ -> true | _ -> false)
           (fun value -> `Dtstart value)
           start
      |> Patch.replace_in_list
           (function `Description _ -> true | _ -> false)
           (fun value -> `Description (Params.empty, value))
           description
      |> Patch.replace_in_list
           (function `Categories _ -> true | _ -> false)
           (fun value -> `Categories (Params.empty, value))
           categories
      |> Patch.replace_in_list
           (function `Status _ -> true | _ -> false)
           (fun value -> `Status (Params.empty, value))
           status
    in
    let* () = validate_journal_props props in
    Ok (with_updated_props t props)

let of_ical_body props =
  let* () = validate_journal_props props in
  Ok { props }

let to_ical_journal t = t.props

let get_summary t =
  List.find_map (function `Summary (_, s) -> Some s | _ -> None) t.props

let get_start_time t =
  List.find_map
    (function `Dtstart (_, value) -> Some value | _ -> None)
    t.props

let get_start_result ~floating_tz t =
  match get_start_time t with
  | None -> Ok None
  | Some value ->
      Result.map Option.some (Date.ptime_of_ical_result ~floating_tz value)

let get_description t =
  match
    List.filter_map
      (function `Description (_, d) -> Some d | _ -> None)
      t.props
  with
  | [] -> None
  | descriptions -> Some (String.concat "\n" descriptions)

let get_categories t =
  List.filter_map
    (function `Categories (_, cats) -> Some cats | _ -> None)
    t.props
  |> List.flatten

let get_status t =
  List.find_map (function `Status (_, s) -> Some s | _ -> None) t.props
