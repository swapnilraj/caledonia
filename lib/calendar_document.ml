type t = {
  source : Component_source.t;
  content : Calendar_codec.t;
  components : Component.t list;
}

let ( let* ) = Result.bind

let known_components content =
  Calendar_codec.entries content
  |> List.filter_map (function
    | Calendar_codec.Known entry -> Some (Calendar_codec.component entry)
    | Calendar_codec.Opaque _ -> None)

let authored_events content =
  let rec collect accumulated = function
    | [] -> Ok (List.rev accumulated)
    | Calendar_codec.Opaque _ :: rest -> collect accumulated rest
    | Calendar_codec.Known known :: rest -> (
        match Calendar_codec.component known with
        | `Event event ->
            let* date_until =
              match
                (event.Icalendar.rrule, Calendar_codec.rrule_date_untils known)
              with
              | None, [] -> Ok None
              | Some _, [ date_until ] -> Ok date_until
              | None, _ :: _ | Some _, ([] | _ :: _ :: _) ->
                  Error
                    (`Msg
                       "Codec VEVENT RRULE metadata has an invalid traversal \
                        shape")
            in
            collect ((event, date_until) :: accumulated) rest
        | `Todo _ | `Journal _ | `Freebusy _ | `Timezone _ ->
            collect accumulated rest)
  in
  collect [] (Calendar_codec.entries content)

let decode ~source content =
  let* authored_events = authored_events content in
  let* components =
    Component.stored_views_of_decoded_components ~authored_events ~source
      (known_components content)
  in
  Ok { source; content; components }

let parse ~source text =
  match Calendar_codec.parse_document text with
  | Error message -> Error (`Msg message)
  | Ok content -> decode ~source content

let source t = t.source
let components t = t.components

let find t identity =
  match
    List.filter
      (fun component ->
        Component_identity.equal identity (Component.get_identity component))
      t.components
  with
  | [ component ] -> Ok component
  | [] -> Error (`Msg "Document does not contain the requested component")
  | _ -> Error (`Msg "Document contains an ambiguous component identity")

let known_of_component ?rrule_date_untils component =
  match Calendar_codec.make_known ?rrule_date_untils component with
  | Ok known -> Ok known
  | Error message -> Error (`Msg message)

let known_entries_of_body body =
  match Component.event_of_body body with
  | Some series ->
      let master = Event.master series in
      let* master =
        known_of_component
          ~rrule_date_untils:
            (match master.Icalendar.rrule with
            | None -> []
            | Some _ -> [ Event.date_until series ])
          (`Event master)
      in
      let rec overrides accumulated = function
        | [] -> Ok (List.rev accumulated)
        | event :: rest ->
            let* known =
              known_of_component ~rrule_date_untils:[] (`Event event)
            in
            overrides (known :: accumulated) rest
      in
      let* overrides = overrides [] (Event.overrides series) in
      Ok (master :: overrides)
  | None ->
      let rec collect accumulated = function
        | [] -> Ok (List.rev accumulated)
        | component :: rest ->
            let* known = known_of_component component in
            collect (known :: accumulated) rest
      in
      collect [] (Component.ical_components_of_body body)

let known_replacements body =
  let* known_entries = known_entries_of_body body in
  let rec collect accumulated = function
    | [] -> Ok (List.rev accumulated)
    | known :: rest ->
        let component = Calendar_codec.component known in
        let* identity =
          match Component_identity.of_ical_component component with
          | Some identity -> Ok identity
          | None -> Error (`Msg "Replacement body has no writable identity")
        in
        collect ((identity, known) :: accumulated) rest
  in
  collect [] known_entries

let decode_rewrite t content = decode ~source:t.source content

let fold_known_with_identity t ~init ~f =
  Calendar_codec.entries t.content
  |> List.fold_left
       (fun accumulated -> function
         | Calendar_codec.Opaque _ -> accumulated
         | Calendar_codec.Known known ->
             let identity =
               Calendar_codec.component known
               |> Component_identity.of_ical_component
             in
             f accumulated identity known)
       init

let rewrite_known_with_identity t ~f =
  Calendar_codec.rewrite_known t.content ~f:(fun known ->
      let identity =
        Calendar_codec.component known |> Component_identity.of_ical_component
      in
      f identity known)

let replace t ~(target : Component_identity.t) ~replacement =
  let* replacements = known_replacements replacement in
  let* () =
    match replacements with
    | (identity, _) :: _ when Component_identity.equal target identity -> Ok ()
    | _ -> Error (`Msg "An edit cannot change component identity")
  in
  let existing =
    fold_known_with_identity t ~init:[] ~f:(fun existing identity known ->
        match identity with
        | Some identity -> (identity, known) :: existing
        | None -> existing)
    |> List.rev
  in
  let replacement_for identity = List.assoc_opt identity replacements in
  let is_target_series identity =
    Component_kind.equal target.kind Component_kind.Event
    && target.recurrence_id = None
    && Component_kind.equal identity.Component_identity.kind
         Component_kind.Event
    && String.equal target.uid identity.uid
  in
  let retained_series =
    existing
    |> List.filter_map (fun (identity, _) ->
        if
          is_target_series identity && Option.is_some (replacement_for identity)
        then Some identity
        else None)
  in
  let last_retained =
    match List.rev retained_series with [] -> None | value :: _ -> Some value
  in
  let new_series =
    replacements
    |> List.filter (fun (identity, _) ->
        not
          (List.exists
             (fun (candidate, _) -> Component_identity.equal identity candidate)
             existing))
    |> List.map snd
  in
  let matches = ref 0 in
  let rewritten =
    rewrite_known_with_identity t ~f:(fun identity _known ->
        match identity with
        | None -> Calendar_codec.Keep
        | Some identity when is_target_series identity -> (
            if identity.recurrence_id = None then incr matches;
            match replacement_for identity with
            | None -> Calendar_codec.Delete
            | Some replacement ->
                let additions =
                  match last_retained with
                  | Some last when Component_identity.equal last identity ->
                      new_series
                  | Some _ | None -> []
                in
                Calendar_codec.Replace (replacement :: additions))
        | Some identity when Component_identity.equal target identity -> (
            incr matches;
            match replacements with
            | [ (_, replacement) ] -> Calendar_codec.Replace [ replacement ]
            | _ -> Calendar_codec.Keep)
        | Some _ -> Calendar_codec.Keep)
  in
  let* content =
    match rewritten with
    | Ok content -> Ok content
    | Error message -> Error (`Msg message)
  in
  if !matches = 0 then Error (`Msg "The target component no longer exists")
  else if !matches > 1 then Error (`Msg "The target component is ambiguous")
  else decode_rewrite t content

let delete t ~(target : Component_identity.t) =
  let matches = ref 0 in
  let should_delete identity =
    if
      Component_kind.equal target.kind Component_kind.Event
      && target.recurrence_id = None
    then
      Component_kind.equal identity.Component_identity.kind Component_kind.Event
      && String.equal target.uid identity.uid
    else Component_identity.equal target identity
  in
  let content =
    rewrite_known_with_identity t ~f:(fun identity _known ->
        match identity with
        | Some identity when should_delete identity ->
            if
              (not (Component_kind.equal target.kind Component_kind.Event))
              || target.recurrence_id <> None
              || identity.recurrence_id = None
            then incr matches;
            Calendar_codec.Delete
        | Some _ | None -> Calendar_codec.Keep)
  in
  let* content =
    match content with
    | Ok content -> Ok content
    | Error message -> Error (`Msg message)
  in
  if !matches = 0 then Error (`Msg "The target component no longer exists")
  else if !matches > 1 then Error (`Msg "The target component is ambiguous")
  else decode_rewrite t content

let serialize ?cr t = Calendar_codec.serialize ?cr t.content
let has_entries t = Calendar_codec.entries t.content <> []

let timezone_entries t =
  Calendar_codec.entries t.content
  |> List.filter_map (function
    | Calendar_codec.Known known -> (
        match Calendar_codec.component known with
        | `Timezone _ -> Some known
        | `Event _ | `Todo _ | `Journal _ | `Freebusy _ -> None)
    | Calendar_codec.Opaque _ -> None)
