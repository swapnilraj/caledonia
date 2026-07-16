module Timezone_id_set = Set.Make (struct
  type t = bool * string

  let compare = compare
end)

let ( let* ) = Result.bind

let add_params_timezone references params =
  match Icalendar.Params.find Icalendar.Tzid params with
  | Some tzid -> Timezone_id_set.add tzid references
  | None -> references

let add_timestamp_timezone references = function
  | `With_tzid (_, tzid) -> Timezone_id_set.add tzid references
  | `Utc _ | `Local _ -> references

let add_calendar_time_timezone references = function
  | `Datetime timestamp -> add_timestamp_timezone references timestamp
  | `Date _ -> references

let add_dates_timezones references = function
  | `Dates _ -> references
  | `Datetimes timestamps ->
      List.fold_left add_timestamp_timezone references timestamps

let add_rdates_timezones references = function
  | (`Dates _ | `Datetimes _) as values -> add_dates_timezones references values
  | `Periods periods ->
      List.fold_left
        (fun references (timestamp, _, _) ->
          add_timestamp_timezone references timestamp)
        references periods

let add_calendar_time_property references params value =
  add_params_timezone references params |> fun references ->
  add_calendar_time_timezone references value

let add_exdate_property references params values =
  add_params_timezone references params |> fun references ->
  add_dates_timezones references values

let add_rdate_property references params values =
  add_params_timezone references params |> fun references ->
  add_rdates_timezones references values

let add_event_property_timezones references (property : Icalendar.event_prop) =
  match property with
  | `Dtstart (params, value) | `Dtend (params, value) | `Recur_id (params, value)
    ->
      add_calendar_time_property references params value
  | `Exdate (params, values) -> add_exdate_property references params values
  | `Rdate (params, values) -> add_rdate_property references params values
  | _ -> references

let add_todo_property_timezones references (property : Icalendar.todo_prop) =
  match property with
  | `Dtstart (params, value) | `Due (params, value) | `Recur_id (params, value)
    ->
      add_calendar_time_property references params value
  | `Exdate (params, values) -> add_exdate_property references params values
  | `Rdate (params, values) -> add_rdate_property references params values
  | _ -> references

let add_journal_property_timezones references
    (property : Icalendar.journal_prop) =
  match property with
  | `Dtstart (params, value) | `Recur_id (params, value) ->
      add_calendar_time_property references params value
  | `Exdate (params, values) -> add_exdate_property references params values
  | `Rdate (params, values) -> add_rdate_property references params values
  | _ -> references

let referenced_timezones components =
  List.fold_left
    (fun references component ->
      match component with
      | `Event (event : Icalendar.event) ->
          let params, start = event.dtstart in
          let references =
            add_params_timezone references params |> fun references ->
            add_calendar_time_timezone references start
          in
          let references =
            match event.dtend_or_duration with
            | Some (`Dtend (params, end_)) ->
                add_params_timezone references params |> fun references ->
                add_calendar_time_timezone references end_
            | Some (`Duration _) | None -> references
          in
          List.fold_left add_event_property_timezones references event.props
      | `Todo (properties, _) ->
          List.fold_left add_todo_property_timezones references properties
      | `Journal properties ->
          List.fold_left add_journal_property_timezones references properties
      | `Freebusy _ | `Timezone _ -> references)
    Timezone_id_set.empty components

let timezone_ids timezone =
  List.filter_map
    (function `Timezone_id (_, tzid) -> Some tzid | _ -> None)
    timezone

let timezone_id timezone =
  match timezone_ids timezone with
  | [ tzid ] -> Ok tzid
  | [] -> Error (`Msg "VTIMEZONE is missing its required TZID property")
  | _ -> Error (`Msg "VTIMEZONE contains duplicate TZID properties")

let string_of_timezone_id (absolute, value) =
  (if absolute then "/" else "") ^ value

let item_source = function
  | Component_query.Stored component -> Ok (Component.get_source component)
  | Component_query.Occurrence { stored_series; occurrence } -> (
      match Component.to_event stored_series with
      | None -> Error (`Msg "Occurrence export context is not a stored VEVENT")
      | Some series ->
          let selected_uid =
            occurrence |> Event.Occurrence.reference
            |> Event.Occurrence.Reference.uid
          in
          if String.equal selected_uid (Event.get_id series) then
            Ok (Component.get_source stored_series)
          else
            Error
              (`Msg
                 "Occurrence export context belongs to a different VEVENT \
                  series"))

let item_known_entries = function
  | Component_query.Stored component ->
      Calendar_document.known_entries_of_body (Component.body component)
  | Component_query.Occurrence { occurrence; _ } -> (
      match
        Calendar_codec.make_known ~rrule_date_untils:[]
          (`Event (Event.Occurrence.effective_ical_event occurrence))
      with
      | Ok known -> Ok [ known ]
      | Error message -> Error (`Msg message))

let find_document ~documents source =
  match
    List.filter
      (fun document ->
        Component_source.equal source (Calendar_document.source document))
      documents
  with
  | document :: _ -> Ok document
  | [] ->
      Error
        (`Msg
           "Selected component has no matching immutable document export \
            context")

let selected_documents ~documents items =
  let rec collect selected_sources selected_documents = function
    | [] -> Ok (List.rev selected_documents)
    | item :: rest ->
        let* source = item_source item in
        if List.exists (Component_source.equal source) selected_sources then
          collect selected_sources selected_documents rest
        else
          let* document = find_document ~documents source in
          collect
            (source :: selected_sources)
            (document :: selected_documents)
            rest
  in
  collect [] [] items

let timezone_entries ~documents exported_components =
  let referenced = referenced_timezones exported_components in
  let timezones =
    documents
    |> List.concat_map Calendar_document.timezone_entries
    |> List.filter_map (fun known ->
        match Calendar_codec.component known with
        | `Timezone timezone -> Some (known, timezone)
        | `Event _ | `Todo _ | `Journal _ | `Freebusy _ -> None)
    |> List.filter (fun (_, timezone) ->
        List.exists
          (fun tzid -> Timezone_id_set.mem tzid referenced)
          (timezone_ids timezone))
  in
  let* unique =
    List.fold_left
      (fun result (known, timezone) ->
        let* unique = result in
        let* tzid = timezone_id timezone in
        match List.assoc_opt tzid unique with
        | None -> Ok ((tzid, (known, timezone)) :: unique)
        | Some (_, existing) when existing = timezone -> Ok unique
        | Some _ ->
            Error
              (`Msg
                 ("Conflicting VTIMEZONE definitions for TZID "
                ^ string_of_timezone_id tzid)))
      (Ok []) timezones
  in
  Ok (List.rev_map (fun (_, (known, _)) -> known) unique)

let to_ics ~documents = function
  | [] -> Ok ""
  | items ->
      let* documents = selected_documents ~documents items in
      let* exported_known =
        let rec collect accumulated = function
          | [] -> Ok (List.rev accumulated |> List.flatten)
          | item :: rest ->
              let* known = item_known_entries item in
              collect (known :: accumulated) rest
        in
        collect [] items
      in
      let exported_components =
        List.map Calendar_codec.component exported_known
      in
      let* timezone_known = timezone_entries ~documents exported_components in
      let properties =
        [
          `Prodid (Icalendar.Params.empty, "-//Freumh//Caledonia//EN");
          `Version (Icalendar.Params.empty, "2.0");
        ]
      in
      let export_document =
        Calendar_codec.create_known ~properties (timezone_known @ exported_known)
      in
      Ok (Calendar_codec.serialize ~cr:true export_document)

let stored_to_ics ~documents components =
  to_ics ~documents
    (List.map (fun component -> Component_query.Stored component) components)
