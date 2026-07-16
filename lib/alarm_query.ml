type fire = Component_query.item Alarm.fire

let run_result ~floating_tz ~from ~to_ components =
  let ( let* ) = Result.bind in
  let* event_fires =
    components
    |> List.fold_left
         (fun result component ->
           let* acc = result in
           match Component.to_event component with
           | None -> Ok acc
           | Some event ->
               let* fires =
                 Event.compute_alarm_fires_result ~floating_tz ~from ~to_ event
               in
               let fires =
                 List.map
                   (fun (candidate : Event.alarm_owner Alarm.fire) ->
                     let owner =
                       match candidate.owner with
                       | Event.Series _ -> Component_query.Stored component
                       | Event.Occurrence occurrence ->
                           Component_query.Occurrence
                             { stored_series = component; occurrence }
                     in
                     {
                       Alarm.fire_time = candidate.fire_time;
                       owner;
                       alarm = candidate.alarm;
                       alarm_index = candidate.alarm_index;
                     })
                   fires
               in
               Ok (List.rev_append fires acc))
         (Ok [])
  in
  let* todo_fires =
    components
    |> List.fold_left
         (fun result component ->
           let* acc = result in
           match Component.to_todo component with
           | None -> Ok acc
           | Some todo ->
               let* fires =
                 Todo.compute_alarm_fires_result ~floating_tz ~from ~to_ todo
               in
               let fires =
                 List.map
                   (fun (candidate : Todo.t Alarm.fire) ->
                     {
                       Alarm.fire_time = candidate.fire_time;
                       owner = Component_query.Stored component;
                       alarm = candidate.alarm;
                       alarm_index = candidate.alarm_index;
                     })
                   fires
               in
               Ok (List.rev_append fires acc))
         (Ok [])
  in
  Ok
    (List.sort
       (fun (left : fire) (right : fire) ->
         Ptime.compare left.fire_time right.fire_time)
       (event_fires @ todo_fires))
