type t = {
  calendar_key : string;
  source_file : string;
  uid : string;
  recurrence_key : string;
  alarm_index : int;
  alarm_key : string;
  fire_time : Ptime.t;
}

let compare = compare

let create ~target ~recurrence_key ~alarm_index ~alarm_key ~fire_time =
  let source = Component_target.source target in
  let identity = Component_target.identity target in
  {
    calendar_key = Component_source.calendar_key source;
    source_file = snd (Component_source.file source);
    uid = identity.uid;
    recurrence_key;
    alarm_index;
    alarm_key;
    fire_time;
  }
