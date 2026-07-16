let is_emoji_format_character codepoint =
  codepoint = 0x200C || codepoint = 0x200D
  || (codepoint >= 0xE0020 && codepoint <= 0xE007F)

let sanitize_terminal_text ~keep_line_feed text =
  let sanitized = Buffer.create (String.length text) in
  let add _ byte_offset = function
    | `Malformed malformed ->
        String.iter (fun _ -> Buffer.add_char sanitized '?') malformed;
        byte_offset + String.length malformed
    | `Uchar uchar ->
        let codepoint = Uchar.to_int uchar in
        (if keep_line_feed && codepoint = 0x0A then
           Uutf.Buffer.add_utf_8 sanitized uchar
         else
           match Uucp.Gc.general_category uchar with
           | `Cc | `Zl | `Zp -> Buffer.add_char sanitized '?'
           | `Cf when not (is_emoji_format_character codepoint) ->
               Buffer.add_char sanitized '?'
           | _ -> Uutf.Buffer.add_utf_8 sanitized uchar);
        byte_offset + Uchar.utf_8_byte_length uchar
  in
  ignore (Uutf.String.fold_utf_8 add 0 text);
  Buffer.contents sanitized

let sanitize_terminal text = sanitize_terminal_text ~keep_line_feed:true text

let sanitize_terminal_line text =
  sanitize_terminal_text ~keep_line_feed:false text

let format_date ?tz date =
  let tz = Option.value tz ~default:Timedesc.Time_zone.utc in
  let dt = Date.ptime_to_timedesc ~tz date in
  let y = Timedesc.year dt in
  let m = Timedesc.month dt in
  let d = Timedesc.day dt in
  let weekday =
    match Timedesc.weekday dt with
    | `Mon -> "Mon"
    | `Tue -> "Tue"
    | `Wed -> "Wed"
    | `Thu -> "Thu"
    | `Fri -> "Fri"
    | `Sat -> "Sat"
    | `Sun -> "Sun"
  in
  Printf.sprintf "%04d-%02d-%02d %s" y m d weekday

let format_opt label f opt =
  Option.map (fun x -> Printf.sprintf "%s: %s\n" label (f x)) opt
  |> Option.value ~default:""

let grapheme_width grapheme =
  let width, regional_indicators, emoji_presentation =
    Uutf.String.fold_utf_8
      (fun (width, regional_indicators, emoji_presentation) _ decoded ->
        match decoded with
        | `Malformed _ -> (max width 1, regional_indicators, emoji_presentation)
        | `Uchar uchar ->
            let codepoint = Uchar.to_int uchar in
            let hint = max 0 (Uucp.Break.tty_width_hint uchar) in
            ( max width hint,
              (regional_indicators
              + if codepoint >= 0x1F1E6 && codepoint <= 0x1F1FF then 1 else 0),
              emoji_presentation || codepoint = 0xFE0F || codepoint = 0x20E3 ))
      (0, 0, false) grapheme
  in
  if regional_indicators >= 2 || emoji_presentation then max 2 width else width

let display_width s =
  Uuseg_string.fold_utf_8 `Grapheme_cluster
    (fun width grapheme -> width + grapheme_width grapheme)
    0 s

let parse_color color_str =
  let color_str = String.trim color_str in
  if String.length color_str > 0 && color_str.[0] = '#' then
    let hex = String.sub color_str 1 (String.length color_str - 1) in
    try
      let r = int_of_string ("0x" ^ String.sub hex 0 2) in
      let g = int_of_string ("0x" ^ String.sub hex 2 2) in
      let b = int_of_string ("0x" ^ String.sub hex 4 2) in
      Some (r, g, b)
    with _ -> None
  else None

let colorize ?color text =
  match color with
  | None -> text
  | Some color_str -> (
      match parse_color color_str with
      | None -> text
      | Some (r, g, b) ->
          Printf.sprintf "\027[38;2;%d;%d;%dm%s\027[0m" r g b text)

let pad_to_width ?(color : string option) target_width s =
  let current_width = display_width s in
  let padded =
    if current_width >= target_width then s
    else s ^ String.make (target_width - current_width) ' '
  in
  match color with None -> padded | Some c -> colorize ~color:c padded

let max_width f data =
  List.fold_left (fun acc x -> max acc (display_width (f x))) 0 data

let format_alarm_trigger span =
  let seconds = Ptime.Span.to_float_s span in
  let abs_seconds = Float.abs seconds in
  let suffix =
    if seconds < 0.0 then " before" else if seconds > 0.0 then " after" else ""
  in
  let days = int_of_float (abs_seconds /. 86400.0) in
  let remaining = abs_seconds -. (float_of_int days *. 86400.0) in
  let hours = int_of_float (remaining /. 3600.0) in
  let remaining = remaining -. (float_of_int hours *. 3600.0) in
  let minutes = int_of_float (remaining /. 60.0) in
  let seconds = int_of_float (remaining -. (float_of_int minutes *. 60.0)) in
  let parts = [] in
  let parts =
    if days > 0 then
      Printf.sprintf "%d day%s" days (if days > 1 then "s" else "") :: parts
    else parts
  in
  let parts =
    if hours > 0 then
      Printf.sprintf "%d hour%s" hours (if hours > 1 then "s" else "") :: parts
    else parts
  in
  let parts =
    if minutes > 0 then
      Printf.sprintf "%d minute%s" minutes (if minutes > 1 then "s" else "")
      :: parts
    else parts
  in
  let parts =
    if seconds > 0 then
      Printf.sprintf "%d second%s" seconds (if seconds > 1 then "s" else "")
      :: parts
    else parts
  in
  let parts = List.rev parts in
  match parts with [] -> "at start" | _ -> String.concat " " parts ^ suffix

let format_alarm_short span =
  let seconds = Ptime.Span.to_float_s span in
  let abs_seconds = Float.abs seconds in
  let days = int_of_float (abs_seconds /. 86400.0) in
  let remaining = abs_seconds -. (float_of_int days *. 86400.0) in
  let hours = int_of_float (remaining /. 3600.0) in
  let remaining = remaining -. (float_of_int hours *. 3600.0) in
  let minutes = int_of_float (remaining /. 60.0) in
  let seconds = int_of_float (remaining -. (float_of_int minutes *. 60.0)) in
  if days > 0 && hours = 0 && minutes = 0 && seconds = 0 then
    Printf.sprintf "%dd" days
  else if days = 0 && hours > 0 && minutes = 0 && seconds = 0 then
    Printf.sprintf "%dh" hours
  else if days = 0 && hours = 0 && minutes > 0 && seconds = 0 then
    Printf.sprintf "%dm" minutes
  else if days = 0 && hours = 0 && minutes = 0 && seconds > 0 then
    Printf.sprintf "%ds" seconds
  else if days = 0 && hours = 0 && minutes = 0 && seconds = 0 then "0m"
  else
    let parts = [] in
    let parts =
      if seconds > 0 then Printf.sprintf "%ds" seconds :: parts else parts
    in
    let parts =
      if minutes > 0 then Printf.sprintf "%dm" minutes :: parts else parts
    in
    let parts =
      if hours > 0 then Printf.sprintf "%dh" hours :: parts else parts
    in
    let parts =
      if days > 0 then Printf.sprintf "%dd" days :: parts else parts
    in
    String.concat "" parts

let format_alarm_trigger_text alarm =
  match Alarm.trigger alarm with
  | _, `Duration span -> format_alarm_trigger span
  | _, `Datetime _ -> "at fixed time"

let format_alarms alarms =
  let trigger_strs =
    List.filter_map
      (fun alarm ->
        match alarm with
        | `None _ -> None
        | `Audio _ | `Display _ | `Email _ ->
            Some (format_alarm_trigger_text alarm))
      alarms
  in
  match trigger_strs with [] -> "" | strs -> String.concat ", " strs

let format_alarms_short alarms =
  let trigger_strs =
    List.filter_map
      (fun alarm ->
        match alarm with
        | `None _ -> None
        | `Audio _ | `Display _ | `Email _ -> (
            match Alarm.trigger alarm with
            | _, `Duration span -> Some (format_alarm_short span)
            | _, `Datetime _ -> Some "abs"))
      alarms
  in
  match trigger_strs with [] -> "" | strs -> String.concat "," strs
