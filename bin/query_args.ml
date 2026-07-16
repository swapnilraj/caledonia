open Cmdliner
open Caledonia_lib

let from_arg =
  let doc =
    "Start date in YYYY-MM-DD format, partial date format (YYYY-MM or YYYY), \
     or a relative expression (today, tomorrow, this-week, next-week, \
     this-month, next-month, +Nd, -Nd, +Nw, +Nm). See DATE FORMATS for more."
  in
  let i = Arg.info [ "from"; "f" ] ~docv:"DATE" ~doc in
  Arg.(value @@ opt (some string) None i)

let to_arg =
  let doc =
    "End date in YYYY-MM-DD format, partial date format (YYYY-MM or YYYY), or \
     a relative expression (today, tomorrow, this-week, next-week, this-month, \
     next-month, +Nd, -Nd, +Nw, +Nm). See DATE FORMATS for more."
  in
  let i = Arg.info [ "to"; "t" ] ~docv:"DATE" ~doc in
  Arg.(value @@ opt (some string) None i)

let calendar_arg =
  let doc = "Filter by calendar" in
  Arg.(
    value & opt_all string [] & info [ "calendar"; "c" ] ~docv:"CALENDAR" ~doc)

let format_enum =
  [
    ("text", `Text);
    ("entries", `Entries);
    ("json", `Json);
    ("csv", `Csv);
    ("ics", `Ics);
    ("sexp", `Sexp);
  ]

let format_arg =
  let doc =
    "Output format (text, entries, json, csv, ics, sexp). Human text dates are \
     localised to the TIMEZONE option. Versioned machine formats preserve \
     authored DATE/UTC/floating/TZID values."
  in
  Arg.(
    value
    & opt (enum format_enum) `Text
    & info [ "format"; "o" ] ~docv:"FORMAT" ~doc)

let count_arg =
  let doc = "Maximum number of components to display" in
  Arg.(value & opt (some int) None & info [ "count"; "n" ] ~docv:"COUNT" ~doc)

let today_arg =
  let doc = "Show components for today only" in
  Arg.(value & flag & info [ "today"; "d" ] ~doc)

let tomorrow_arg =
  let doc = "Show components for tomorrow only" in
  Arg.(value & flag & info [ "tomorrow" ] ~doc)

let week_arg =
  let doc = "Show components for the current week" in
  Arg.(value & flag & info [ "week"; "w" ] ~doc)

let month_arg =
  let doc = "Show components for the current month" in
  Arg.(value & flag & info [ "month"; "m" ] ~doc)

let timezone_arg =
  let doc =
    "Timezone to use for date calculations (e.g., 'America/New_York', 'UTC', \
     'Europe/London') defaulting to the system timezone"
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "timezone"; "z" ] ~docv:"TIMEZONE" ~doc)

let color_arg =
  let doc = "Enable colorized output even when stdout is not a terminal" in
  Arg.(
    value
    & vflag `Auto
        [
          (`Always, info [ "color" ] ~doc);
          (`Never, info [ "no-color" ] ~doc:"Disable colorized output");
        ])

let exclusive_upper ~tz timestamp =
  Date.next_midnight_result ~tz timestamp
  |> Result.map_error (fun error ->
      `Msg (Date.string_of_conversion_error error))

let sort_field_enum =
  [
    ("start", Component_query.Start);
    ("end", Component_query.End);
    ("summary", Component_query.Summary_sort);
    ("location", Component_query.Location_sort);
    ("calendar", Component_query.Calendar);
    ("type", Component_query.Type);
  ]

let parse_sort_spec str =
  let ( let* ) = Result.bind in
  let parts = String.split_on_char ':' str in
  match parts with
  | [] -> Error (`Msg "Empty sort specification")
  | field_str :: order_opt -> (
      let* descending =
        match order_opt with
        | [ "desc" ] | [ "descending" ] -> Ok true
        | [ "asc" ] | [ "ascending" ] -> Ok false
        | [] -> Ok false (* Default to ascending *)
        | _ -> Error (`Msg ("Invalid sort order in: " ^ str))
      in
      match List.assoc_opt field_str sort_field_enum with
      | Some field -> Ok Component_query.{ field; descending }
      | None ->
          Error
            (`Msg
               (Printf.sprintf "Invalid sort field '%s'. Valid options are: %s"
                  field_str
                  (String.concat ", " (List.map fst sort_field_enum)))))

let sort_converter =
  let parse s = parse_sort_spec s in
  let print ppf (spec : Component_query.sort_spec) =
    let field_str =
      List.find_map
        (fun (name, field) -> if field = spec.field then Some name else None)
        sort_field_enum
    in
    let order_str = if spec.descending then ":desc" else "" in
    Fmt.pf ppf "%s%s" (Option.value field_str ~default:"unknown") order_str
  in
  Arg.conv (parse, print)

let default_sort = Component_query.{ field = Start; descending = false }

let sort_arg =
  let doc =
    "Sorting specifications in the format 'field[:order]' where field is one \
     of 'start', 'end', 'summary', 'location', 'calendar', 'type' and order is \
     one of 'asc'/'ascending' or 'desc'/'descending' (default: asc). Multiple \
     sort specs can be provided for multi-level sorting. When no sort is \
     specified, defaults to sorting by start time ascending."
  in
  Arg.(
    value
    & opt_all sort_converter [ default_sort ]
    & info [ "sort"; "S" ] ~docv:"SORT" ~doc)

let create_component_query_sort sort_specs =
  let specs = if sort_specs = [] then [ default_sort ] else sort_specs in
  specs

let parse_timezone_result ~timezone =
  match timezone with
  | Some tzid -> Component_query.timezone_of_name tzid
  | None -> Ok (Date.local_timezone ())

let validate_date_shortcuts ~today ~tomorrow ~week ~month =
  let selected =
    [
      (today, "--today");
      (tomorrow, "--tomorrow");
      (week, "--week");
      (month, "--month");
    ]
    |> List.filter_map (fun (enabled, name) ->
        if enabled then Some name else None)
  in
  match selected with
  | [] | [ _ ] -> Ok ()
  | _ ->
      Error
        (`Msg
           ("Date shortcuts are mutually exclusive; choose one of --today, \
             --tomorrow, --week, or --month (received "
           ^ String.concat ", " selected
           ^ ")"))

type temporal_scope =
  | Unbounded
  | Bounded of { from : Ptime.t option; to_ : Ptime.t }

let conversion_error error = `Msg (Date.string_of_conversion_error error)

let resolve_temporal_scope ~tz ~now ~from_str ~to_str ~today ~tomorrow ~week
    ~month ~default =
  let ( let* ) = Result.bind in
  let* () = validate_date_shortcuts ~today ~tomorrow ~week ~month in
  let* shortcut =
    Date.convert_relative_date_formats ~tz ~now ~today ~tomorrow ~week ~month ()
    |> Result.map_error conversion_error
  in
  match shortcut with
  | Some (from, to_) ->
      let* () =
        match (from_str, to_str) with
        | None, None -> Ok ()
        | _ ->
            Error
              (`Msg
                 "Can't specify --from / --to with --today, --tomorrow, \
                  --week, or --month")
      in
      let* to_ = exclusive_upper ~tz to_ in
      Ok (Bounded { from = Some from; to_ })
  | None -> (
      let* from =
        match from_str with
        | None -> Ok None
        | Some value ->
            Date.parse_date ~tz ~now value `From |> Result.map Option.some
      in
      let* to_ =
        match to_str with
        | None -> Ok None
        | Some value ->
            Date.parse_date ~tz ~now value `To |> Result.map Option.some
      in
      match (from, to_) with
      | Some from, Some to_ ->
          let* to_ = exclusive_upper ~tz to_ in
          Ok (Bounded { from = Some from; to_ })
      | Some from, None ->
          let* to_ =
            Date.add_months_result ~tz from 1
            |> Result.map_error conversion_error
          in
          Ok (Bounded { from = Some from; to_ })
      | None, Some to_ ->
          let* to_ = exclusive_upper ~tz to_ in
          Ok (Bounded { from = None; to_ })
      | None, None -> (
          match default with
          | `Unbounded -> Ok Unbounded
          | `One_month ->
              let* from =
                Date.today_result ~tz ~now |> Result.map_error conversion_error
              in
              let* to_ =
                Date.add_months_result ~tz from 1
                |> Result.map_error conversion_error
              in
              Ok (Bounded { from = Some from; to_ })))

let date_format_manpage_entries =
  [
    `S "DATE FORMATS";
    `P
      "The following are the possible date formats for the --from and --to \
       command line parameters. Note the value is dependent on --from / --to, \
       so --from 2025 --to 2025 includes all matching components in 2025";
    `I ("YYYY-MM-DD", "Specific date (e.g., 2025-3-27, zero-padding optional)");
    `I ("YYYY-MM", "Start/end of specific month (e.g., 2025-3 for March 2025)");
    `I ("YYYY", "Start/end of specific year (e.g., 2025)");
    `I ("today", "Current day");
    `I ("tomorrow", "Next day");
    `I ("yesterday", "Previous day");
    `I ("this-week", "Start/end of current week");
    `I ("next-week", "Start/end of next week");
    `I ("this-month", "Start/end of current month");
    `I ("next-month", "Start/end of next month");
    `I ("+Nd", "N days from today (e.g., +7d for a week from today)");
    `I ("-Nd", "N days before today (e.g., -7d for a week ago)");
    `I ("+Nw", "N weeks from today (e.g., +4w for 4 weeks from today)");
    `I ("+Nm", "N months from today (e.g., +2m for 2 months from today)");
  ]
