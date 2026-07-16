type t = Event | Todo | Journal [@@deriving sexp]

let equal left right = left = right

let to_string = function
  | Event -> "event"
  | Todo -> "todo"
  | Journal -> "journal"
