type t =
  | Conflict of string
  | Missing_target of string
  | Ambiguous_identity of string
  | Invalid_replacement of string
  | Path_violation of string
  | Io of string
  | Invalid_document of string
  | Unsupported of string

let message = function
  | Conflict message
  | Missing_target message
  | Ambiguous_identity message
  | Invalid_replacement message
  | Path_violation message
  | Io message
  | Invalid_document message
  | Unsupported message ->
      message

let is_conflict = function Conflict _ -> true | _ -> false
