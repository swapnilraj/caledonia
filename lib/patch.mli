(** An explicit update to a value that may be absent. *)

type 'a t = Keep | Clear | Set of 'a

val apply : 'a t -> current:'a option -> 'a option
(** [apply patch ~current] returns the value selected by [patch]. *)

val replace_in_list :
  ('element -> bool) ->
  ('value -> 'element) ->
  'value t ->
  'element list ->
  'element list
(** Apply a patch to the unique matching element of a property-style list. *)
