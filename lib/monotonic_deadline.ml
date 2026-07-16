type t = float

let after ~now seconds =
  if seconds < 0. then invalid_arg "deadline duration must be non-negative";
  now () +. seconds

let reached ~now deadline = now () >= deadline
