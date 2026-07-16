type t = {
  calendar_key : string;
  display_name : string;
  file : Eio.Fs.dir_ty Eio.Path.t;
  fingerprint : string;
}

let of_decoded_document ~calendar_key ?display_name ~file ~fingerprint () =
  let display_name = Option.value display_name ~default:calendar_key in
  { calendar_key; display_name; file; fingerprint }

let calendar_key t = t.calendar_key
let display_name t = t.display_name
let file t = t.file
let fingerprint t = t.fingerprint

let compare left right =
  compare
    (left.calendar_key, snd left.file, left.fingerprint, left.display_name)
    (right.calendar_key, snd right.file, right.fingerprint, right.display_name)

let equal left right = compare left right = 0
