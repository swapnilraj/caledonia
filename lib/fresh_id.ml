let generator = lazy (Uuidm.v4_gen (Random.State.make_self_init ()))
let generate () = Uuidm.to_string (Lazy.force generator ())
