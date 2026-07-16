type t = { source : Component_source.t; identity : Component_identity.t }

let create ~source ~identity = { source; identity }
let source t = t.source
let identity t = t.identity
