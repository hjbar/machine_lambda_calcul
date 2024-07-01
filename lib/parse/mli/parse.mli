open Lamb
open De_bruijn

val parse : string -> (de_bruijn_term, unit) Hashtbl.t
