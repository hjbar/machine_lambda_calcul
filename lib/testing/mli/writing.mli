open De_bruijn

val weak_terms_htbl : unit -> (de_bruijn_term, unit) Hashtbl.t

val strong_terms_htbl : unit -> (de_bruijn_term, unit) Hashtbl.t

val generate_terms : unit -> unit
