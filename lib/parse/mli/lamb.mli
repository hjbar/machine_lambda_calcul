type de_bruijn_term =
  | Var of int
  | App of de_bruijn_term * de_bruijn_term
  | Abs of de_bruijn_term

type program = (de_bruijn_term, unit) Hashtbl.t
