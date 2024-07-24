open De_bruijn_def

let rec pp_de_bruijn fmt t =
  match t with
  | Var x -> Format.pp_print_int fmt x
  | App (t1, t2) -> Format.fprintf fmt "(%a) (%a)" pp_de_bruijn t1 pp_de_bruijn t2
  | Abs t1 -> Format.fprintf fmt "λ. %a" pp_de_bruijn t1

let rec de_bruijn_to_string t =
  match t with
  | Var x -> string_of_int x
  | App (t1, t2) ->
    Format.sprintf "(%s) (%s)" (de_bruijn_to_string t1) (de_bruijn_to_string t2)
  | Abs t1 -> Format.sprintf "λ. %s" (de_bruijn_to_string t1)
