open Lambda_ext_def

let rec pp_extended (fmt : Format.formatter) (t : extended_terms) : unit =
  match t with
  | Var x -> Format.fprintf fmt "%s" x
  | Abs (x, t) -> Format.fprintf fmt "λ %s. %a" x pp_extended t
  | App (t1, t2) -> begin
    match (t1, t2) with
    | Abs _, Abs _ | App _, App _ ->
      Format.fprintf fmt "(%a) (%a)" pp_extended t1 pp_extended t2
    | _, Var _ | Var _, _ | _, Ext _ | Ext _, _ ->
      Format.fprintf fmt "%a %a" pp_extended t1 pp_extended t2
    | _ -> Format.fprintf fmt "%a (%a)" pp_extended t1 pp_extended t2
  end
  | Ext l ->
    Format.fprintf fmt "[";
    Format.pp_print_list
      ?pp_sep:(Some (fun fmt () -> Format.fprintf fmt ", "))
      pp_value fmt l;
    Format.fprintf fmt "]"

and pp_value (fmt : Format.formatter) (v : value) : unit =
  match v with
  | Cst x -> Format.fprintf fmt "%s" x
  | Lam (x, t) -> Format.fprintf fmt "λ %s. %a" x pp_extended t
  | Lst l ->
    Format.fprintf fmt "[";
    Format.pp_print_list
      ?pp_sep:(Some (fun fmt () -> Format.fprintf fmt ", "))
      pp_value fmt l;
    Format.fprintf fmt "]"

let pp_lambda_ext (t : extended_terms) : unit =
  pp_extended Format.std_formatter t

let pp_lambda_val (v : value) : unit = pp_value Format.std_formatter v
