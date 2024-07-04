(* Definition of types *)

open Lambda
open Lambda_ext_cbv

(* Strong Call By Value Evaluator *)

let rec norm (t : extended_term) (e : env) : extended_term =
  let v, e' = value t e in
  readback v e'

and readback (v : value) (e : env) : extended_term =
  match v with
  | Cst x -> Var x
  | Lam (x, b) ->
    let y = gensym () in
    let t = App (Abs (x, b), Ext (y, [])) in

    let t' = norm t e in
    Abs (y, t')
  | Lst (x, l) -> lst_to_app (Var x) l

and lst_to_app (t : extended_term) (args : value_closure list) : extended_term =
  match args with
  | [] -> t
  | (v, e) :: args' ->
    let t' = readback v e in
    lst_to_app (App (t, t')) args'

(* Evaluator for normal form *)

and value (t : extended_term) (e : env) : value_closure =
  match t with
  | Var x ->
    let default = (Lst (x, []), e) in
    Option.value ~default (find_opt x e)
  | Abs (x, t') -> (Lam (x, t'), e)
  | Ext (x, l) -> (Lst (x, l), e)
  | App (t1, t2) -> begin
    match value t1 e with
    | Lam (x, t'), e' ->
      let closure = value t2 e in
      let e' = add x closure e' in
      value t' e'
    | Lst (x, l), e' ->
      let closure = value t2 e in
      let v' = Lst (x, l @ [ closure ]) in
      (v', e')
    | _ -> assert false
  end

(* The function of strong cbv eval *)

let eval (t : lambda_term) : lambda_term =
  gensym_reset ();
  norm (term_to_extended t) empty |> extended_to_term
