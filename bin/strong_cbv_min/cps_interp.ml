(* Definition of types *)

open Lambda
open Lambda_ext_cbv

type ext_cont = extended_term -> extended_term

type val_clo_cont = value_closure -> extended_term

(* Strong Call By Value Evaluator *)

let rec norm (t : extended_term) (e : env) (k : ext_cont) : extended_term =
  value t e @@ fun (v, e') -> readback v e' k

and readback (v : value) (e : env) (k : ext_cont) : extended_term =
  match v with
  | Cst x -> k @@ Var x
  | Lam (x, b) ->
    let y = gensym () in
    let t = App (Abs (x, b), Ext (y, [])) in
    norm t e @@ fun t' -> k @@ Abs (y, t')
  | Lst (x, l) -> lst_to_app (Var x) l k

and lst_to_app (t : extended_term) (args : value_closure list) (k : ext_cont) : extended_term =
  match args with
  | [] -> k t
  | (v, e) :: args' -> readback v e @@ fun t' -> lst_to_app (App (t, t')) args' k

(* Evaluator for normal form *)

and value (t : extended_term) (e : env) (k : val_clo_cont) : extended_term =
  match t with
  | Var x ->
    let default = (Lst (x, []), e) in
    k @@ Option.value ~default (find_opt x e)
  | Abs (x, t') -> k (Lam (x, t'), e)
  | Ext (x, l) -> k (Lst (x, l), e)
  | App (t1, t2) -> begin
    value t1 e @@ fun (v, e') ->
    match v with
    | Lam (x, t') ->
      value t2 e @@ fun closure ->
      let e' = add x closure e' in
      value t' e' k
    | Lst (x, l) ->
      value t2 e @@ fun closure ->
      let v' = Lst (x, l @ [ closure ]) in
      k (v', e')
    | _ -> assert false
  end

(* The function of strong cbv eval *)

let eval (t : lambda_term) : lambda_term =
  gensym_reset ();
  norm (term_to_extended t) empty Fun.id |> extended_to_term
