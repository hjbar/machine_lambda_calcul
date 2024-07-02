(* Definition of types *)

open Lambda
open Lambda_ext_cbnd

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
  | Var x -> begin
    match find_opt x e with
    | None ->
      let v = Lst (x, []) in
      k (v, e)
    | Some var -> begin
      match !var with
      | Delayed (t', e') ->
        value t' e' @@ fun closure ->
        var := Computed closure;
        k closure
      | Computed closure -> k closure
    end
  end
  | Abs (x, t') -> k (Lam (x, t'), e)
  | Ext (x, l) -> k (Lst (x, l), e)
  | App (t1, t2) -> begin
    value t1 e @@ fun (v, e') ->
    match v with
    | Lam (x, t') ->
      let e' = add x (ref @@ Delayed (t2, e)) e' in
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
