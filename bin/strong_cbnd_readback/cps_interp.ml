(* Definition of types *)

open Lambda
open Lambda_ext_cbnd

type ext_cont = extended_term -> extended_term

type val_clo_cont = value_closure -> extended_term

(* Strong Call By Value Evaluator *)

let rec norm (b : extended_term) (e : env) (k : ext_cont) : extended_term =
  value b e @@ fun (v, e') -> readback v e' k

and readback (v : value) (e : env) (k : ext_cont) : extended_term =
  match v with
  | Cst x -> k @@ Var x
  | Lam (x, b) ->
    let y = gensym () in
    let t = App (Abs (x, b), Ext (y, [])) in
    norm t e @@ fun t' -> k @@ Abs (y, t')
  | Lst (x, l) -> lst_to_app (Var x) l k

and lst_to_app (b : extended_term) (args : value_closure list) (k : ext_cont) : extended_term =
  match args with
  | [] -> k b
  | (v, e) :: args' -> readback v e @@ fun b' -> lst_to_app (App (b, b')) args' k

(* Evaluator for normal form *)

and value (b : extended_term) (e : env) (k : val_clo_cont) : extended_term =
  match b with
  | Var x -> begin
    match find_opt x e with
    | None ->
      let v = Lst (x, []) in
      k (v, e)
    | Some var -> begin
      match !var with
      | Delayed (b', e') ->
        value b' e' @@ fun closure ->
        var := Computed closure;
        k closure
      | Computed closure -> k closure
    end
  end
  | Abs (x, b') -> k (Lam (x, b'), e)
  | Ext (x, l) -> k (Lst (x, l), e)
  | App (b1, b2) -> begin
    value b1 e @@ fun (v, e') ->
    match v with
    | Lam (x, b') ->
      let e' = add x (ref @@ Delayed (b2, e)) e' in
      value b' e' k
    | Lst (x, l) ->
      value b2 e @@ fun closure ->
      let v' = Lst (x, l @ [ closure ]) in
      k (v', e')
    | _ -> assert false
  end

(* The function of strong cbv eval *)

let eval (t : lambda_term) : lambda_term =
  gensym_reset ();

  let b = term_to_extended t in
  let b' = norm b empty Fun.id in
  extended_to_term b'
