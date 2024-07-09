(* Definition of types *)

open Lambda
open Lambda_ext_cbnd

(* Strong Call By Value Evaluator *)

let rec norm (b : extended_term) (e : env) : extended_term =
  let v, e' = value b e in
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

and lst_to_app (b : extended_term) (args : value_closure list) : extended_term =
  match args with
  | [] -> b
  | (v, e) :: args' ->
    let b' = readback v e in
    lst_to_app (App (b, b')) args'

(* Evaluator for normal form *)

and value (b : extended_term) (e : env) : value_closure =
  match b with
  | Var x -> begin
    match find_opt x e with
    | None ->
      let v = Lst (x, []) in
      (v, e)
    | Some var -> begin
      match !var with
      | Delayed (b', e') ->
        let closure = value b' e' in
        var := Computed closure;
        closure
      | Computed closure -> closure
    end
  end
  | Abs (x, b') -> (Lam (x, b'), e)
  | Ext (x, l) -> (Lst (x, l), e)
  | App (b1, b2) -> begin
    match value b1 e with
    | Lam (x, b'), e' ->
      let e' = add x (ref @@ Delayed (b2, e)) e' in
      value b' e'
    | Lst (x, l), e' ->
      let closure = value b2 e in
      let v' = Lst (x, l @ [ closure ]) in
      (v', e')
    | _ -> assert false
  end

(* The function of strong cbv eval *)

let eval (t : lambda_term) : lambda_term =
  gensym_reset ();

  let b = term_to_extended t in
  let b' = norm b empty in
  extended_to_term b'
