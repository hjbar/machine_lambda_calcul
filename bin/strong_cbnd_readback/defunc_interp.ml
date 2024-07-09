(* Definition of types *)

open Lambda
open Lambda_ext_cbnd

type ext_cont =
  | Id
  | RebuildAbs of string * ext_cont
  | ArgsToApp of extended_term * value_closure list * ext_cont

type val_cont =
  | Readback of ext_cont
  | StoreVal of stoval ref * val_cont
  | CaseLst of string * value_closure list * env * val_cont
  | Val of extended_term * env * val_cont

(* Strong Call By Value Evaluator *)

let rec norm (b : extended_term) (e : env) (k : ext_cont) : extended_term =
  value b e @@ Readback k

and readback (v : value) (e : env) (k : ext_cont) : extended_term =
  match v with
  | Cst x -> apply_ext (Var x) k
  | Lam (x, b) ->
    let y = gensym () in
    let t = App (Abs (x, b), Ext (y, [])) in
    norm t e @@ RebuildAbs (y, k)
  | Lst (x, l) -> lst_to_app (Var x) l k

and lst_to_app (b : extended_term) (args : value_closure list) (k : ext_cont) : extended_term =
  match args with
  | [] -> apply_ext b k
  | (v, e) :: args' -> readback v e @@ ArgsToApp (b, args', k)

and apply_ext (b : extended_term) (k : ext_cont) : extended_term =
  match k with
  | Id -> b
  | ArgsToApp (b', args, k') -> lst_to_app (App (b', b)) args k'
  | RebuildAbs (y, k') -> apply_ext (Abs (y, b)) k'

(* Evaluator for normal form *)

and value (b : extended_term) (e : env) (k : val_cont) : extended_term =
  match b with
  | Var x -> begin
    match find_opt x e with
    | None -> apply_val (Lst (x, [])) e k
    | Some var -> begin
      match !var with
      | Delayed (b', e') -> value b' e' @@ StoreVal (var, k)
      | Computed (b', e') -> apply_val b' e' k
    end
  end
  | Abs (x, b') -> apply_val (Lam (x, b')) e k
  | Ext (x, l) -> apply_val (Lst (x, l)) e k
  | App (b1, b2) -> value b1 e @@ Val (b2, e, k)

and apply_val (v : value) (e : env) (k : val_cont) : extended_term =
  match k with
  | Readback k' -> readback v e k'
  | StoreVal (var, k') ->
    var := Computed (v, e);
    apply_val v e k'
  | CaseLst (x, l, e', k') ->
    let v' = Lst (x, l @ [ (v, e) ]) in
    apply_val v' e' k'
  | Val (b2, e2, k') -> begin
    match v with
    | Lam (x, b) ->
      let e = add x (ref @@ Delayed (b2, e2)) e in
      value b e k'
    | Lst (x, l) -> value b2 e2 @@ CaseLst (x, l, e, k')
    | _ -> assert false
  end

(* The function of strong cbv eval *)

let eval (t : lambda_term) : lambda_term =
  gensym_reset ();

  let b = term_to_extended t in
  let b' = norm b empty Id in
  extended_to_term b'
