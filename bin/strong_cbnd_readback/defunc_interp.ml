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

let rec norm (t : extended_term) (e : env) (k : ext_cont) : extended_term =
  value t e @@ Readback k

and readback (v : value) (e : env) (k : ext_cont) : extended_term =
  match v with
  | Cst x -> apply_ext (Var x) k
  | Lam (x, b) ->
    let y = gensym () in
    let t = App (Abs (x, b), Ext (y, [])) in
    norm t e @@ RebuildAbs (y, k)
  | Lst (x, l) -> lst_to_app (Var x) l k

and lst_to_app (t : extended_term) (args : value_closure list) (k : ext_cont) : extended_term =
  match args with
  | [] -> apply_ext t k
  | (v, e) :: args' -> readback v e @@ ArgsToApp (t, args', k)

and apply_ext (t : extended_term) (k : ext_cont) : extended_term =
  match k with
  | Id -> t
  | ArgsToApp (t', args, k') -> lst_to_app (App (t', t)) args k'
  | RebuildAbs (y, k') -> apply_ext (Abs (y, t)) k'

(* Evaluator for normal form *)

and value (t : extended_term) (e : env) (k : val_cont) : extended_term =
  match t with
  | Var x -> begin
    match find_opt x e with
    | None -> apply_val (Lst (x, [])) e k
    | Some var -> begin
      match !var with
      | Delayed (t', e') -> value t' e' @@ StoreVal (var, k)
      | Computed (t', e') -> apply_val t' e' k
    end
  end
  | Abs (x, t') -> apply_val (Lam (x, t')) e k
  | Ext (x, l) -> apply_val (Lst (x, l)) e k
  | App (t1, t2) -> value t1 e @@ Val (t2, e, k)

and apply_val (v : value) (e : env) (k : val_cont) : extended_term =
  match k with
  | Readback k' -> readback v e k'
  | StoreVal (var, k') ->
    var := Computed (v, e);
    apply_val v e k'
  | CaseLst (x, l, e', k') ->
    let v' = Lst (x, l @ [ (v, e) ]) in
    apply_val v' e' k'
  | Val (t2, e2, k') -> begin
    match v with
    | Lam (x, t) ->
      let e = add x (ref @@ Delayed (t2, e2)) e in
      value t e k'
    | Lst (x, l) -> value t2 e2 @@ CaseLst (x, l, e, k')
    | _ -> assert false
  end

(* The function of strong cbv eval *)

let eval (t : lambda_term) : lambda_term =
  gensym_reset ();
  norm (term_to_extended t) empty Id |> extended_to_term
