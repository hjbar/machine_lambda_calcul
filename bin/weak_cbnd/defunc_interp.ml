(* Definition of types *)

open Lambda
open Env

type cont =
  | StoreVal of stoval ref
  | EvalApp of stoval ref

(* Functions of interp *)

let rec interp (t : lambda_term) (e : env) (k : cont list) : closure =
  match t with
  | Var x -> begin
    let var = find x e in
    match !var with
    | Delayed (t', e') -> interp t' e' (StoreVal var :: k)
    | Computed v -> apply v k
  end
  | Abs _ -> apply (t, e) k
  | App (t1, t2) -> interp t1 e (EvalApp (ref @@ Delayed (t2, e)) :: k)

and apply (v : closure) (k : cont list) : closure =
  match k with
  | [] -> v
  | StoreVal var :: k' ->
    var := Computed v;
    apply v k'
  | EvalApp var :: k' -> begin
    match v with
    | Abs (x, t'), e' ->
      let e' = add x var e' in
      interp t' e' k'
    | _ -> assert false
  end

(* Functions of eval *)

let eval (t : lambda_term) : lambda_term =
  let t', e' = interp t empty [] in
  replace t' e'

let eval_with_env (t : lambda_term) : closure = interp t empty []
