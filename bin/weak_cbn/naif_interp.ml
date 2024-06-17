open Lambda
open Env

(* Functions of interp *)

let rec interp (t : lambda_term) (e : env) : closure =
  match t with
  | Var x ->
    let t', e' = find x e in
    interp t' e'
  | Abs _ -> (t, e)
  | App (t1, t2) -> begin
    match interp t1 e with
    | Abs (x, t'), e' ->
      let e' = add x (t2, e) e' in
      interp t' e'
    | _ -> assert false
  end

(* Functions of eval *)

let _eval (t : lambda_term) : lambda_term =
  let t', e' = interp t empty in
  replace t' e'

let eval (t : lambda_term) : lambda_term =
  let t', e' = interp t empty in
  replace t' e'

let eval_with_env (t : lambda_term) : closure = interp t empty
