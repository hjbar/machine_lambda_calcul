open Lambda
open Env

(* Functions of interp *)

let rec interp (t : lambda_term) (e : env) (k : closure -> closure) : closure =
  match t with
  | Var x -> k @@ find x e
  | Abs _ -> k (t, e)
  | App (t1, t2) -> begin
    interp t1 e @@ fun (t1', e') ->
    match t1' with
    | Abs (x, t') ->
      interp t2 e @@ fun closure ->
      let e' = add x closure e' in
      interp t' e' k
    | _ -> assert false
  end

(* Functions of eval *)

let eval (t : lambda_term) : lambda_term =
  let t', e' = interp t empty Fun.id in
  replace t' e'

let eval_with_env (t : lambda_term) : closure = interp t empty Fun.id
