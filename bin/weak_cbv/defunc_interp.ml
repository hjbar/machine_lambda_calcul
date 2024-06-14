open Lambda
open Env

(* Functions of interp *)

let rec interp (t : lambda_term) (e : env) (k : closure list) : closure =
  match t with
  | Var x ->
    let t', e' = find x e in
    apply t' e' k
  | Abs _ -> apply t e k
  | App (t1, t2) -> interp t1 e ((t2, e) :: k)

and apply (t : lambda_term) (e : env) (k : closure list) : closure =
  match k with
  | [] -> (t, e)
  | (t2, e2) :: k' -> begin
    match t with
    | Abs (x, t') ->
      let closure = interp t2 e2 [] in
      let e' = add x closure e in
      interp t' e' k'
    | _ -> assert false
  end

(* Functions of eval *)

let eval (t : lambda_term) : lambda_term =
  let t', e' = interp t empty [] in
  replace t' e'

let eval_with_env (t : lambda_term) : closure = interp t empty []
