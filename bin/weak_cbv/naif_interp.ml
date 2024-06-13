open Lambda
open Env

(* Functions of interp *)

let rec interp (t : lambda_term) (e : env) : closure =
  match t with
  | Var x -> find x e
  | Abs _ -> (t, e)
  | App (t1, t2) -> begin
    match interp t1 e with
    | Abs (x, t'), e' ->
      let closure = interp t2 e in
      let e' = add x closure e' in
      interp t' e'
    | _ -> assert false
  end

(* Functions of eval *)

let eval (t : lambda_term) : lambda_term =
  let t', e' = interp t empty in
  replace t' e'
