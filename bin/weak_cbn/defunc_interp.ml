open Lambda
open Env

(* Functions of interp *)

let rec interp (t : lambda_term) (e : env) (k : closure list) : closure =
  match t with
  | Var x ->
    let t', e' = find x e in
    interp t' e' k
  | Abs _ -> apply t e k
  | App (t1, t2) -> interp t1 e ((t2, e) :: k)

and apply (t : lambda_term) (e : env) (k : (lambda_term * env) list) :
  lambda_term * env =
  match k with
  | [] -> (t, e)
  | (t', e') :: k' -> begin
    match t with
    | Abs (x, t) ->
      let e = add x (t', e') e in
      interp t e k'
    | _ -> assert false
  end

(* Functions of eval *)

let eval t =
  let t', e' = interp t empty [] in
  replace t' e'
