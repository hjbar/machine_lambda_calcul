open Lambda
open Env

(* Functions of interp *)

let rec interp (t : lambda_term) (e : env) (k : closure -> closure) : closure =
  match t with
  | Var x ->
    let t', e' = find x e in
    interp t' e' k
  | Abs _ -> k (t, e)
  | App (t1, t2) -> begin
    interp t1 e @@ fun (t1', e') ->
    match t1' with
    | Abs (x, t') ->
      let e' = add x (t2, e) e' in
      interp t' e' k
    | _ -> assert false
  end

(* Functions of eval *)

let eval t =
  let t', e' = interp t empty Fun.id in
  replace t' e'
