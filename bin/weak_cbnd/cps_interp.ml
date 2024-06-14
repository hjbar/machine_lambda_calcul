open Lambda
open Env

(* Functions of interp *)

let rec interp (t : lambda_term) (e : env) (k : closure -> closure) : closure =
  match t with
  | Var x -> begin
    let var = find x e in
    let v =
      match !var with
      | Delayed (t', e') ->
        interp t' e' @@ fun v ->
        var := Computed v;
        v
      | Computed v -> v
    in
    k v
  end
  | Abs _ -> k @@ (t, e)
  | App (t1, t2) -> begin
    interp t1 e @@ fun expval ->
    match expval with
    | Abs (x, t'), e' ->
      let e' = add x (ref @@ Delayed (t2, e)) e' in
      interp t' e' k
    | _ -> assert false
  end

(* Functions of eval *)

let eval (t : lambda_term) : lambda_term =
  let t', e' = interp t empty Fun.id in
  replace t' e'

let eval_with_env (t : lambda_term) : closure = interp t empty Fun.id
