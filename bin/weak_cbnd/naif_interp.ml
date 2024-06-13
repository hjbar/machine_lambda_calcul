(* Definition of types *)

open Lambda
open Env

(* Functions of interp *)

let rec interp (t : lambda_term) (e : env) : closure =
  match t with
  | Var x -> begin
    let var = find x e in
    match !var with
    | Delayed (t', e') ->
      let v = interp t' e' in
      var := Computed v;
      v
    | Computed v -> v
  end
  | Abs _ -> (t, e)
  | App (t1, t2) -> begin
    match interp t1 e with
    | Abs (x, t'), e' ->
      let e' = add x (ref @@ Delayed (t2, e)) e' in
      interp t' e'
    | _ -> assert false
  end

(* Eval functions *)

let eval t =
  let t', e' = interp t empty in
  replace t' e'
