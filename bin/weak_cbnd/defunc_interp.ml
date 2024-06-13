(* Definition of types *)

open Lambda
open Env

type cont =
  | CONT0
  | CONT1 of stoval ref * cont
  | CONT2 of stoval ref * cont

(* Functions of interp *)

let rec interp (t : lambda_term) (e : env) (k : cont) : closure =
  match t with
  | Var x -> begin
    let var = find x e in
    match !var with
    | Delayed (t', e') -> interp t' e' (CONT1 (var, k))
    | Computed v -> apply v k
  end
  | Abs _ -> apply (t, e) k
  | App (t1, t2) -> interp t1 e (CONT2 (ref @@ Delayed (t2, e), k))

and apply (v : closure) (k : cont) : closure =
  match k with
  | CONT0 -> v
  | CONT1 (var, k') ->
    var := Computed v;
    apply v k'
  | CONT2 (var, k') -> begin
    match v with
    | Abs (x, t'), e' ->
      let e' = add x var e' in
      interp t' e' k'
    | _ -> assert false
  end

(* Eval functions *)

let eval t =
  let t', e' = interp t empty CONT0 in
  replace t' e'
