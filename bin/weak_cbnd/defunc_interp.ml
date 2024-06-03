(* Definition of types *)

open Lambda
module Env = Map.Make (String)

type closure = lambda_term * env

and stoval =
  | Delayed of closure
  | Computed of closure

and env = stoval ref Env.t

and cont =
  | CONT0
  | CONT1 of stoval ref * cont
  | CONT2 of stoval ref * cont

(* Functions of interp *)

let rec interp (t : lambda_term) (e : env) (k : cont) : closure =
  match t with
  | Var x -> begin
    let var = Env.find x e in
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
      let e' = Env.add x var e' in
      interp t' e' k'
    | _ -> assert false
  end

(* Eval functions *)

let eval t = interp t Env.empty CONT0 |> fst
