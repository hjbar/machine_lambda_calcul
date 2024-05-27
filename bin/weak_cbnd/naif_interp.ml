(* Definition of types *)

open Lambda
module Env = Map.Make (String)

type expval = Closure of lambda_term * env

and stoval =
  | Delayed of lambda_term * env
  | Computed of expval

and env = stoval ref Env.t

(* Functions of interp *)

let rec interp (t : lambda_term) (e : env) : expval =
  match t with
  | Var x -> begin
    let var = Env.find x e in
    match !var with
    | Delayed (t', e') ->
      let v = interp t' e' in
      var := Computed v;
      v
    | Computed v -> v
  end
  | Abs _ -> Closure (t, e)
  | App (t1, t2) -> begin
    match interp t1 e with
    | Closure (Abs (x, t'), e') ->
      let e' = Env.add x (ref @@ Delayed (t2, e)) e' in
      interp t' e'
    | _ -> assert false
  end

(* Eval functions *)

let eval t =
  let (Closure (t, _env)) = interp t Env.empty in
  t
