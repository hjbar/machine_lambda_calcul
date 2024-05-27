(* Definition of types *)

open Lambda
module Env = Map.Make (String)

type expval = Closure of lambda_term * env

and stoval =
  | Delayed of lambda_term * env
  | Computed of expval

and env = stoval ref Env.t

(* Functions of interp *)

let rec interp (t : lambda_term) (e : env) (k : expval -> expval) : expval =
  match t with
  | Var x -> begin
    let var = Env.find x e in
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
  | Abs _ -> k @@ Closure (t, e)
  | App (t1, t2) -> begin
    interp t1 e @@ fun expval ->
    match expval with
    | Closure (Abs (x, t'), e') ->
      let e' = Env.add x (ref @@ Delayed (t2, e)) e' in
      interp t' e' k
    | _ -> assert false
  end

(* Eval functions *)

let eval t =
  let (Closure (t, _env)) = interp t Env.empty Fun.id in
  t
