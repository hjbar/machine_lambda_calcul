(* Definition of types *)

open Lambda
module Env = Map.Make (String)

type closure = lambda_term * env

and stoval =
  | Delayed of closure
  | Computed of closure

and env = stoval ref Env.t

(* Functions of interp *)

let rec interp (t : lambda_term) (e : env) (k : closure -> closure) : closure =
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
  | Abs _ -> k @@ (t, e)
  | App (t1, t2) -> begin
    interp t1 e @@ fun expval ->
    match expval with
    | Abs (x, t'), e' ->
      let e' = Env.add x (ref @@ Delayed (t2, e)) e' in
      interp t' e' k
    | _ -> assert false
  end

(* Eval functions *)

let eval t = interp t Env.empty Fun.id |> fst
