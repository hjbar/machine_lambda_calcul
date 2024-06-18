open Lambda
open Env
open Utils

(* Functions for interp *)

let rec reify : sem -> lambda_term = function
  | Sem f ->
    let x = gensym () in
    Abs (x, abstract_variable x |> f |> reify)
  | Neutral l -> l ()
  | Cache (c, v) -> cached_call c (fun () -> reify v)

let rec from_sem : sem -> sem -> sem = function
  | Sem f -> f
  | Neutral l -> apply_neutral l
  | Cache (c, Neutral l) -> apply_neutral (fun () -> cached_call c l)
  | Cache (_, v) -> from_sem v

and apply_neutral (l : unit -> lambda_term) (v : sem) : sem =
  let f () =
    let v' = reify v in
    let l' = l () in
    App (l', v')
  in
  Neutral f

let rec interp (t : lambda_term) (e : env) : sem =
  match t with
  | Var x -> env_lookup x e
  | Abs (x, t') ->
    to_sem @@ fun v ->
    let e' = Dict.add x (mount_cache v) e in
    interp t' e'
  | App (t1, t2) ->
    let v2 = interp t2 e in
    let v1 = interp t1 e in
    from_sem v1 v2

(* Functions of interp *)

let eval (t : lambda_term) : lambda_term = interp t Dict.empty |> reify
