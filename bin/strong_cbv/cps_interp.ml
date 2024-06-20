(* Definitions of types *)

open Lambda
open Env

(* Functions for interp *)

let rec reify (s : sem) (k : lambda_term -> lambda_term) : lambda_term =
  match s with
  | Sem f ->
    let x = gensym () in
    reify (abstract_variable x |> f) @@ fun t -> k @@ Abs (x, t)
  | Neutral l -> k @@ l ()
  | Cache (c, v) -> k @@ cached_call c (fun () -> reify v Fun.id)

let rec from_sem (s1 : sem) (s2 : sem) (k : sem -> sem) : sem =
  match s1 with
  | Sem f -> k @@ f s2
  | Neutral l -> apply_neutral l s2 k
  | Cache (c, Neutral l) -> apply_neutral (fun () -> cached_call c l) s2 k
  | Cache (_, v) -> from_sem v s2 k

and apply_neutral (l : unit -> lambda_term) (v : sem) (k : sem -> sem) : sem =
  let f () =
    reify v @@ fun v' ->
    let l' = l () in
    App (l', v')
  in
  k @@ Neutral f

let rec interp (t : lambda_term) (e : env) (k : sem -> sem) : sem =
  match t with
  | Var x -> k @@ env_lookup x e
  | Abs (x, t') ->
    let sem =
      to_sem @@ fun v ->
      let e' = Dict.add x (mount_cache v) e in
      interp t' e' Fun.id
    in
    k sem
  | App (t1, t2) ->
    interp t2 e @@ fun v2 ->
    interp t1 e @@ fun v1 -> from_sem v1 v2 k

(* Functions of interp *)

let eval (t : lambda_term) : lambda_term =
  gensym_reset ();
  let t' = interp t Dict.empty Fun.id in
  reify t' Fun.id
