open Lambda
open Env
open Utils

let debug = false

(* Functions for interp *)

let rec reify (s : sem) (k : lambda_term list) : lambda_term =
  if debug then println_flush "REIFY";
  match s with
  | Sem _f -> apply_reify s k
  | Neutral l -> l ()
  | Cache (c, v) -> cached_call c (fun () -> reify v k)

and apply_reify (s : sem) (k : lambda_term list) : lambda_term =
  if debug then println_flush "APPLY_REIFY";
  match s with
  | Sem f ->
    let x = gensym () in
    let t = reify (f @@ abstract_variable x) k in
    Abs (x, t)
  | _ -> assert false

let rec from_sem (s1 : sem) (s2 : sem) (k : sem list) : sem =
  if debug then println_flush "FROM_SEM";
  match s1 with
  | Sem f -> f s2
  | Neutral l -> apply_neutral l s2 k
  | Cache (c, Neutral l) -> apply_neutral (fun () -> cached_call c l) s2 k
  | Cache (_, v) -> from_sem v s2 k

and apply_neutral (l : unit -> lambda_term) (v : sem) (_k : sem list) : sem =
  if debug then println_flush "APPLY_NEUTRAL";
  let f () =
    let v' = reify v [] in
    let l' = l () in
    App (l', v')
  in
  Neutral f

let rec interp (t : lambda_term) (e : env) (k : sem list) : sem =
  if debug then println_flush "INTERP";
  match t with
  | Var _ -> apply_interp t e k
  | Abs _ -> apply_interp t e k
  | App (t1, t2) ->
    let t2' = apply_interp t2 e k in
    interp t1 e (t2' :: k)

and apply_interp (t : lambda_term) (e : env) (k : sem list) : sem =
  if debug then println_flush "APPLY_INTERP";
  match k with
  (*
  | [] -> interp t e []
  *)
  | [] -> begin
    match t with
    | Var x -> env_lookup x e
    | Abs (x, t') ->
      to_sem @@ fun v ->
      let e' = Dict.add x (mount_cache v) e in
      interp t' e' []
    | App _ -> interp t e []
  end
  | sem' :: k' -> begin
    match t with
    | Var x ->
      let sem = env_lookup x e in
      from_sem sem sem' k'
    | Abs (x, t') ->
      let sem =
        to_sem @@ fun v ->
        let e' = Dict.add x (mount_cache v) e in
        interp t' e' []
      in
      from_sem sem sem' k'
    | _ -> assert false
  end

(* Functions of interp *)

let eval (t : lambda_term) : lambda_term =
  ignore @@ failwith "DEFUNC TODO";
  let t' = interp t Dict.empty [] in
  reify t' []
