open Lambda

(* Define some types *)

module StringMap = Map.Make (String)

type env = Env of closure StringMap.t

and closure = lambda_term * env

let empty = Env StringMap.empty

let find x env =
  let (Env env) = env in
  StringMap.find x env

let add x elem env =
  let (Env env) = env in
  let env' = StringMap.add x elem env in
  Env env'

(* Functions of interp *)

let rec interp (t : lambda_term) (e : env) (k : closure list) : closure =
  match t with
  | Var x ->
    let t', e' = find x e in
    apply t' e' k
  | Abs _ -> apply t e k
  | App (t1, t2) -> interp t1 e ((t2, e) :: k)

and apply (t : lambda_term) (e : env) (k : closure list) : closure =
  match k with
  | [] -> (t, e)
  | (t2, e2) :: k' -> begin
    match t with
    | Abs (x, t') ->
      let closure = interp t2 e2 k' in
      let e' = add x closure e in
      interp t' e' k'
    | _ -> assert false
  end

(* Functions of eval *)

let eval (t : lambda_term) : lambda_term = interp t empty [] |> fst
