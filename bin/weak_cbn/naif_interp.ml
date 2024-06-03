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

let rec interp (t : lambda_term) (e : env) : lambda_term * env =
  match t with
  | Var x ->
    let t', e' = find x e in
    interp t' e'
  | Abs _ -> (t, e)
  | App (t1, t2) -> begin
    match interp t1 e with
    | Abs (x, t'), e' ->
      let e' = add x (t2, e) e' in
      interp t' e'
    | _ -> assert false
  end

(* Functions of eval *)

let eval t = interp t empty |> fst
