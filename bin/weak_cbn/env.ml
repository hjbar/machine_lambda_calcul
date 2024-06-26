open Lambda
module StringMap = Map.Make (String)

type env = Env of closure StringMap.t

and closure = lambda_term * env

let empty = Env StringMap.empty

let find x env =
  let (Env env) = env in
  StringMap.find x env

let find_opt x env =
  let (Env env) = env in
  StringMap.find_opt x env

let add x elem env =
  let (Env env) = env in
  let env' = StringMap.add x elem env in
  Env env'

let rec replace t e =
  match t with
  | Var x -> begin match find_opt x e with None -> Var x | Some (t', e') -> replace t' e' end
  | App (t1, t2) -> App (replace t1 e, replace t2 e)
  | Abs (x, t) -> Abs (x, replace t e)
