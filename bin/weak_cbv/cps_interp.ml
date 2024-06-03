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

let rec interp (t : lambda_term) (e : env) (k : closure -> closure) : closure =
  match t with
  | Var x -> k @@ find x e
  | Abs _ -> k (t, e)
  | App (t1, t2) -> begin
    interp t1 e @@ fun (t1', e') ->
    match t1' with
    | Abs (x, t') ->
      interp t2 e @@ fun closure ->
      let e' = add x closure e' in
      interp t' e' k
    | _ -> assert false
  end

(* Functions of eval *)

let eval (t : lambda_term) : lambda_term = interp t empty Fun.id |> fst
