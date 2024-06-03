open Lambda_eval

(* Define some types *)

module StringMap = Map.Make (String)

type env = Env of (term * env) StringMap.t

let empty = Env StringMap.empty

let find x env =
  let (Env env) = env in
  StringMap.find x env

let add x elem env =
  let (Env env) = env in
  let env' = StringMap.add x elem env in
  Env env'

(* Functions of interp *)

let rec interp (t : term) (e : env) (k : term * env -> term * env) : term * env
    =
  match t with
  | Value (Var x) -> k @@ find x e
  | Value (Abs _) -> k (t, e)
  | Comp (App (t1, t2)) -> begin
    interp t1 e @@ fun (t1', e') ->
    match t1' with
    | Value (Abs (x, t')) ->
      interp t2 e @@ fun closure ->
      let e' = add x closure e' in
      interp t' e' k
    | _ -> assert false
  end

(* Functions of eval *)

let eval (t : lambda_term) : lambda_term =
  interp (term_to_eval t) empty Fun.id |> fst |> eval_to_term
