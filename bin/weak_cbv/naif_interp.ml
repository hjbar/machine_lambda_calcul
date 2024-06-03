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

let rec interp (t : term) (e : env) : term * env =
  match t with
  | Value (Var x) -> find x e
  | Value (Abs _) -> (t, e)
  | Comp (App (t1, t2)) -> begin
    match interp t1 e with
    | Value (Abs (x, t')), e' ->
      let closure = interp t2 e in
      let e' = add x closure e' in
      interp t' e'
    | _ -> assert false
  end

(* Functions of eval *)

let eval (t : lambda_term) : lambda_term =
  t |> term_to_eval |> Fun.flip interp @@ empty |> fst |> eval_to_term
