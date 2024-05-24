open Lambda

(* Define some types *)

module StringMap = Map.Make (String)

type env = Env of (lambda_term * env) StringMap.t

let get_env e = match e with Env e -> e

let set_env e = Env e

(* Functions of interp *)

let rec interp (t : lambda_term) (e : env) : lambda_term * env =
  match t with
  | Var x ->
    let t', e' = get_env e |> StringMap.find x in
    interp t' e'
  | Abs _ -> (t, e)
  | App (t1, t2) -> begin
    match interp t1 e with
    | Abs (x, t'), e' ->
      let e' = get_env e' |> StringMap.add x (t2, e) |> set_env in
      interp t' e'
    | _ -> assert false
  end

(* Functions of eval *)

let eval t = interp t (Env StringMap.empty) |> fst
