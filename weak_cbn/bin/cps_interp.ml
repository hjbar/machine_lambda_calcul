open Lambda

(* Define some types *)

module StringMap = Map.Make (String)

type env = Env of (lambda_term * env) StringMap.t

let get_env e = match e with Env e -> e

let set_env e = Env e

(* Functions of interp *)

let rec interp (t : lambda_term) (e : env)
  (k : lambda_term -> env -> lambda_term * env) : lambda_term * env =
  match t with
  | Var x ->
    let t', e' = get_env e |> StringMap.find x in
    interp t' e' k
  | Abs _ -> k t e
  | App (t1, t2) -> begin
    interp t1 e @@ fun t1' e' ->
    match t1' with
    | Abs (x, t') ->
      let e' = get_env e' |> StringMap.add x (t2, e) |> set_env in
      interp t' e' k
    | _ -> assert false
  end

(* Functions of eval *)

let eval t = interp t (Env StringMap.empty) (fun t e -> (t, e)) |> fst
