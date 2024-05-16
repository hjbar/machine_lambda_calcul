open Lambda
module StringMap = Map.Make (String)

type env = Env of (lambda_term * env) StringMap.t

let get_env e = match e with Env e -> e

let set_env e = Env e

let eval, eval_with_env =
  let rec eval t e k =
    match t with
    | Var x ->
      let t', e' = get_env e |> StringMap.find x in
      eval t' e' k
    | Abs _ -> k t e
    | App (t1, t2) -> begin
      eval t1 e @@ fun t1' e' ->
      match t1' with
      | Abs (x, t') ->
        let e' = get_env e' |> StringMap.add x (t2, e) |> set_env in
        eval t' e' k
      | _ -> assert false
    end
  in
  ( (fun t -> eval t (Env StringMap.empty) (fun t e -> (t, e)))
  , fun t env -> eval t env (fun t e -> (t, e)) )
