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
    | Abs _ -> apply t e k
    | App (t1, t2) -> eval t1 e ((t2, e) :: k)
  and apply t e k =
    match k with
    | [] -> (t, e)
    | (t', e') :: _k' -> begin
      match t' with
      | Abs (x, t') ->
        let e = get_env e |> StringMap.add x (t', e') |> set_env in
        eval t e k
      | _ -> assert false
    end
  in
  ((fun t -> eval t (Env StringMap.empty) []), fun t env -> eval t env [])
