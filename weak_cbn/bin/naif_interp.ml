open Lambda
module StringMap = Map.Make (String)

type env = Env of (lambda_term * env) StringMap.t

let get_env e = match e with Env e -> e

let set_env e = Env e

let eval t =
  let rec eval t e =
    match t with
    | Var x ->
      let t', e' = get_env e |> StringMap.find x in
      eval t' e'
    | Abs _ -> (t, e)
    | App (t1, t2) -> begin
      match eval t1 e with
      | Abs (x, t'), e' ->
        let e' = get_env e' |> StringMap.add x (t2, e) |> set_env in
        eval t' e'
      | _ -> assert false
    end
  in
  eval t (Env StringMap.empty)
