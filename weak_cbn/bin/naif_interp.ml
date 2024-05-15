open Lambda
module StringMap = Map.Make (String)

type env = Env of (lambda_term * env) StringMap.t

let get_env e = match e with Env e -> e

let eval t =
  let rec eval t e =
    match t with
    | Var x ->
      let e = get_env e in
      let t', e' = StringMap.find x e in
      eval t' e'
    | Abs _ as res -> (res, e)
    | App (t1, t2) -> begin
      match eval t1 e with
      | Abs (x, t'), e' ->
        let e' = get_env e' in
        let e' = StringMap.add x (t2, e) e' in
        eval t' (Env e')
      | _ -> assert false
    end
  in
  eval t (Env StringMap.empty)
