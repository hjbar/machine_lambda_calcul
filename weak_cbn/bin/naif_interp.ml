open Lambda

let rec interp t e =
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

let eval t = interp t (Env StringMap.empty)

let eval_with_env t env = interp t env
