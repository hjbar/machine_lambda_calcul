open Lambda

let rec interp t e k =
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

let eval t = interp t (Env StringMap.empty) (fun t e -> (t, e))

let eval_with_env t env = interp t env (fun t e -> (t, e))
