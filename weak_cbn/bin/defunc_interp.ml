open Lambda

let rec interp t e k =
  match t with
  | Var x ->
    let t', e' = get_env e |> StringMap.find x in
    interp t' e' k
  | Abs _ -> apply t e k
  | App (t1, t2) -> interp t1 e ((t2, e) :: k)

and apply t e k =
  match k with
  | [] -> (t, e)
  | (t', e') :: k' -> begin
    match t with
    | Abs (x, t) ->
      let e = get_env e |> StringMap.add x (t', e') |> set_env in
      interp t e k'
    | _ -> assert false
  end

let eval t = interp t (Env StringMap.empty) []

let eval_with_env t env = interp t env []
