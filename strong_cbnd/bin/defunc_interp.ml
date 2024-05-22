(* open Lambda *)

let interp _e t = t

let eval t =
  let env = Hashtbl.create 16 in
  let t' = interp env t in
  (t', env)

let eval_with_env t env =
  let t' = interp env t in
  (t', env)
