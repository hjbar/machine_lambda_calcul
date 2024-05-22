(* Definitions of types *)

open Lambda

type env = (string, lambda_term) Hashtbl.t

(* Functions of interp *)

let interp t _e = t

let eval t =
  let env = Hashtbl.create 16 in
  let t' = interp t env in
  (t', env)

let eval_with_env t env =
  let t' = interp t env in
  (t', env)
