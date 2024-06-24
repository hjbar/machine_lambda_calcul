open Lambda_ext
module StringMap = Map.Make (String)

type env = Env of extended_closure StringMap.t

and extended_closure = extended_terms * env

and value_closure = value * env

let empty = Env StringMap.empty

let find x (Env env) = StringMap.find x env

let find_opt x (Env env) = StringMap.find_opt x env

let add x elem (Env env) = Env (StringMap.add x elem env)

let union (Env e1) (Env e2) =
  let e' = StringMap.union (fun _ _ _ -> assert false) e1 e2 in
  Env e'

let gensym, gensym_reset = get_gensym ~kind:Interp
