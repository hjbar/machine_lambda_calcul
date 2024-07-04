open Lambda_ext_cbv_def

(* Env functions *)

let empty = Env StringMap.empty

let find_opt x (Env env) = StringMap.find_opt x env

let add x elem (Env env) = Env (StringMap.add x elem env)

(* Gensym *)

let gensym, gensym_reset = Utils.get_gensym ~kind:Interp
