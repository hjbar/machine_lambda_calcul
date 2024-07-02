open Lambda_ext_cbnd_def

(* Env functions *)

let empty = StringMap.empty

let add = StringMap.add

let find_opt = StringMap.find_opt

(* Gensym *)

let gensym, gensym_reset = Utils.get_gensym ~kind:Interp
