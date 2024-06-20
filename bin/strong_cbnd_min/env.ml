open Lambda_ext
module Env = Map.Make (String)

type env = stoval ref Env.t

and stoval =
  | Delayed of extended_closure
  | Computed of extended_closure

and extended_closure = extended_terms * env

and value_closure = value * env

type cont =
  | CONT1 of stoval ref
  | CONT2 of stoval ref

let empty = Env.empty

let add = Env.add

let find = Env.find

let find_opt = Env.find_opt

let unwrap sto_r = match !sto_r with Delayed c | Computed c -> c

let gensym, gensym_reset = get_gensym ~kind:Interp
