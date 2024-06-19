open Lambda
open Env

(* Gensym *)

let gensym, gensym_reset = get_gensym ~kind:Interp

(* Utils for interp *)

let abstract_variable (x : identifier) : sem = Neutral (fun () -> Var x)

let to_sem (f : sem -> sem) : sem = Sem f

let env_lookup (x : identifier) (e : env) : sem =
  match Dict.find_opt x e with None -> abstract_variable x | Some var -> var

let cached_call (c : 'a cache) (t : unit -> 'a) : 'a =
  match !c with
  | None ->
    let cache = t () in
    c := Some cache;
    cache
  | Some cache -> cache

let mount_cache (v : sem) : sem =
  match v with Cache _ -> v | _ -> Cache (ref None, v)
