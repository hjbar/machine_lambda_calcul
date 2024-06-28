open Lambda
open Utils

(* Definition of types *)

type identifier = string

type sem =
  | Sem of (sem -> sem)
  | Neutral of (unit -> lambda_term)
  | Cache of lambda_term cache * sem

and 'a cache = 'a option ref

(* Definition of env *)

module Dict = Map.Make (struct
  type t = identifier

  let compare = compare
end)

type env = sem Dict.t

(* Gensym *)

let gensym, reset_gensym = get_gensym ~kind:Interp

(* Some utils functions for interp *)

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

let mount_cache (v : sem) : sem = match v with Cache _ -> v | _ -> Cache (ref None, v)
