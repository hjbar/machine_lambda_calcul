open Lambda
open Env

let gensym : unit -> string =
  let cpt = ref (-1) in
  fun () ->
    incr cpt;
    Format.sprintf "#x%d" !cpt

(*
let free_var : unit -> string =
  let cpt = ref (-1) in
  fun () ->
    incr cpt;
    Format.sprintf "y%d" !cpt
*)

let abstract_variable (x : identifier) : sem = Neutral (fun () -> Var x)

let to_sem (f : sem -> sem) : sem = Sem f

let env_lookup (x : identifier) (e : env) : sem =
  match Dict.find_opt x e with
  (* | None -> abstract_variable @@ free_var () *)
  | None -> abstract_variable x
  | Some var -> var

let cached_call (c : 'a cache) (t : unit -> 'a) : 'a =
  match !c with
  | None ->
    let cache = t () in
    c := Some cache;
    cache
  | Some cache -> cache

let mount_cache (v : sem) : sem =
  match v with Cache _ -> v | _ -> Cache (ref None, v)
