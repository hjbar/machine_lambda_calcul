(* Definitions of types *)

open Lambda

type identifier = string

type sem =
  | Sem of (sem -> sem)
  | Neutral of (unit -> lambda_term)
  | Cache of lambda_term cache * sem

and 'a cache = 'a option ref

module Dict = Map.Make (struct
  type t = identifier

  let compare = compare
end)

type env = sem Dict.t

(* Functions for variable names *)

let gensym : unit -> string =
  let cpt = ref (-1) in
  fun () ->
    incr cpt;
    Format.sprintf "x%d" !cpt

let free_var : unit -> string =
  let cpt = ref (-1) in
  fun () ->
    incr cpt;
    Format.sprintf "y%d" !cpt

(* Functions for interp *)

let abstract_variable (x : identifier) : sem = Neutral (fun () -> Var x)

let env_lookup (x : identifier) (e : env) : sem =
  match Dict.find_opt x e with
  | None -> abstract_variable @@ free_var ()
  | Some var -> var

let cached_call (c : 'a cache) (t : unit -> 'a) : 'a =
  match !c with
  | None ->
    let cache = t () in
    c := Some cache;
    cache
  | Some cache -> cache

let rec reify : sem -> lambda_term = function
  | Sem f ->
    let x = gensym () in
    Abs (x, abstract_variable x |> f |> reify)
  | Neutral l -> l ()
  | Cache (c, v) -> cached_call c (fun () -> reify v)

let to_sem (f : sem -> sem) : sem = Sem f

let rec from_sem : sem -> sem -> sem = function
  | Sem f -> f
  | Neutral l -> apply_neutral l
  | Cache (c, Neutral l) -> apply_neutral (fun () -> cached_call c l)
  | Cache (_, v) -> from_sem v

and apply_neutral (l : unit -> lambda_term) (v : sem) : sem =
  let f () =
    let v' = reify v in
    let l' = l () in
    App (l', v')
  in
  Neutral f

let mount_cache (v : sem) : sem =
  match v with Cache _ -> v | _ -> Cache (ref None, v)

let rec interp (t : lambda_term) (e : env) : sem =
  match t with
  | Var x -> env_lookup x e
  | Abs (x, t') ->
    to_sem @@ fun v ->
    let e' = Dict.add x (mount_cache v) e in
    interp t' e'
  | App (t1, t2) ->
    let v2 = interp t2 e in
    let v1 = interp t1 e in
    from_sem v1 v2

(* Functions of interp *)

let eval (t : lambda_term) : lambda_term =
  interp t Dict.empty |> reify |> ignore;
  failwith "CPS TODO"
