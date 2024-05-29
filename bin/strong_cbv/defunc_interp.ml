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

let rec reify (s : sem) (k : lambda_term -> lambda_term) : lambda_term =
  match s with
  | Sem f ->
    let x = gensym () in
    reify (abstract_variable x |> f) @@ fun t -> k @@ Abs (x, t)
  | Neutral l -> k @@ l ()
  | Cache (c, v) -> cached_call c (fun () -> reify v k)

let to_sem (f : sem -> sem) : sem = Sem f

let rec from_sem (s1 : sem) (s2 : sem) (k : sem -> sem) : sem =
  match s1 with
  | Sem f -> k @@ f s2
  | Neutral l -> apply_neutral l s2 k
  | Cache (c, Neutral l) -> apply_neutral (fun () -> cached_call c l) s2 k
  | Cache (_, v) -> from_sem v s2 k

and apply_neutral (l : unit -> lambda_term) (v : sem) (k : sem -> sem) : sem =
  let f () =
    reify v @@ fun v' ->
    let l' = l () in
    App (l', v')
  in
  k @@ Neutral f

let mount_cache (v : sem) : sem =
  match v with Cache _ -> v | _ -> Cache (ref None, v)

let rec interp (t : lambda_term) (e : env) (k : sem -> sem) : sem =
  match t with
  | Var x -> k @@ env_lookup x e
  | Abs (x, t') ->
    let sem =
      to_sem @@ fun v ->
      let e' = Dict.add x (mount_cache v) e in
      interp t' e' Fun.id
    in
    k sem
  | App (t1, t2) ->
    interp t2 e @@ fun v2 ->
    interp t1 e @@ fun v1 -> from_sem v1 v2 k

(* Functions of interp *)

let eval (t : lambda_term) : lambda_term =
  if true then failwith "DEFUNC TODO";
  let t' = interp t Dict.empty Fun.id in
  reify t' Fun.id
