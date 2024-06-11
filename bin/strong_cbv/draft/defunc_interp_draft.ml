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

type closure = lambda_term * env

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

let to_sem (f : sem -> sem) : sem = Sem f

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

let mount_cache (v : sem) : sem =
  match v with Cache _ -> v | _ -> Cache (ref None, v)

let rec reify (s : sem) (k : closure list) : lambda_term =
  println_flush "Reify";
  match s with
  | Sem _f -> apply_reify s k
  | Neutral l -> l ()
  | Cache (c, v) -> cached_call c (fun () -> reify v k)

and apply_reify (s : sem) (k : closure list) : lambda_term =
  println_flush "Apply_reify";
  match s with
  | Sem f ->
    let x = gensym () in
    let t = reify (abstract_variable x |> f) k in
    Abs (x, t)
  | _ -> assert false

let rec from_sem (s1 : sem) (s2 : sem) (k : closure list) : sem =
  println_flush "From_sem";
  match s1 with
  | Sem f -> f s2
  | Neutral l -> apply_neutral l s2 k
  | Cache (c, Neutral l) -> apply_neutral (fun () -> cached_call c l) s2 k
  | Cache (_, v) -> from_sem v s2 k

and apply_neutral (l : unit -> lambda_term) (v : sem) (k : closure list) : sem =
  println_flush "Apply_neutral";
  let f () =
    let v' = reify v k in
    let l' = l () in
    App (l', v')
  in
  Neutral f

let rec interp (t : lambda_term) (e : env) (k : closure list) : sem =
  println_flush "Interp";
  match t with
  | Var x -> apply_interp (env_lookup x e) k
  | Abs (x, t') ->
    let sem =
      to_sem @@ fun v ->
      let e' = Dict.add x (mount_cache v) e in
      interp t' e' []
    in
    apply_interp sem k
  | App (t1, t2) -> interp t2 e ((t1, e) :: k)

and apply_interp (s : sem) (k : closure list) : sem =
  println_flush "Apply_interp";
  match k with
  | [] -> s
  | (t, e) :: k' ->
    let s' = interp t e [] in
    from_sem s' s k'

(* Functions of interp *)

let eval (t : lambda_term) : lambda_term =
  let t' = interp t Dict.empty [] in
  reify t' []
