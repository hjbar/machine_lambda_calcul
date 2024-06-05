(* Definitions of types for strong cbv *)

open Lambda_ext

(*

(* Definitions of types for weak cbv *)

module StringMap = Map.Make (String)

type env = Env of closure StringMap.t

and closure = extended_terms * env

let empty = Env StringMap.empty

let find x env =
  let (Env env) = env in
  StringMap.find x env

let add x elem env =
  let (Env env) = env in
  let env' = StringMap.add x elem env in
  Env env'

*)

(* Functions for variable names *)

let gensym : unit -> string =
  let cpt = ref (-1) in
  fun () ->
    incr cpt;
    Format.sprintf "x%d" !cpt

(* Functions of strong cbv eval *)

let rec n (b : extended_terms) : extended_terms = r @@ v b

and r : value -> extended_terms = function
  | Cst x -> Var x
  | Lam (x, b) ->
    let y = gensym () in
    let t = App (Abs (x, b), Ext [ Cst y ]) in
    Abs (y, n t)
  | Lst l -> begin
    let t_opt =
      List.fold_left
        begin
          Fun.flip
            begin
              fun t -> function
                | None -> Some (r t)
                | Some t' -> Some (App (t', r t))
            end
        end
        None l
    in
    Option.get t_opt
  end

and v (t : extended_terms) : value =
  match beta_reduce t with
  | Var x -> Cst x
  | App (t1, t2) -> Lst [ v t1; v t2 ]
  | Abs (x, t) -> Lam (x, t)
  | Ext l -> Lst l

(*

(* Functions of weak cbv eval *)

and interp (t : extended_terms) (e : env) (k : closure list) : closure =
  match t with
  | Var x ->
    let t', e' = find x e in
    apply t' e' k
  | Abs _ -> apply t e k
  | App (t1, t2) -> interp t2 e ((t1, e) :: k)
  | Ext _ -> apply t e k

and apply (t : extended_terms) (e : env) (k : closure list) : closure =
  match k with
  | [] -> (t, e)
  | (t2, e2) :: k' -> begin
    match t with
    | Abs (x, t') ->
      let closure = interp t2 e2 [] in
      let e' = add x closure e in
      interp t' e' k'
    | Ext l ->
      let t' = v @@ fst @@ interp t2 e2 [] in
      let ext' = Ext (l @ [ t' ]) in
      apply ext' e k'
    | _ -> assert false
  end

and weak_eval (t : extended_terms) : extended_terms = interp t empty [] |> fst

*)

(*

(* Functions of interp *)

and interp (t : extended_terms) (e : env) : closure =
  match t with
  | Var x -> find x e
  | Abs _ -> (t, e)
  | App (t1, t2) -> begin
    match interp t2 e with
    | Abs (x, t'), e' ->
      let closure = interp t1 e in
      let e' = add x closure e' in
      interp t' e'
    | Ext l, e' ->
      let t', _e' = interp t1 e in
      (Ext (l @ [ v t' ]), e')
    | _ -> assert false
  end
  | Ext _ -> (t, e)

(* Functions of eval *)

and weak_eval (t : extended_terms) : extended_terms = interp t empty |> fst

*)

(* The function of strong cbv eval *)

let eval (t : lambda_term) : lambda_term =
  t |> term_to_extended |> n |> extended_to_term
