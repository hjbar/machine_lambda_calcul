(* Definition of types *)

open Lambda_ext
open Env

(* Strong Cbnd Interp *)

let rec norm (b : extended_terms) (e : env) : extended_closure =
  let b', e' = value b e [] in
  readback b' e'

and readback (v : value) (e : env) : extended_closure =
  match v with
  | Cst x ->
    let b = Var x |> Option.some in
    apply_readback b e []
  | Lam (x, b) ->
    let y = gensym () in

    let e = add x (ref @@ Delayed (Var y, e)) e in
    let t = App (Abs (x, b), Ext [ Cst y ]) in

    let t', e' = norm t e in
    let b = Abs (y, t') |> Option.some in

    apply_readback b e' []
  | Lst l -> apply_readback None e l

and apply_readback (b : extended_terms option) (e : env) (k : value list) : extended_closure =
  match k with
  | [] -> (Option.get b, e)
  | v :: k' -> begin
    match b with
    | None ->
      let b' = readback v e |> fst |> Option.some in
      apply_readback b' e k'
    | Some b ->
      let b2 = readback v e |> fst in
      let b' = App (b, b2) |> Option.some in
      apply_readback b' e k'
  end

and value (b : extended_terms) (e : env) (k : extended_closure list) : value_closure =
  let b', e' = weak_eval b e in
  match b' with
  | Var x -> apply_value (Cst x) e' k
  | Abs (x, t) -> apply_value (Lam (x, t)) e' k
  | Ext l -> apply_value (Lst l) e' k
  | App (t1, t2) -> value t1 e' ((t2, e') :: k)

and apply_value (v : value) (e : env) (k : extended_closure list) : value_closure =
  match k with
  | [] -> (v, e)
  | (b2, e2) :: k' ->
    let v2, e2' = value b2 e2 [] in
    let v' = Lst [ v; v2 ] in
    apply_value v' e2' k'

(* Weak Cbnd Eval *)

and weak_eval (t : extended_terms) (e : env) : extended_closure = interp t e []

and interp (t : extended_terms) (e : env) (k : cont list) : extended_closure =
  match t with
  | Var x -> begin
    match find_opt x e with
    | None -> apply (Ext [ Cst x ]) empty k
    | Some var -> begin
      match !var with
      | Delayed (t', e') -> interp t' e' (CONT1 var :: k)
      | Computed (t', e') -> apply t' e' k
    end
  end
  | App (t1, t2) -> interp t1 e (CONT2 (ref @@ Delayed (t2, e)) :: k)
  | Abs _ | Ext _ -> apply t e k

and apply (t : extended_terms) (e : env) (k : cont list) : extended_closure =
  match k with
  | [] -> (t, e)
  | CONT1 var :: k' ->
    var := Computed (t, e);
    apply t e k'
  | CONT2 var :: k' -> begin
    match t with
    | Abs (x, t') ->
      let e' = add x var e in
      interp t' e' k'
    | Ext l ->
      let t2, e2 = unwrap var in
      let t2', e2' = interp t2 e2 [] in
      let v = value t2' e2' [] |> fst in
      let ext' = Ext (l @ [ v ]) in
      apply ext' e k'
    | _ -> assert false
  end

(* Functions of eval *)

let eval t =
  gensym_reset ();
  norm (term_to_extended t) empty |> fst |> extended_to_term
