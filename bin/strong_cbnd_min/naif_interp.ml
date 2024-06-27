(* Definition of types *)

open Lambda_ext
open Env

(* Strong Cbnd Interp *)

let rec norm (b : extended_terms) (e : env) : extended_closure =
  let b', e' = value b e in
  readback b' e'

and readback (v : value) (e : env) : extended_closure =
  match v with
  | Cst x -> (Var x, e)
  | Lam (x, b) ->
    let y = gensym () in
    let e = add x (ref @@ Delayed (Var y, e)) e in

    let t = App (Abs (x, b), Ext [ Cst y ]) in
    let t', e' = norm t e in

    (Abs (y, t'), e')
  | Lst l ->
    let t_opt =
      List.fold_left
        begin
          fun acc t ->
            let t' = readback t e |> fst in
            match acc with None -> Some t' | Some acc' -> Some (App (acc', t'))
        end
        None l
    in
    (Option.get t_opt, e)

and value (b : extended_terms) (e : env) : value_closure =
  let b', e' = weak_eval b e in
  let v' =
    match b' with
    | Var x -> Cst x
    | App (t1, t2) -> Lst [ value t1 e' |> fst; value t2 e' |> fst ]
    | Abs (x, t) -> Lam (x, t)
    | Ext l -> Lst l
  in
  (v', e')

(* Weak Cbnd Eval *)

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
      let v = value t2' e2' |> fst in
      let ext' = Ext (l @ [ v ]) in
      apply ext' e k'
    | _ -> assert false
  end

and weak_eval (t : extended_terms) (e : env) : extended_closure = interp t e []

(* Functions of eval *)

let eval t =
  gensym_reset ();
  norm (term_to_extended t) empty |> fst |> extended_to_term
