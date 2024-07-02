(* Definition of types *)

open Lambda_ext
open Env

(* Strong Cbnd Interp *)

let rec norm (b : extended_terms) (e : env) (k : extended_closure -> extended_closure) :
  extended_closure =
  let b', e' = value b e Fun.id in
  readback b' e' k

and readback (v : value) (e : env) (k : extended_closure -> extended_closure) :
  extended_closure =
  match v with
  | Cst x -> k (Var x, e)
  | Lam (x, b) ->
    let y = gensym () in
    let e = add x (ref @@ Delayed (Var y, e)) e in

    let t = App (Abs (x, b), Ext [ Cst y ]) in
    norm t e @@ fun (t', e') -> k (Abs (y, t'), e')
  | Lst l ->
    Cps.map (fun v -> readback v e) l @@ fun l' ->
    let t_opt =
      List.fold_left
        begin
          fun acc (t, _) -> match acc with None -> Some t | Some acc' -> Some (App (acc', t))
        end
        None l'
    in
    k (Option.get t_opt, e)

and value (b : extended_terms) (e : env) (k : value_closure -> value_closure) : value_closure =
  let b', e' = weak_eval b e in
  match b' with
  | Var x -> k (Cst x, e')
  | App (t1, t2) ->
    value t1 e' @@ fun (t1', _) ->
    value t2 e' @@ fun (t2', _) -> k (Lst [ t1'; t2' ], e')
  | Abs (x, t) -> k (Lam (x, t), e')
  | Ext l -> k (Lst l, e')

(* Weak Cbnd Eval *)

and interp (t : extended_terms) (e : env) (k : cont list) : extended_closure =
  match t with
  | Var x -> begin
    match find_opt x e with
    | None ->
      let t' = Ext [ Cst x ] in
      let e' = add x (ref @@ Delayed (Var x, e)) e in
      apply t' e' k
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
      let v = value t2' e2' Fun.id |> fst in
      let ext' = Ext (l @ [ v ]) in
      apply ext' e k'
    | _ -> assert false
  end

and weak_eval (t : extended_terms) (e : env) : extended_closure = interp t e []

(* Functions of eval *)

let eval t =
  gensym_reset ();
  norm (term_to_extended t) empty Fun.id |> fst |> extended_to_term
