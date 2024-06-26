open Lambda_ext
open Env

(* Strong Call By Value Evaluator *)

let rec n (b : extended_terms) (e : env) (k : extended_closure -> extended_closure) :
  extended_closure =
  let b', e' = v b e Fun.id in
  r b' e' k

and r (v : value) (e : env) (k : extended_closure -> extended_closure) : extended_closure =
  match v with
  | Cst x -> k (Var x, e)
  | Lam (x, b) ->
    let y = gensym () in
    let e = add x (Var y, e) e in

    let t = App (Abs (x, b), Ext [ Cst y ]) in
    n t e @@ fun (t', e') -> k (Abs (y, t'), e')
  | Lst l ->
    Cps.map (fun v -> r v e) l @@ fun l' ->
    let t_opt =
      List.fold_left
        begin
          fun acc (t, _) -> match acc with None -> Some t | Some acc' -> Some (App (acc', t))
        end
        None l'
    in
    k (Option.get t_opt, e)

and v (t : extended_terms) (e : env) (k : value_closure -> value_closure) : value_closure =
  let t', e' = weak_eval t e in
  match t' with
  | Var x -> k (Cst x, e')
  | App (t1, t2) ->
    v t1 e' @@ fun (t1', _) ->
    v t2 e' @@ fun (t2', _) -> k (Lst [ t1'; t2' ], e')
  | Abs (x, t) -> k (Lam (x, t), e')
  | Ext l -> k (Lst l, e')

(* Evaluator for weak normal form *)

and interp (t : extended_terms) (e : env) (k : extended_closure list) : extended_closure =
  match t with
  | Var x ->
    let t', e' = Option.value ~default:(t, empty) (find_opt x e) in
    apply t' e' k
  | App (t1, t2) -> interp t1 e ((t2, e) :: k)
  | Abs _ | Ext _ -> apply t e k

and apply (t : extended_terms) (e : env) (k : extended_closure list) : extended_closure =
  match k with
  | [] -> (t, e)
  | (t2, e2) :: k' -> begin
    match t with
    | Abs (x, t') ->
      let closure = interp t2 e2 [] in
      let e' = add x closure e in
      interp t' e' k'
    | Ext l ->
      let l' =
        List.map
          begin
            fun (t, e) ->
              let t', e' = interp t e [] in
              v t' e' Fun.id |> fst
          end
          k
      in
      let ext' = Ext (l @ l') in
      (ext', e)
    | Var _ | App _ ->
      let t2', e2' = interp t2 e2 [] in
      let t' = App (t, t2') in
      let e' = union e e2' in
      apply t' e' k'
  end

and weak_eval (t : extended_terms) (e : env) : extended_closure = interp t e []

(* The function of strong cbv eval *)

let eval (t : lambda_term) : lambda_term =
  gensym_reset ();
  n (term_to_extended t) empty Fun.id |> fst |> extended_to_term
