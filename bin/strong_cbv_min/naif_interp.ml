open Lambda_ext
open Env

(* Strong Call By Value Evaluator *)

let rec n (b : extended_terms) (e : env) : extended_closure =
  let b', e' = v b e in
  r b' e'

and r (v : value) (e : env) : extended_closure =
  match v with
  | Cst x -> (Var x, e)
  | Lam (x, b) ->
    let y = gensym () in
    let e = add x (Var y, e) e in

    let t = App (Abs (x, b), Ext [ Cst y ]) in
    let t', e' = n t e in

    (Abs (y, t'), e')
  | Lst l ->
    let t_opt =
      List.fold_left
        begin
          fun acc t ->
            let t' = r t e |> fst in
            match acc with None -> Some t' | Some acc' -> Some (App (acc', t'))
        end
        None l
    in
    (Option.get t_opt, e)

(* Evaluator for normal form *)

and v (t : extended_terms) (e : env) : value_closure =
  let t', e' = weak_eval t e in
  let v' =
    match t' with
    | Var x -> Cst x
    | App (t1, t2) -> Lst [ v t1 e' |> fst; v t2 e' |> fst ]
    | Abs (x, t) -> Lam (x, t)
    | Ext l -> Lst l
  in
  (v', e')

and interp (t : extended_terms) (e : env) (k : extended_closure list) : extended_closure =
  match t with
  | Var x -> begin
    match find_opt x e with
    | None ->
      let t' = Ext [ Cst x ] in
      let e' = add x (t', e) e in
      apply t' e' k
    | Some (t', e') -> apply t' e' k
  end
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
              v t' e' |> fst
          end
          k
      in
      let ext' = Ext (l @ l') in
      (ext', e)
    | _ -> assert false
  end

and weak_eval (t : extended_terms) (e : env) : extended_closure = interp t e []

(* The function of strong cbv eval *)

let eval (t : lambda_term) : lambda_term =
  gensym_reset ();
  n (term_to_extended t) empty |> fst |> extended_to_term
