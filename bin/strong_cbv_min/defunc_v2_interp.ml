open Lambda_ext
open Env

(* Strong Call By Value Evaluator *)

let rec n (b : extended_terms) (e : env)
  (k : extended_closure -> extended_closure) : extended_closure =
  v b e @@ fun (b', e') -> r b' e' k

and r (v : value) (e : env) (k : extended_closure -> extended_closure) :
  extended_closure =
  match v with
  | Cst x -> k (Var x, e)
  | Lam (x, b) ->
    let y = gensym () in
    let t = App (Abs (x, b), Ext [ Cst y ]) in
    n t e @@ fun (t', e') -> k (Abs (y, t'), e')
  | Lst l ->
    Cps.map (fun v -> r v e) l @@ fun l' ->
    let t_opt =
      List.fold_left
        begin
          fun acc (t, _e) ->
            match acc with None -> Some t | Some acc' -> Some (App (acc', t))
        end
        None l'
    in
    k (Option.get t_opt, e)

and v (t : extended_terms) (e : env) (k : value_closure -> extended_closure) :
  extended_closure =
  weak_eval t e @@ fun (t', e') ->
  match t' with
  | Var x -> k (Cst x, e')
  | App (t1, t2) ->
    v t1 e' @@ fun (t1', _) ->
    v t2 e' @@ fun (t2', _) -> k @@ (Lst [ t1'; t2' ], e')
  | Abs (x, t) -> k (Lam (x, t), e')
  | Ext l -> k (Lst l, e')

(* Evaluator for weak normal form *)

and interp (t : extended_terms) (e : env) (k : extended_closure list)
  (cont : extended_closure -> extended_closure) : extended_closure =
  match t with
  | Var x ->
    let t', e' = Option.value ~default:(t, e) (find_opt x e) in
    apply t' e' k cont
  | Abs _ -> apply t e k cont
  | App (t1, t2) -> interp t1 e ((t2, e) :: k) cont
  | Ext _ -> apply t e k cont

and apply (t : extended_terms) (e : env) (k : extended_closure list)
  (cont : extended_closure -> extended_closure) : extended_closure =
  match k with
  | [] -> cont (t, e)
  | (t2, e2) :: k' -> begin
    match t with
    | Abs (x, t') ->
      interp t2 e2 [] @@ fun closure ->
      let e' = add x closure e in
      interp t' e' k' cont
    | Ext l ->
      Cps.map (fun (t, e) -> interp t e []) k @@ fun ext_closure_args ->
      Cps.map (fun (t, e) -> v t e) ext_closure_args @@ fun val_closure_args ->
      let l' = List.map fst val_closure_args in
      let ext' = Ext (l @ l') in
      cont (ext', e)
    | Var _ ->
      Cps.map (fun (t, e) -> interp t e []) k @@ fun interp_k ->
      let closure =
        List.fold_left
          begin
            fun (acc, _) (t', e') -> (App (acc, t'), e')
          end
          (t, e) interp_k
      in
      cont closure
    | _ -> assert false
  end

and weak_eval (t : extended_terms) (e : env)
  (cont : extended_closure -> extended_closure) : extended_closure =
  interp t e [] cont

(* The function of strong cbv eval *)

let eval (t : lambda_term) : lambda_term =
  ignore @@ failwith "DEFUNC V2 TODO";
  n (term_to_extended t) empty Fun.id |> fst |> extended_to_term
