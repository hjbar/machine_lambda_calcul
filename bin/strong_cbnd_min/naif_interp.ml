(* Definition of types *)

open Lambda_ext
open Env

let debug = false

(* Strong Cbnd Interp *)

let rec norm (b : extended_terms) (e : env) : extended_closure =
  let b', e' = value b e in
  readback b' e'

and readback (v : value) (e : env) : extended_closure =
  match v with
  | Cst x -> (Var x, e)
  | Lam (x, b) ->
    let y = gensym () in
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

and interp (t : extended_terms) (e : env) (k : cont) : extended_closure =
  match t with
  | Var x -> begin
    match find_opt x e with
    | None -> apply t e k
    | Some var -> begin
      match !var with
      | Delayed (t', e') -> interp t' e' (CONT1 (var, k))
      | Computed (t', e') -> apply t' e' k
    end
  end
  | Abs _ -> apply t e k
  | App (t1, t2) -> interp t1 e (CONT2 (ref @@ Delayed (t2, e), k))
  | Ext _ -> apply t e k

and apply (t : extended_terms) (e : env) (k : cont) : extended_closure =
  match k with
  | CONT0 -> (t, e)
  | CONT1 (var, k') ->
    var := Computed (t, e);
    apply t e k'
  | CONT2 (var, k') -> begin
    match t with
    | Abs (x, t') ->
      let e' = add x var e in
      interp t' e' k'
    | Ext l ->
      let t2, e2 = unwrap var in
      let t2', e2' = interp t2 e2 CONT0 in
      let v = value t2' e2' |> fst in
      let ext' = Ext (l @ [ v ]) in
      apply ext' e k'
    | Var _ | App _ ->
      let t2, e2 = unwrap var in
      let t2' = interp t2 e2 CONT0 |> fst in
      let t' = App (t, t2') in
      apply t' e k'
  end

and weak_eval (t : extended_terms) (e : env) : extended_closure =
  interp t e CONT0

(* Functions of eval *)

let eval t =
  gensym_reset ();

  if debug then begin
    print_newline ();
    println_flush "INPUT :";
    pp_lambda t;
    print_newline ()
  end;

  let t' = norm (term_to_extended t) empty |> fst |> extended_to_term in

  if debug then begin
    println_flush "OUTPUT :";
    pp_lambda t';
    print_newline ()
  end;

  t'
