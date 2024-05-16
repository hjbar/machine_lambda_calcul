open Lambda

let gensym x = x ^ "'"

let force ref_state =
  match !ref_state with
  | Done value -> value
  | Todo term ->
    let v = Term term in
    ref_state := Done v;
    v

let memothunk thunk = ref @@ Todo thunk

let reify value = match value with Foo (c, _) -> force c | Term t -> Term t

let apply_value value w =
  match value with
  | Foo (_, f) -> f w
  | Term t -> begin
    match reify (Term w) with Term w -> Term (App (t, w)) | _ -> assert false
  end

let rec interp e t =
  match t with
  | Var x ->
    let res =
      match Hashtbl.find_opt e x with
      | None -> ref (Done (Term (Var x)))
      | Some res -> res
    in
    force res
  | App (t, u) -> begin
    let i1 = interp e t in
    match interp e u with Term i2 -> apply_value i1 i2 | _ -> assert false
  end
  | Abs (x, t) ->
    let f v =
      Hashtbl.replace e x (memothunk v);
      interp e t
    in
    let x1 = gensym x in
    let term =
      match reify (f (Var x1)) with Term t -> Abs (x1, t) | _ -> assert false
    in
    Foo (memothunk term, f)

let eval t =
  let env = Hashtbl.create 16 in
  match reify (interp env t) with Term t -> (t, env) | _ -> assert false

let eval_with_env t env =
  match reify (interp env t) with Term t -> (t, env) | _ -> assert false
