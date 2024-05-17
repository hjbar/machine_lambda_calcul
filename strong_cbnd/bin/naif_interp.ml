open Lambda

(* Some utils functions *)

let get_term t = match t with Term t -> t | _ -> assert false

let set_term t = Term t

let set_done v = Done v

(* Some interp functions *)

let gensym x = x ^ "'"

let force ref_state =
  match !ref_state with
  | Done value -> value
  | Todo term ->
    let v = Term term in
    ref_state := Done v;
    v

let memothunk thunk = ref @@ Todo thunk

let reify value = match value with Foo (c, _) -> force c | Term _ -> value

let apply_value value w =
  match value with
  | Foo (_, f) -> f w
  | Term t ->
    let w = set_term w |> reify |> get_term in
    App (t, w) |> set_term

let rec interp e t =
  match t with
  | Var x ->
    let res =
      match Hashtbl.find_opt e x with
      | None -> Var x |> set_term |> set_done |> ref
      | Some res -> res
    in
    force res
  | App (t, u) ->
    let i1 = interp e t in
    let i2 = interp e u |> get_term in
    apply_value i1 i2 (* ERROR ICI *)
  | Abs (x, t) ->
    let f v =
      Hashtbl.replace e x (memothunk v);
      interp e t
    in
    let x1 = gensym x in
    let term =
      let t = Var x1 |> f |> reify |> get_term in
      Abs (x1, t)
    in
    Foo (memothunk term, f)

(* The functions of eval *)

let eval t =
  let env = Hashtbl.create 16 in
  let t' = interp env t |> reify |> get_term in
  (t', env)

let eval_with_env t env =
  let t' = interp env t |> reify |> get_term in
  (t', env)
