open Lambda

(* Definition of types *)

type value =
  | Term of lambda_term
  | Foo of state ref * (value -> lambda_term)

and state =
  | Todo of (unit -> lambda_term)
  | Done of lambda_term

type env = (string, state ref) Hashtbl.t

(* Some utils functions *)

let get_term t = match t with Term t -> t | _ -> assert false

let set_term t = Term t

let set_done v = Done v

(* Some interp functions *)

let gensym (x : string) : string = x ^ "'"

let force (ref_state : state ref) : lambda_term =
  match !ref_state with
  | Done term -> term
  | Todo todo ->
    let v = todo () in
    ref_state := Done v;
    v

let memothunk (thunk : unit -> lambda_term) : state ref = Todo thunk |> ref

let reify (value : value) : lambda_term =
  match value with Foo (c, _) -> force c | Term t -> t

let apply_value (v : value) (w : value) : lambda_term =
  match v with Foo (_, f) -> f w | Term t -> App (t, reify w)

let rec interp (e : env) (t : lambda_term) : value =
  match t with
  | Var x ->
    let res =
      match Hashtbl.find_opt e x with
      | None -> Var x |> set_done |> ref
      | Some res -> res
    in
    force res |> set_term
  | App (t, u) ->
    let i1 = interp e t in
    let i2 = interp e u in
    apply_value i1 i2 |> set_term
  (* ERROR ICI *)
  | Abs (x, t) ->
    let f (v : value) : lambda_term =
      Hashtbl.replace e x (memothunk (fun () -> v |> get_term));
      interp e t |> get_term
    in
    let x1 = gensym x in
    let term () =
      let t = f (Var x1 |> set_term) |> set_term |> reify in
      Abs (x1, t)
    in
    Foo (memothunk term, f)

(* The functions of eval *)

let eval t =
  ignore @@ failwith "CPS TODO";
  interp (Hashtbl.create 16) t |> reify
