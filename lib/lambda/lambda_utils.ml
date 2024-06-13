open Lambda_def

(* Alpha equivalence *)

let alpha_equiv e1 e2 =
  let module Env = Map.Make (String) in
  let uid =
    let cpt = ref ~-1 in
    fun () ->
      incr cpt;
      !cpt
  in

  let rec loop e1 e2 m1 m2 k =
    match (e1, e2) with
    | Var x1, Var x2 -> begin
      let b =
        match (Env.find_opt x1 m1, Env.find_opt x2 m2) with
        | Some n1, Some n2 -> n1 = n2
        | _ -> x1 = x2
      in
      k b
    end
    | App (e1, e2), App (e1', e2') ->
      loop e1 e1' m1 m2 @@ fun b -> b && loop e2 e2' m1 m2 k
    | Abs (s1, e1), Abs (s2, e2) ->
      let id = uid () in
      loop e1 e2 (Env.add s1 id m1) (Env.add s2 id m2) k
    | _ -> k false
  in

  loop e1 e2 Env.empty Env.empty Fun.id

(* Beta reduction *)

let scope_analysis t =
  let module Env = Map.Make (String) in
  let gensym =
    let cpt = ref ~-1 in
    fun () ->
      incr cpt;
      Format.sprintf "x%d" !cpt
  in
  let rec loop t env k =
    match t with
    | Var x ->
      let x' = Option.value ~default:x (Env.find_opt x env) in
      k @@ Var x'
    | App (t1, t2) ->
      loop t1 env @@ fun t1' ->
      loop t2 env @@ fun t2' -> k @@ App (t1', t2')
    | Abs (x, t) ->
      let x' = gensym () in
      loop t (Env.add x x' env) @@ fun t' -> k @@ Abs (x', t')
  in
  loop t Env.empty Fun.id

let subst t1 s t2 =
  let module Env = Set.Make (String) in
  let rec loop t env k =
    match t with
    | Var x ->
      k @@ if x = s && Option.is_none (Env.find_opt x env) then t2 else t
    | App (t1, t2) ->
      loop t1 env @@ fun t1' ->
      loop t2 env @@ fun t2' -> k @@ App (t1', t2')
    | Abs (x, _) when x = s -> k t
    | Abs (x, t1) -> loop t1 (Env.add x env) @@ fun t1' -> k @@ Abs (x, t1')
  in
  loop t1 Env.empty Fun.id

let beta_reduce_general ?(max_recur = max_int) ~full (t : lambda_term) :
  lambda_term =
  let rec loop t step k =
    if step < 0 then failwith "max step";

    match t with
    | Var _ -> k t
    | App (t1, t2) -> begin
      loop t1 (step - 1) @@ fun t1' ->
      loop t2 (step - 1) @@ fun t2' ->
      match (t1', t2') with
      | Abs (s, t1''), t2'' -> loop (subst t1'' s t2'') (step - 1) k
      | _ -> k @@ App (t1', t2')
    end
    | Abs (x, t1) -> begin
      match full with
      | false -> k t
      | true -> loop t1 (step - 1) @@ fun t1' -> k @@ Abs (x, t1')
    end
  in

  loop (scope_analysis t) max_recur Fun.id

let beta_reduce_weak = beta_reduce_general ~full:false

let beta_reduce_strong = beta_reduce_general ~full:true
