open Lambda
open Lambda_eval_def

let alpha_equiv e1 e2 =
  let module Env = Map.Make (String) in
  let uid =
    let cpt = ref 0 in
    fun () ->
      incr cpt;
      !cpt
  in

  let rec loop e1 e2 m1 m2 k =
    match (e1, e2) with
    | Value (Var x1), Value (Var x2) -> begin
      let b =
        match (Env.find_opt x1 m1, Env.find_opt x2 m2) with
        | Some n1, Some n2 -> n1 = n2
        | _ -> x1 = x2
      in
      k b
    end
    | Comp (App (e1, e2)), Comp (App (e1', e2')) ->
      loop e1 e1' m1 m2 @@ fun b -> b && loop e2 e2' m1 m2 k
    | Value (Abs (s1, e1)), Value (Abs (s2, e2)) ->
      let id = uid () in
      loop e1 e2 (Env.add s1 id m1) (Env.add s2 id m2) k
    | _ -> k false
  in

  loop e1 e2 Env.empty Env.empty Fun.id

let rec eval_to_term : term -> lambda_term = function
  | Value (Var x) -> Var x
  | Value (Abs (x, t)) -> Abs (x, eval_to_term t)
  | Comp (App (t1, t2)) -> App (eval_to_term t1, eval_to_term t2)

let rec term_to_eval : lambda_term -> term = function
  | Var x -> Value (Var x)
  | Abs (x, t) -> Value (Abs (x, term_to_eval t))
  | App (t1, t2) -> Comp (App (term_to_eval t1, term_to_eval t2))
