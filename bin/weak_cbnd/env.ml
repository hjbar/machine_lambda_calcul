open Lambda
module Env = Map.Make (String)

type closure = lambda_term * env

and stoval =
  | Delayed of closure
  | Computed of closure

and env = stoval ref Env.t

let empty = Env.empty

let add = Env.add

let find = Env.find

let find_opt = Env.find_opt

let replace t (e : env) =
  let unwrap content =
    match !content with Delayed (t, e) | Computed (t, e) -> (t, e)
  in
  let rec loop t e =
    match t with
    | Var x -> begin
      match find_opt x e with
      | None -> Var x
      | Some r ->
        let t', e' = unwrap r in
        loop t' e'
    end
    | App (t1, t2) -> App (loop t1 e, loop t2 e)
    | Abs (x, t) -> Abs (x, loop t e)
  in
  loop t e
