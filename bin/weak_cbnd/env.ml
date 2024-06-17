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

let pp_env e =
  let pp_sep fmt () = Format.fprintf fmt ", " in

  let pp_val fmt (x, sto) ~f =
    let label, t, e =
      match !sto with
      | Delayed (t, e) -> ("Delayed", t, e)
      | Computed (t, e) -> ("Computed", t, e)
    in
    Format.fprintf fmt "%s |--> %s: " x label;
    pp_lambda t;
    Format.fprintf fmt ", %a" f e
  in

  let rec loop fmt e =
    Format.fprintf fmt "{";
    let l = e |> Env.to_seq |> List.of_seq in
    Format.pp_print_list ~pp_sep (pp_val ~f:loop) fmt l;
    Format.fprintf fmt "}"
  in

  loop Format.std_formatter e

let replace t e =
  let unwrap content =
    match !content with Delayed (t, e) | Computed (t, e) -> (t, e)
  in
  let rec loop t e k =
    match t with
    | Var x -> begin
      match find_opt x e with
      | None -> k @@ Var x
      | Some r ->
        let t', e' = unwrap r in
        loop t' e' k
    end
    | App (t1, t2) ->
      loop t1 e @@ fun t1' ->
      loop t2 e @@ fun t2' -> k @@ App (t1', t2')
    | Abs (x, t1) -> loop t1 e @@ fun t1' -> k @@ Abs (x, t1')
  in
  loop t e Fun.id
