open Lambda_ext
module Env = Map.Make (String)

type env = stoval ref Env.t

and stoval =
  | Delayed of extended_closure
  | Computed of extended_closure

and extended_closure = extended_terms * env

and value_closure = value * env

type cont =
  | CONT1 of stoval ref
  | CONT2 of stoval ref

let empty = Env.empty

let add = Env.add

let find = Env.find

let find_opt = Env.find_opt

let union e1 e2 = Env.union (fun _key _val1 _val2 -> assert false) e1 e2

let unwrap sto_r = match !sto_r with Delayed c | Computed c -> c

let gensym, gensym_reset = get_gensym ~kind:Interp

let pp_env e =
  let pp_sep fmt () = Format.fprintf fmt ", " in

  let pp_val fmt (x, sto) ~f =
    let label, t, e =
      match !sto with
      | Delayed (t, e) -> ("Delayed", t, e)
      | Computed (t, e) -> ("Computed", t, e)
    in
    Format.fprintf fmt "%s |--> %s: " x label;
    pp_lambda_ext t;
    Format.fprintf fmt ", %a" f e
  in

  let rec loop fmt e =
    Format.fprintf fmt "{";
    let l = e |> Env.to_seq |> List.of_seq in
    Format.pp_print_list ~pp_sep (pp_val ~f:loop) fmt l;
    Format.fprintf fmt "}"
  in

  loop Format.std_formatter e
