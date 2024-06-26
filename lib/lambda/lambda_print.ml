open Lambda_def
open Printing

let pp_lambda e =
  let rec loop fmt e =
    match e with
    | Var x -> Format.fprintf fmt "%s" x
    | Abs (s, e) -> Format.fprintf fmt "λ %s. %a" s loop e
    | App (e1, e2) -> Format.fprintf fmt "(%a) (%a)" loop e1 loop e2
  in
  loop Format.std_formatter e

let pp_struct e =
  let rec loop fmt e =
    match e with
    | Var x -> Format.fprintf fmt "Var %s" x
    | Abs (s, e) -> Format.fprintf fmt "Abs (%s, %a)" s loop e
    | App (e1, e2) -> Format.fprintf fmt "App (%a, %a)" loop e1 loop e2
  in
  loop Format.std_formatter e

let pp_result t res =
  pp_lambda t;
  print_flush " -> ";
  pp_lambda res;
  print_newline ()
