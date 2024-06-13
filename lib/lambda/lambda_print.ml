open Lambda_def

(* Some functions of printing *)

let print_newline () = Format.printf "\n%!"

let print_flush = Format.printf "%s%!"

let println_flush = Format.printf "%s\n%!"

let pp_lambda e =
  let rec loop fmt e =
    match e with
    | Var x -> Format.fprintf fmt "%s" x
    | Abs (s, e) -> Format.fprintf fmt "λ %s. %a" s loop e
    | App (e1, e2) -> begin
      match (e1, e2) with
      | _, Var _ | Var _, _ -> Format.fprintf fmt "%a %a" loop e1 loop e2
      | _ -> Format.fprintf fmt "(%a) (%a)" loop e1 loop e2
    end
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
