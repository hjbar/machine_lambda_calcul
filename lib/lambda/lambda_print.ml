open Lambda_def

(* Codes *)

let red = "\027[31m"

let green = "\027[32m"

let blue = "\027[36m"

let reset = "\027[0m"

(* Some functions on strings *)

let red_string s = Format.sprintf "%s%s%s" red s reset

let green_string s = Format.sprintf "%s%s%s" green s reset

let blue_string s = Format.sprintf "%s%s%s" blue s reset

(* Some functions of printing *)

let print_newline () = Format.printf "\n%!"

let print_flush = Format.printf "%s%!"

let println_flush = Format.printf "%s\n%!"

let print_error s = red_string s |> print_flush

let println_error s = red_string s |> println_flush

let print_ok s = green_string s |> print_flush

let println_ok s = green_string s |> println_flush

let print_data s = blue_string s |> print_flush

let println_data s = blue_string s |> println_flush

(* Some functions of priting on lambda_terms *)

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
