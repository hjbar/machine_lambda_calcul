(* Define the type of lambda_term *)

type lambda_term =
  | Var of string
  | App of lambda_term * lambda_term
  | Abs of string * lambda_term

(* Some functions of printing *)

let new_line () = Format.printf "\n%!"

let pp_lambda e =
  let rec loop fmt e =
    match e with
    | Var x -> Format.fprintf fmt "%s" x
    | Abs (s, e) -> Format.fprintf fmt "λ %s. %a" s loop e
    | App (e1, e2) -> begin
      match (e1, e2) with
      | Abs _, Abs _ | App _, App _ ->
        Format.fprintf fmt "(%a) (%a)" loop e1 loop e2
      | _, Var _ | Var _, _ -> Format.fprintf fmt "%a %a" loop e1 loop e2
      | _ -> Format.fprintf fmt "%a (%a)" loop e1 loop e2
    end
  in
  loop Format.std_formatter e

let pp_result t res =
  pp_lambda t;
  Format.printf " -> ";
  pp_lambda res;
  new_line ()
