(* Define the type of lambda_term *)

type lambda_term =
  | Var of string
  | App of lambda_term * lambda_term
  | Abs of string * lambda_term

(* Define the type of the env and some fucntions *)

module StringMap = Map.Make (String)

type env = Env of (lambda_term * env) StringMap.t

let get_env e = match e with Env e -> e

let set_env e = Env e

(* Some functions of printing *)

let pp_lambda e =
  let rec loop fmt e =
    match e with
    | Var x -> Format.fprintf fmt "%s" x
    | App (e1, e2) -> (
      match (e1, e2) with
      | Var _, App _ | App _, Var _ | App _, App _ | Var _, Var _ ->
        Format.fprintf fmt "%a %a" loop e1 loop e2
      | App _, Abs _ | Var _, Abs _ ->
        Format.fprintf fmt "%a (%a)" loop e1 loop e2
      | _, _ -> Format.fprintf fmt "(%a) (%a)" loop e1 loop e2 )
    | Abs (s, e) -> Format.fprintf fmt "λ %s. %a" s loop e
  in
  loop Format.std_formatter e

let pp_result t res =
  pp_lambda t;
  Format.printf " -> ";
  pp_lambda res
