(* Define the type of lambda_term *)

type lambda_term =
  | Var of string
  | App of lambda_term * lambda_term
  | Abs of string * lambda_term

(* Some functions on lambda_terms *)

module Env = Map.Make (String)

let alpha_equiv e1 e2 =
  let uid =
    let cpt = ref 0 in
    fun () ->
      incr cpt;
      !cpt
  in

  let rec loop e1 e2 m1 m2 =
    match (e1, e2) with
    | Var x1, Var x2 -> (
      match (Env.find_opt x1 m1, Env.find_opt x2 m2) with
      | Some n1, Some n2 -> n1 = n2
      | _, _ -> x1 = x2 )
    | App (e1, e2), App (e1', e2') -> loop e1 e1' m1 m2 && loop e2 e2' m1 m2
    | Abs (s1, e1), Abs (s2, e2) ->
      let id = uid () in
      loop e1 e2 (Env.add s1 id m1) (Env.add s2 id m2)
    | _, _ -> false
  in

  loop e1 e2 Env.empty Env.empty

(* Some functions of printing *)

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
  pp_lambda res
