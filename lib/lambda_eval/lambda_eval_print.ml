open Lambda_eval_def

(* Some functions of printing *)

let print_newline () = Format.printf "\n%!"

let print_flush = Format.printf "%s%!"

let println_flush = Format.printf "%s\n%!"

let pp_lambda t =
  let rec loop fmt = function
    | Value (Var x) -> Format.fprintf fmt "%s" x
    | Value (Abs (s, t)) -> Format.fprintf fmt "λ %s. %a" s loop t
    | Comp (App (t1, t2)) -> begin
      match (t1, t2) with
      | Value (Abs _), Value (Abs _) | Comp (App _), Comp (App _) ->
        Format.fprintf fmt "(%a) (%a)" loop t1 loop t2
      | _, Value (Var _) | Value (Var _), _ ->
        Format.fprintf fmt "%a %a" loop t1 loop t2
      | _ -> Format.fprintf fmt "%a (%a)" loop t1 loop t2
    end
  in
  loop Format.std_formatter t

let pp_result t res =
  pp_lambda t;
  print_flush " -> ";
  pp_lambda res;
  print_newline ()
