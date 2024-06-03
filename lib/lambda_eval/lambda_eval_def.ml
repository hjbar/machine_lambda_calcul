(* Define the type of lambda_terms *)

type term =
  | Value of value
  | Comp of comp

and value =
  | Var of string
  | Abs of string * term

and comp = App of term * term
