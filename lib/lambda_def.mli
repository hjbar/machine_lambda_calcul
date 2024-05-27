type lambda_term =
  | Var of string
  | App of lambda_term * lambda_term
  | Abs of string * lambda_term
