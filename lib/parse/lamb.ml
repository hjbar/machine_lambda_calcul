type expr =
  | Var of int
  | App of expr * expr
  | Abs of expr

type program = expr list
