module StringMap = Map.Make (String)

type env = stoval ref StringMap.t

and stoval =
  | Delayed of extended_closure
  | Computed of value_closure

and extended_closure = extended_term * env

and value_closure = value * env

and extended_term =
  | Var of string
  | App of extended_term * extended_term
  | Abs of string * extended_term
  | Ext of string * value_closure list

and value =
  | Cst of string
  | Lam of string * extended_term
  | Lst of string * value_closure list
