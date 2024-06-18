open Lambda

(* Definitions of types *)

type extended_terms =
  | Var of string
  | App of extended_terms * extended_terms
  | Abs of string * extended_terms
  | Ext of value list

and value =
  | Cst of string
  | Lam of string * extended_terms
  | Lst of value list

(* Conversion of types *)

let rec extended_to_term : extended_terms -> lambda_term = function
  | Var x -> Var x
  | App (t1, t2) -> App (extended_to_term t1, extended_to_term t2)
  | Abs (x, t) -> Abs (x, extended_to_term t)
  | Ext _ -> assert false

let rec term_to_extended : lambda_term -> extended_terms = function
  | Var x -> Var x
  | App (t1, t2) -> App (term_to_extended t1, term_to_extended t2)
  | Abs (x, t) -> Abs (x, term_to_extended t)
