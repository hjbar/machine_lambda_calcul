open Lambda
open Lambda_ext_cbv_def

let rec extended_to_term : extended_term -> lambda_term = function
  | Var x -> Var x
  | App (t1, t2) -> App (extended_to_term t1, extended_to_term t2)
  | Abs (x, t) -> Abs (x, extended_to_term t)
  | Ext _ -> assert false

let rec term_to_extended : lambda_term -> extended_term = function
  | Var x -> Var x
  | App (t1, t2) -> App (term_to_extended t1, term_to_extended t2)
  | Abs (x, t) -> Abs (x, term_to_extended t)
