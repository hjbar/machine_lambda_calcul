type extended_terms =
  | Var of string
  | App of extended_terms * extended_terms
  | Abs of string * extended_terms
  | Ext of value list

and value =
  | Cst of string
  | Lam of string * extended_terms
  | Lst of value list
