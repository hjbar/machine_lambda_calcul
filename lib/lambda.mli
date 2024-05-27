(* Define the type of lambda_terms *)

type lambda_term =
  | Var of string
  | App of lambda_term * lambda_term
  | Abs of string * lambda_term

(* Some functions on lambda_terms *)

val alpha_equiv : lambda_term -> lambda_term -> bool

(* Some functions of printing *)

val print_newline : unit -> unit

val print_flush : string -> unit

val println_flush : string -> unit

val pp_lambda : lambda_term -> unit

val pp_result : lambda_term -> lambda_term -> unit

(* Some functions of testing *)

val test_eval : lambda_term -> lambda_term -> (lambda_term -> lambda_term) -> string -> unit

val test_weak : (lambda_term -> lambda_term) -> string -> string -> bool -> unit

val test_strong : (lambda_term -> lambda_term) -> string -> string -> bool -> unit
