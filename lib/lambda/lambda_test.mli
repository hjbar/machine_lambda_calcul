open Lambda_def

val test_eval :
  lambda_term -> lambda_term -> (lambda_term -> lambda_term) -> string -> unit

val test_weak : (lambda_term -> lambda_term) -> string -> string -> bool -> unit

val test_strong :
  (lambda_term -> lambda_term) -> string -> string -> bool -> unit
