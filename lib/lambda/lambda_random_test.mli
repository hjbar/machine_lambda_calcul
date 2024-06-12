open Lambda_def

val test_random_weak_with_reference :
  (lambda_term -> lambda_term) -> string -> unit

val test_random_weak :
     (lambda_term -> lambda_term)
  -> (lambda_term -> lambda_term)
  -> string
  -> string
  -> unit

val test_random_strong_with_reference :
  (lambda_term -> lambda_term) -> string -> unit

val test_random_strong :
     (lambda_term -> lambda_term)
  -> (lambda_term -> lambda_term)
  -> string
  -> string
  -> unit
