open Lambda_def

(* Functions for testing weak evaluator *)

val test_random_weak_cbn_with_reference :
  (lambda_term -> lambda_term) -> string -> unit

val test_random_weak_cbv_with_reference :
  (lambda_term -> lambda_term) -> string -> unit

val test_random_weak_cbnd_with_reference :
  (lambda_term -> lambda_term) -> string -> unit

val test_random_weak :
     (lambda_term -> lambda_term)
  -> (lambda_term -> lambda_term)
  -> string
  -> string
  -> unit

(* Functions for testing strong evaluator *)

val test_random_strong_cbn_with_reference :
  (lambda_term -> lambda_term) -> string -> unit

val test_random_strong_cbv_with_reference :
  (lambda_term -> lambda_term) -> string -> unit

val test_random_strong_cbnd_with_reference :
  (lambda_term -> lambda_term) -> string -> unit

val test_random_strong :
     (lambda_term -> lambda_term)
  -> (lambda_term -> lambda_term)
  -> string
  -> string
  -> unit
