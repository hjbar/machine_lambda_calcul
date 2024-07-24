open Lambda

(* Functions for testing weak evaluator *)

val test_weak_cbn_with_ref : (lambda_term -> lambda_term) -> string -> bool

val test_weak_cbv_with_ref : (lambda_term -> lambda_term) -> string -> bool

val test_weak_cbnd_with_ref : (lambda_term -> lambda_term) -> string -> bool

val test_weak_gen :
  (lambda_term -> lambda_term) -> (lambda_term -> lambda_term) -> string -> string -> bool

(* Functions for testing opti weak evaluator *)

val test_opti_weak_cbn :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> bool

val test_opti_weak_cbv :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> bool

val test_opti_weak_cbnd :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> bool

(* Functions for testing strong evaluator *)

val test_strong_cbn_with_ref : (lambda_term -> lambda_term) -> string -> bool

val test_strong_cbv_with_ref : (lambda_term -> lambda_term) -> string -> bool

val test_strong_cbnd_with_ref : (lambda_term -> lambda_term) -> string -> bool

val test_strong_gen :
  (lambda_term -> lambda_term) -> (lambda_term -> lambda_term) -> string -> string -> bool

(* Functions for testing opti strong evaluator *)

val test_opti_strong_cbn :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> bool

val test_opti_strong_cbv :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> bool

val test_opti_strong_cbnd :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> bool
