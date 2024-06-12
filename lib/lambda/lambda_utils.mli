open Lambda_def

val alpha_equiv : lambda_term -> lambda_term -> bool

val beta_reduce_weak : ?max_recur:int -> lambda_term -> lambda_term

val beta_reduce_strong : ?max_recur:int -> lambda_term -> lambda_term
