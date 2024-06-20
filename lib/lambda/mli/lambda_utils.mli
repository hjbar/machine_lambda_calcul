open Lambda_def
open Lambda_enum

val get_gensym : kind:kind_gensym -> (unit -> string) * (unit -> unit)

val alpha_equiv : lambda_term -> lambda_term -> bool

val beta_reduce_weak_cbn : ?max_recur:int -> lambda_term -> lambda_term

val beta_reduce_strong_cbn : ?max_recur:int -> lambda_term -> lambda_term

val beta_reduce_weak_cbv : ?max_recur:int -> lambda_term -> lambda_term

val beta_reduce_strong_cbv : ?max_recur:int -> lambda_term -> lambda_term

val beta_reduce_weak_cbnd : ?max_recur:int -> lambda_term -> lambda_term

val beta_reduce_strong_cbnd : ?max_recur:int -> lambda_term -> lambda_term
