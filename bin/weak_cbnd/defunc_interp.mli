open Lambda
open Env

val eval : lambda_term -> lambda_term

val eval_with_env : lambda_term -> closure
