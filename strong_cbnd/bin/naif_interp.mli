open Lambda

val eval : lambda_term -> lambda_term * env

val eval_with_env :  lambda_term -> env -> lambda_term * env
