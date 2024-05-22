open Lambda

type value =
  | Term of lambda_term
  | Foo of state ref * (value -> lambda_term)

and state =
  | Todo of (unit -> lambda_term)
  | Done of lambda_term

type env = (string, state ref) Hashtbl.t

val eval : lambda_term -> lambda_term * env

val eval_with_env :  lambda_term -> env -> lambda_term * env
