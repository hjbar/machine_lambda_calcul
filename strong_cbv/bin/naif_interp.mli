(* Definitions of types *)

open Lambda

type env = (string, lambda_term) Hashtbl.t

(* Functions of interp *)

val eval : lambda_term -> lambda_term * env

val eval_with_env :  lambda_term -> env -> lambda_term * env
