open Lambda

(* Functions of interp *)

let eval (t : lambda_term) : lambda_term =
  ignore @@ failwith "DEFUNC TODO";
  pp_lambda t;
  t
