open Lambda_ext

(* The function of strong cbv eval *)

let eval (t : lambda_term) : lambda_term =
  ignore @@ failwith "DEFUNC V1 TODO";
  pp_lambda t;
  t
