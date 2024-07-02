open Lambda
open Lambda_ext_cbnd

let eval (t : lambda_term) : lambda_term =
  ignore @@ failwith "DEFUNC TODO";
  t |> term_to_extended |> extended_to_term
