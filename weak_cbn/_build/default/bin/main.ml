open Lambda
open Naif_interp

let t1 = Abs ("x", Var "x")

let omega =
  let delta = Abs ("x", App (Var "x", Var "x")) in
  App (delta, delta)

let () =
  if fst @@ eval t1 <> t1 then failwith "Error with t1 in weak_cbn";

  fst @@ eval omega |> pp_lambda;

  Printf.printf "weak_cbn : OK\n%!"
