open Lambda

let pritnln_flush = Format.printf "%s\n%!"

let newline_flush () = Format.printf "\n%!"

let () = newline_flush ()

(* Test pour naif_interp *)

open Naif_interp

let () = pritnln_flush "Naif_interp :"

let () =
  let t = Abs ("x", Var "x") in
  let res = eval t |> fst in
  if t <> res then failwith "Error with test1 in strong_cbv_naif"
  else pp_result t res;
  newline_flush ()

let () =
  let omega =
    let delta = Abs ("x", App (Var "x", Var "x")) in
    App (delta, delta)
  in
  eval omega |> fst |> pp_lambda;
  newline_flush ();
  failwith "Error with test2 in strong_cbv_naif : omega's reduction is finite"

let () = pritnln_flush "strong_cbv_naif : OK"

let () = newline_flush ()
