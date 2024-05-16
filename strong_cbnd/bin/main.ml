open Lambda

let () = Format.print_newline ()

(* Test pour naif_interp *)

open Naif_interp

let () = Format.printf "Naif_interp :\n%!"

let () =
  let t = Abs ("x", Var "x") in
  let res = eval t |> fst in
  if not @@ alpha_equiv t res then begin
    pp_lambda res;
    failwith "Error with test1 in strong_cbnd_naif"
  end
  else pp_result t res;
  Format.print_newline ()

let () =
  let f = Abs ("x", Var "x") in
  let y = App (f, Var "y") in
  let calc = eval y |> fst in
  let res = Var "y" in
  if not @@ alpha_equiv calc res then begin
    pp_lambda calc;
    failwith "Error with test2 in strong_cbnd_naif"
  end
  else pp_result y res;
  Format.print_newline ()

(*
let () =
  let omega =
    let delta = Abs ("x", App (Var "x", Var "x")) in
    App (delta, delta)
  in
  eval omega |> fst |> pp_lambda;
  Format.print_newline ()
*)

let () = Format.printf "weak_cbnd_naif : OK\n%!"

let () = Format.print_newline ()
