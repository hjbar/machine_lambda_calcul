open Lambda

let print_reduc t1 t2 =
  pp_lambda t1;
  Format.printf " -> ";
  pp_lambda t2

let () = Format.print_newline ()

(* Test pour naif_interp *)

open Naif_interp

let () = Format.printf "Naif_interp :\n%!"

let () =
  let t = Abs ("x", Var "x") in
  let res = eval t |> fst in
  if t <> res then failwith "Error with test1 in weak_cbn_naif"
  else print_reduc t res;
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

let () = Format.printf "weak_cbn_naif : OK\n%!"

let () = Format.print_newline ()

(* Test pour cps_interp *)

open Cps_interp

let () = Format.printf "Cps_interp :\n%!"

let () =
  let t = Abs ("x", Var "x") in
  let res = eval t |> fst in
  if t <> res then failwith "Error with test1 in weak_cbn_naif"
  else print_reduc t res;
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

let () = Format.printf "weak_cbn_cps : OK\n%!"

let () = Format.print_newline ()
