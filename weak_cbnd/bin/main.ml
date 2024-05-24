open Lambda

let () = new_line ()

(* Test pour naif_interp *)

open Naif_interp

let () = Format.printf "Naif_interp :\n%!"

let () =
  let t = Abs ("x", Var "x") in
  let res = eval t in
  if t <> res then begin
    pp_result t res;
    failwith "Error with test1 in weak_cbnd_naif"
  end
  else pp_result t res

(*
let () =
  let omega =
    let delta = Abs ("x", App (Var "x", Var "x")) in
    App (delta, delta)
  in
  eval omega |> pp_result omega
*)

let () = Format.printf "weak_cbnd_naif : OK\n%!"

let () = new_line ()

(* Test pour cps_interp *)

open Cps_interp

let () = Format.printf "Cps_interp :\n%!"

let () =
  let t = Abs ("x", Var "x") in
  let res = eval t in
  if t <> res then begin
    pp_result t res;
    failwith "Error with test1 in weak_cbnd_cps"
  end
  else pp_result t res

(*
let () =
  let omega =
    let delta = Abs ("x", App (Var "x", Var "x")) in
    App (delta, delta)
  in
  eval omega |> pp_result omega
*)

let () = Format.printf "weak_cbnd_cps : OK\n%!"

let () = new_line ()

(* Test pour defunc_interp *)

open Defunc_interp

let () = Format.printf "Defunc_interp :\n%!"

let () =
  let t = Abs ("x", Var "x") in
  let res = eval t in
  if t <> res then begin
    pp_result t res;
    failwith "Error with test1 in weak_cbnd_defunc"
  end
  else pp_result t res

let () =
  let omega =
    let delta = Abs ("x", App (Var "x", Var "x")) in
    App (delta, delta)
  in
  eval omega |> pp_result omega

let () = Format.printf "weak_cbnd_defunc : OK\n%!"

let () = new_line ()
