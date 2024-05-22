open Lambda

let pritnln_flush = Format.printf "%s\n%!"

let newline_flush () = Format.printf "\n%!"

let () = newline_flush ()

(* Test pour naif_interp *)

open Naif_interp

let () = pritnln_flush "Naif_interp :"

let () =
  let t = Abs ("x", Var "x") in
  let res = eval t in
  if not @@ alpha_equiv t res then begin
    pp_result t res;
    failwith "Error with test1 in strong_cbv_naif"
  end
  else pp_result t res

(*
let () =
  let omega =
    let delta = Abs ("x", App (Var "x", Var "x")) in
    App (delta, delta)
  in
  eval omega |> pp_lambda;
  newline_flush ();
  failwith "Error with test2 in strong_cbv_naif : omega's reduction is finite"
*)

let church_int n =
  let rec loop = function
    | 0 -> Var "x"
    | n -> App (Var "f", loop @@ (n - 1))
  in
  Abs ("f", Abs ("x", loop n))

let church_add n1 n2 =
  let add =
    Abs
      ( "m"
      , Abs
          ( "n"
          , Abs
              ( "f"
              , Abs
                  ( "x"
                  , App
                      ( App (Var "m", Var "f")
                      , App (App (Var "n", Var "f"), Var "x") ) ) ) ) )
  in
  App (App (add, n1), n2)

let () =
  let deux = church_int 2 in
  let trois = church_int 3 in
  let cinq = church_int 5 in
  let t = church_add deux trois in
  let res = eval t in
  if not @@ alpha_equiv cinq res then begin
    pp_result t res;
    failwith "Error with test3 in strong_cbv_naif"
  end
  else pp_result t res

let () = pritnln_flush "strong_cbv_naif : OK"

let () = newline_flush ()
