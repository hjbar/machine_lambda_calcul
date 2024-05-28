open Lambda

let test () =
  (* Début test *)
  let strat = "strong_cbv" in
  println_flush "Strong_cbv tests :";
  print_newline ();

  (* Test pour naif_interp *)
  let open Naif_interp in
  test_strong eval strat "naif" false;

  (* Test pour cps_interp *)
  let open Cps_interp in
  test_strong eval strat "cps" false;

  (* Test pour defunc_interp *)
  let open Defunc_interp in
  test_strong eval strat "defunc" true;

  (* Fin test *)
  println_flush "Strong_cbv tests: OK";
  print_newline ();
  print_newline ()
