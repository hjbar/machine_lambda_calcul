open Lambda

let test () =
  (* Début test *)
  let strat = "strong_cbnd" in
  println_flush "Strong_cbnd tests :";
  print_newline ();

  (* Test pour naif_interp *)
  let open Naif_interp in
  test_strong eval strat "naif" true;

  (* Test pour cps_interp *)
  let open Cps_interp in
  test_strong eval strat "cps" true;

  (* Test pour defunc_interp *)
  let open Defunc_interp in
  test_strong eval strat "defunc" true;

  (* Fin test *)
  println_flush "Strong_cbnd tests: OK";
  print_newline ();
  print_newline ()
