open Lambda

let test () =
  (* Début test *)
  let strat = "strong_cbv_min" in
  println_flush "Strong_cbv_min tests :";
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
  println_flush "Strong_cbv_min tests: OK";
  print_newline ();
  print_newline ()
