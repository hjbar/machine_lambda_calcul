open Lambda

let test () =
  try
    (* Début test *)
    let strat = "strong_cbv" in
    println_flush "Strong_cbv tests :";
    print_newline ();

    (* Test pour naif_interp *)
    let open Naif_interp in
    test_strong eval strat "naif" false;

    (* Test pour cps_interp *)
    let open Cps_interp in
    test_strong eval strat "cps_v1" false;

    (* Test pour defunc_interp *)
    let open Defunc_interp in
    test_strong eval strat "defunc_v1" true;

    (* Fin test *)
    println_flush "Strong_cbv tests: OK";
    print_newline ();
    print_newline ()
  with err ->
    begin
      println_flush @@ Printexc.to_string err;
      print_newline ();

      println_flush "Strong_cbv tests: ERROR";
      print_newline ();
      print_newline ()
    end
