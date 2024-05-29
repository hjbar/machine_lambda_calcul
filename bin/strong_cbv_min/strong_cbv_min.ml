open Lambda

let test () =
  try
    (* Début test *)
    let strat = "strong_cbv_min" in
    println_flush "Strong_cbv_min tests :";
    print_newline ();

    (* Test pour naif_interp *)
    let open Naif_interp in
    test_strong eval strat "naif" false;

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
  with err ->
    begin
      println_flush @@ Printexc.to_string err;
      print_newline ();

      println_flush "Strong_cbv_min tests: ERROR";
      print_newline ();
      print_newline ()
    end
