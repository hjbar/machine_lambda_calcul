open Lambda

let test () =
  try
    (* Début test *)
    let strat = "weak_cbn" in
    println_flush "Weak_cbn tests :";
    print_newline ();

    (* Test pour naif_interp *)
    let open Naif_interp in
    test_weak eval strat "naif" false;

    (* Test pour cps_interp *)
    let open Cps_interp in
    test_weak eval strat "cps" false;

    (* Test pour defunc_interp *)
    let open Defunc_interp in
    test_weak eval strat "defunc" false;

    (* Fin test *)
    println_flush "Weak_cbn tests: OK"
  with err ->
    begin
      println_flush @@ Printexc.to_string err;
      print_newline ();

      println_flush "Weak_cbn tests: ERROR"
    end
