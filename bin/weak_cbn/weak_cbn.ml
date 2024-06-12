open Lambda

let test () =
  let error = ref false in

  let () =
    try
      (* Début test *)
      let strat = "weak_cbn" in
      println_flush "Weak_cbn tests :";
      print_newline ();

      (* Test pour naif_interp *)
      test_weak Naif_interp.eval strat "naif" false;

      (* Test pour cps_interp *)
      test_weak Cps_interp.eval strat "cps" false;

      (* Test pour defunc_interp *)
      test_weak Defunc_interp.eval strat "defunc" false;

      (* Fin test *)
      println_flush "Weak_cbn tests : OK";
      print_newline ()
    with err ->
      begin
        println_flush @@ Printexc.to_string err;
        print_newline ();

        println_flush "Weak_cbn tests : ERROR";
        print_newline ();
        error := true
      end
  in

  let () =
    try
      println_flush "Weak_cbn Random tests :";
      print_newline ();

      test_random_weak_with_reference Naif_interp.eval "naif";
      test_random_weak_with_reference Cps_interp.eval "cps";
      test_random_weak_with_reference Defunc_interp.eval "defunc";

      test_random_weak Naif_interp.eval Cps_interp.eval "naif" "cps";
      test_random_weak Naif_interp.eval Defunc_interp.eval "naif" "defunc";
      test_random_weak Cps_interp.eval Defunc_interp.eval "cps" "defunc";

      println_flush "Weak_cbn Random tests : OK";
      print_newline ()
    with err ->
      begin
        println_flush @@ Printexc.to_string err;
        print_newline ();

        println_flush "Weak_cbn Random tests : ERROR";
        print_newline ();
        error := true
      end
  in

  if !error then println_flush "Weak_cbn : ERROR"
  else println_flush "Weak_cbn : OK"
