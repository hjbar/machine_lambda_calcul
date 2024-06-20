open Lambda

let test () =
  let error = ref false in

  let () =
    try
      (* Début test *)
      let strat = "strong_cbnd" in
      println_flush "Strong_cbnd tests :";
      print_newline ();

      (* Test pour naif_interp *)
      test_strong Naif_interp.eval strat "naif" true;

      (* Test pour cps_interp *)
      test_strong Cps_interp.eval strat "cps" true;

      (* Test pour defunc_interp *)
      test_strong Defunc_interp.eval strat "defunc" true;

      (* Fin test *)
      println_ok "Strong_cbnd tests : OK";
      print_newline ()
    with err ->
      begin
        println_warning @@ Printexc.to_string err;
        print_newline ();

        println_error "Strong_cbnd tests : ERROR";
        print_newline ();
        error := true
      end
  in

  let () =
    try
      println_flush "Strong_cbnd Random tests :";
      print_newline ();

      test_random_strong_cbnd_with_reference Naif_interp.eval "naif";
      test_random_strong_cbnd_with_reference Cps_interp.eval "cps";
      test_random_strong_cbnd_with_reference Defunc_interp.eval "defunc";

      test_random_strong Naif_interp.eval Cps_interp.eval "naif" "cps";
      test_random_strong Naif_interp.eval Defunc_interp.eval "naif" "defunc";
      test_random_strong Cps_interp.eval Defunc_interp.eval "cps" "defunc";

      println_ok "Strong_cbnd Random tests : OK";
      print_newline ()
    with err ->
      begin
        println_warning @@ Printexc.to_string err;
        print_newline ();

        println_error "Strong_cbnd Random tests : ERROR";
        print_newline ();
        error := true
      end
  in

  if !error then println_error "Strong_cbnd : ERROR"
  else println_ok "Strong_cbnd : OK"
