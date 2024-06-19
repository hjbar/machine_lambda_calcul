open Lambda

let test () =
  let error = ref false in

  let () =
    try
      (* Début test *)
      let strat = "strong_cbnd_min" in
      println_flush "Strong_cbnd_min tests :";
      print_newline ();

      (* Test pour naif_interp *)
      test_strong Naif_interp.eval strat "naif" false;

      (* Test pour cps_interp *)
      test_strong Cps_interp.eval strat "cps" false;

      (* Test pour defunc_interp *)
      test_strong Defunc_interp.eval strat "defunc" false;

      (* Fin test *)
      println_flush @@ green_string "Strong_cbnd_min tests : OK";
      print_newline ()
    with err ->
      begin
        println_flush @@ Printexc.to_string err;
        print_newline ();

        println_flush @@ red_string "Strong_cbnd_min tests : ERROR";
        print_newline ();
        error := true
      end
  in

  let () =
    try
      println_flush "Strong_cbnd_min Random tests :";
      print_newline ();

      test_random_strong_cbnd_with_reference Naif_interp.eval "naif";
      test_random_strong_cbnd_with_reference Cps_interp.eval "cps";
      test_random_strong_cbnd_with_reference Defunc_interp.eval "defunc";

      test_random_strong Naif_interp.eval Cps_interp.eval "naif" "cps";
      test_random_strong Naif_interp.eval Defunc_interp.eval "naif" "defunc";
      test_random_strong Cps_interp.eval Defunc_interp.eval "cps" "defunc";

      println_flush @@ green_string "Strong_cbnd_min Random tests : OK";
      print_newline ()
    with err ->
      begin
        println_flush @@ Printexc.to_string err;
        print_newline ();

        println_flush @@ red_string "Strong_cbnd_min Random tests : ERROR";
        print_newline ();
        error := true
      end
  in

  if !error then println_flush @@ green_string "Strong_cbnd_min : ERROR"
  else println_flush @@ red_string "Strong_cbnd_min : OK"
