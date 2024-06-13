open Lambda

let test () =
  let error = ref false in

  let () =
    try
      (* Début test *)
      let strat = "strong_cbv_min" in
      println_flush "Strong_cbv_min tests :";
      print_newline ();

      (* Test pour naif_interp *)
      test_strong Naif_interp.eval strat "naif" false;

      (* Test pour cps_interp *)
      test_strong Cps_v1_interp.eval strat "cps_v1" false;
      test_strong Cps_v2_interp.eval strat "cps_v2" false;

      (* Test pour defunc_interp *)
      test_strong Defunc_v1_interp.eval strat "defunc_v1" true;
      test_strong Defunc_v2_interp.eval strat "defunc_v2" true;

      (* Fin test *)
      println_flush "Strong_cbv_min tests : OK";
      print_newline ()
    with err ->
      begin
        println_flush @@ Printexc.to_string err;
        print_newline ();

        println_flush "Strong_cbv_min tests : ERROR";
        print_newline ();
        error := true
      end
  in

  let () =
    try
      println_flush "Strong_cbv_min Random tests :";
      print_newline ();

      test_random_strong_cbv_with_reference Naif_interp.eval "naif";
      test_random_strong_cbv_with_reference Cps_v1_interp.eval "cps_v1";
      test_random_strong_cbv_with_reference Cps_v2_interp.eval "cps_v2";
      test_random_strong_cbv_with_reference Defunc_v1_interp.eval "defunc_v1";
      test_random_strong_cbv_with_reference Defunc_v2_interp.eval "defunc_v2";

      test_random_strong Naif_interp.eval Cps_v1_interp.eval "naif" "cps_v1";
      test_random_strong Naif_interp.eval Cps_v2_interp.eval "naif" "cps_v2";

      test_random_strong Naif_interp.eval Defunc_v1_interp.eval "naif"
        "defunc_v1";
      test_random_strong Naif_interp.eval Defunc_v2_interp.eval "naif"
        "defunc_v2";

      test_random_strong Cps_v1_interp.eval Cps_v2_interp.eval "cps_v1" "cps_v2";

      test_random_strong Cps_v1_interp.eval Defunc_v1_interp.eval "cps_v1"
        "defunc_v1";
      test_random_strong Cps_v1_interp.eval Defunc_v2_interp.eval "cps_v1"
        "defunc_v2";
      test_random_strong Cps_v2_interp.eval Defunc_v1_interp.eval "cps_v2"
        "defunc_v1";
      test_random_strong Cps_v2_interp.eval Defunc_v2_interp.eval "cps_v2"
        "defunc_v2";

      test_random_strong Defunc_v1_interp.eval Defunc_v2_interp.eval "defunc_v1"
        "defunc_v2";

      println_flush "Strong_cbv_min Random tests : OK";
      print_newline ()
    with err ->
      begin
        println_flush @@ Printexc.to_string err;
        print_newline ();

        println_flush "Strong_cbv_min Random tests : ERROR";
        print_newline ();
        error := true
      end
  in

  if !error then println_flush "Strong_cbv_min : ERROR"
  else println_flush "Strong_cbv_min : OK"
