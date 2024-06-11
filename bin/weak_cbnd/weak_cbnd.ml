open Lambda

let test () =
  let error = ref false in

  let () =
    try
      (* Début test *)
      let strat = "weak_cbnd" in
      println_flush "Weak_cbnd tests :";
      print_newline ();

      (* Test pour naif_interp *)
      test_weak Naif_interp.eval strat "naif" false;

      (* Test pour cps_interp *)
      test_weak Cps_interp.eval strat "cps" false;

      (* Test pour defunc_interp *)
      test_weak Defunc_interp.eval strat "defunc" false;

      (* Fin test *)
      println_flush "Weak_cbnd tests : OK";
      print_newline ()
    with err ->
      begin
        println_flush @@ Printexc.to_string err;
        print_newline ();

        println_flush "Weak_cbnd tests : ERROR";
        print_newline ();
        error := true
      end
  in

  let () =
    try
      println_flush "Weak_cbnd Random tests :";

      (* Test cps & defunc avec de l'aléatoire *)
      test_random_naif Naif_interp.eval Cps_interp.eval "naif" "cps";
      test_random_naif Naif_interp.eval Defunc_interp.eval "naif" "defunc";
      test_random_naif Cps_interp.eval Defunc_interp.eval "cps" "defunc";

      (* Fin test *)
      println_flush "Weak_cbnd Random tests : OK";
      print_newline ()
    with err ->
      begin
        println_flush @@ Printexc.to_string err;
        print_newline ();

        println_flush "Weak_cbnd Random tests : ERROR";
        print_newline ();
        error := true
      end
  in

  if !error then println_flush "Weak_cbnd : ERROR"
  else println_flush "Weak_cbnd : OK"
