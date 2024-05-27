open Lambda

let test () =
  (* Début test *)
  let strat = "strong_cbv" in
  let () = println_flush "Strong_cbv tests :" in
  let () = print_newline () in

  (* Test pour naif_interp *)
  let open Strong_cbv_naif_interp in
  let () = test_strong eval strat "naif" false in

  (* Test pour cps_interp *)
  let open Strong_cbv_cps_interp in
  let () = test_strong eval strat "cps" true in

  (* Test pour defunc_interp *)
  let open Strong_cbv_defunc_interp in
  let () = test_strong eval strat "defunc" true in

  (* Fin test *)
  let () = println_flush "Strong_cbv tests: OK" in
  let () = print_newline () in
  let () = print_newline () in
  ()
