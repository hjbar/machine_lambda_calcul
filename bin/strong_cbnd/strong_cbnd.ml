open Lambda

let test () =
  let strat = "strong_cbnd" in
  let () = print_newline () in

  (* Test pour naif_interp *)
  let open Strong_cbnd_naif_interp in
  let () = test_strong eval strat "naif" false in

  (* Test pour cps_interp *)
  let open Strong_cbnd_cps_interp in
  let () = test_strong eval strat "cps" true in

  (* Test pour defunc_interp *)
  let open Strong_cbnd_defunc_interp in
  let () = test_strong eval strat "defunc" true in

  ()
