open Lambda

let test () =
  (* Début test *)
  let strat = "weak_cbnd" in
  let () = println_flush "Weak_cbnd tests :" in
  let () = print_newline () in

  (* Test pour naif_interp *)
  let open Weak_cbnd_naif_interp in
  let () = test_weak eval strat "naif" false in

  (* Test pour cps_interp *)
  let open Weak_cbnd_cps_interp in
  let () = test_weak eval strat "cps" false in

  (* Test pour defunc_interp *)
  let open Weak_cbnd_defunc_interp in
  let () = test_weak eval strat "defunc" false in

  (* Fin test *)
  let () = println_flush "Weak_cbnd tests: OK" in
  let () = print_newline () in
  let () = print_newline () in
  ()
