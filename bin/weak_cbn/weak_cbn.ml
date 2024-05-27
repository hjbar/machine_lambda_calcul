open Lambda

let test () =
  (* Début test *)
  let strat = "weak_cbn" in
  let () = println_flush "Weak_cbn tests :" in
  let () = print_newline () in

  (* Test pour naif_interp *)
  let open Naif_interp in
  let () = test_weak eval strat "naif" false in

  (* Test pour cps_interp *)
  let open Cps_interp in
  let () = test_weak eval strat "cps" false in

  (* Test pour defunc_interp *)
  let open Defunc_interp in
  let () = test_weak eval strat "defunc" false in

  (* Fin test *)
  let () = println_flush "Weak_cbn tests: OK" in
  let () = print_newline () in
  let () = print_newline () in
  ()
