open Lambda

let test () =
  let strat = "weak_cbn" in
  let () = Format.print_newline () in

  (* Test pour naif_interp *)
  let open Weak_cbn_naif_interp in
  let () = test_weak eval strat "naif" false in

  (* Test pour cps_interp *)
  let open Weak_cbn_cps_interp in
  let () = test_weak eval strat "cps" false in

  (* Test pour defunc_interp *)
  let open Weak_cbn_defunc_interp in
  let () = test_weak eval strat "defunc" false in

  ()
