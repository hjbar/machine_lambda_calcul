open Lambda

let test () =
  (* Début test *)
  let strat = "strong_cbv_min" in
  let () = println_flush "Strong_cbv_min tests :" in
  let () = print_newline () in

  (* Test pour naif_interp *)
  let open Naif_interp in
  let () = test_strong eval strat "naif" true in

  (* Test pour cps_interp *)
  let open Cps_interp in
  let () = test_strong eval strat "cps" true in

  (* Test pour defunc_interp *)
  let open Defunc_interp in
  let () = test_strong eval strat "defunc" true in

  (* Fin test *)
  let () = println_flush "Strong_cbv_min tests: OK" in
  let () = print_newline () in
  let () = print_newline () in
  ()
