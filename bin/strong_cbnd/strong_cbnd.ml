open Lambda

let test () =
  (* Début test *)
  let strat = "strong_cbnd" in
  let () = println_flush "Strong_cbnd tests :" in
  let () = print_newline () in

  (* Test pour naif_interp *)
  let open Naif_interp in
  let () = test_strong eval strat "naif" false in

  (* Test pour cps_interp *)
  let open Cps_interp in
  let () = test_strong eval strat "cps" true in

  (* Test pour defunc_interp *)
  let open Defunc_interp in
  let () = test_strong eval strat "defunc" true in

  (* Fin test *)
  let () = println_flush "Strong_cbnd tests: OK" in
  let () = print_newline () in
  let () = print_newline () in
  ()
