open Lambda

let strat = "strong_cbnd"

let () = print_newline ()

(* Test pour naif_interp *)

open Naif_interp

let () = test_strong eval strat "naif" false

(* Test pour cps_interp *)

open Cps_interp

let () = test_strong eval strat "cps" true

(* Test pour defunc_interp *)

open Defunc_interp

let () = test_strong eval strat "defunc" true
