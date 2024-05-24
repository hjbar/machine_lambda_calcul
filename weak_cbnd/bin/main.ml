open Lambda

let strat = "weak_cbnd"

let () = print_newline ()

(* Test pour naif_interp *)

open Naif_interp

let () = test_weak eval strat "naif" false

(* Test pour cps_interp *)

open Cps_interp

let () = test_weak eval strat "cps" false

(* Test pour defunc_interp *)

open Defunc_interp

let () = test_weak eval strat "defunc" false
