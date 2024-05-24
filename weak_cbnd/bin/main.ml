open Lambda

let () = print_newline ()

(* Test pour naif_interp *)

open Naif_interp

let () = test_weak eval "weak_cbnd" "naif" false

(* Test pour cps_interp *)

open Cps_interp

let () = test_weak eval "weak_cbnd" "cps" false

(* Test pour defunc_interp *)

open Defunc_interp

let () = test_weak eval "weak_cbnd" "defunc" true
