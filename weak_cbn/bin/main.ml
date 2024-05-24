open Lambda

let () = Format.print_newline ()

(* Test pour naif_interp *)

open Naif_interp

let () = test_weak eval "weak_cbn" "naif" false

(* Test pour cps_interp *)

open Cps_interp

let () = test_weak eval "weak_cbn" "cps" false

(* Test pour defunc_interp *)

open Defunc_interp

let () = test_weak eval "weak_cbn" "defunc" false
