open Lambda

let () = print_newline ()

(* Test pour naif_interp *)

open Naif_interp

let () = test_strong eval "strong_cbnd" "naif" false

(* Test pour cps_interp *)

open Cps_interp

let () = test_strong eval "strong_cbnd" "cps" true

(* Test pour defunc_interp *)

open Defunc_interp

let () = test_strong eval "strong_cbnd" "defunc" true
