include Writing
open Verify
open Printing

(* General test all*)

let test_gen_all ~test_ref ~test_interp ~naif_interp ~cps_interp ~defunc_interp ~version_name =
  let version_name = String.capitalize_ascii version_name in

  println_flush @@ version_name ^ " tests :";
  println_newline ();

  let err1 = test_ref naif_interp "naif" in
  let err2 = test_ref cps_interp "cps" in
  let err3 = test_ref defunc_interp "defunc" in

  let err4 = test_interp naif_interp cps_interp "naif" "cps" in
  let err5 = test_interp naif_interp defunc_interp "naif" "defunc" in
  let err6 = test_interp cps_interp defunc_interp "cps" "defunc" in

  print_newline ();
  if err1 || err2 || err3 || err4 || err5 || err6 then
    println_error @@ version_name ^ " : ERROR"
  else println_ok @@ version_name ^ " : OK"

(* Weak test all *)

let test_weak_cbn_all =
  test_gen_all ~test_ref:test_random_weak_cbn_with_reference ~test_interp:test_random_weak

let test_weak_cbnd_all =
  test_gen_all ~test_ref:test_random_weak_cbnd_with_reference ~test_interp:test_random_weak

let test_weak_cbv_all =
  test_gen_all ~test_ref:test_random_weak_cbv_with_reference ~test_interp:test_random_weak

(* Strong test all *)

let test_strong_cbv_all =
  test_gen_all ~test_ref:test_random_strong_cbv_with_reference ~test_interp:test_random_strong

let test_strong_cbnd_all =
  test_gen_all ~test_ref:test_random_strong_cbnd_with_reference ~test_interp:test_random_strong
