include Writing
open Verify
open Printing

(* General test all*)

let test_ref_gen ~test_ref ~naif_interp ~cps_interp ~defunc_interp ~version_name =
  let version_name = String.capitalize_ascii version_name in

  println_flush @@ version_name ^ " tests :";
  println_newline ();

  let err1 = test_ref naif_interp "naif" in
  let err2 = test_ref cps_interp "cps" in
  let err3 = test_ref defunc_interp "defunc" in

  print_newline ();
  if err1 || err2 || err3 then println_error @@ version_name ^ " : ERROR"
  else println_ok @@ version_name ^ " : OK"

let test_all_interp ~test_interp ~naif_interp ~cps_interp ~defunc_interp ~version_name =
  let version_name = String.capitalize_ascii version_name in

  println_flush @@ version_name ^ " tests :";
  println_newline ();

  let err1 = test_interp naif_interp cps_interp "naif" "cps" in
  let err2 = test_interp cps_interp defunc_interp "cps" "defunc" in

  print_newline ();
  if err1 || err2 then println_error @@ version_name ^ " : ERROR"
  else println_ok @@ version_name ^ " : OK"

let test_all_gen ~test_ref ~test_interp ~naif_interp ~cps_interp ~defunc_interp ~version_name =
  let version_name = String.capitalize_ascii version_name in

  println_flush @@ version_name ^ " tests :";
  println_newline ();

  let err1 = test_ref naif_interp "naif" in
  let err2 = test_interp naif_interp cps_interp "naif" "cps" in
  let err3 = test_interp cps_interp defunc_interp "cps" "defunc" in

  print_newline ();
  if err1 || err2 || err3 then println_error @@ version_name ^ " : ERROR"
  else println_ok @@ version_name ^ " : OK"

let test_opti_gen ~test_opti ~naif_interp ~cps_interp ~defunc_interp ~version_name =
  let version_name = String.capitalize_ascii version_name in
  println_flush @@ version_name ^ " tests :";
  println_newline ();

  let err = test_opti ~naif_interp ~cps_interp ~defunc_interp in

  print_newline ();
  if err then println_error @@ version_name ^ " : ERROR"
  else println_ok @@ version_name ^ " : OK"

(* Weak test ref *)

let test_weak_cbn_ref = test_ref_gen ~test_ref:test_weak_cbn_with_ref

let test_weak_cbv_ref = test_ref_gen ~test_ref:test_weak_cbv_with_ref

let test_weak_cbnd_ref = test_ref_gen ~test_ref:test_weak_cbnd_with_ref

(* Weak test interp *)

let test_weak_interp, test_weak_cbv_interp, test_weak_cbnd_interp =
  let f = test_all_interp ~test_interp:test_weak_gen in
  (f, f, f)

(* Weak test all *)

let test_weak_cbn_all =
  test_all_gen ~test_ref:test_weak_cbn_with_ref ~test_interp:test_weak_gen

let test_weak_cbv_all =
  test_all_gen ~test_ref:test_weak_cbv_with_ref ~test_interp:test_weak_gen

let test_weak_cbnd_all =
  test_all_gen ~test_ref:test_weak_cbnd_with_ref ~test_interp:test_weak_gen

(* Weak test opti *)

let test_weak_cbn_opti = test_opti_gen ~test_opti:test_opti_weak_cbn

let test_weak_cbv_opti = test_opti_gen ~test_opti:test_opti_weak_cbv

let test_weak_cbnd_opti = test_opti_gen ~test_opti:test_opti_weak_cbnd

(* Strong test ref *)

let test_strong_cbv_ref = test_ref_gen ~test_ref:test_strong_cbv_with_ref

let test_strong_cbnd_ref = test_ref_gen ~test_ref:test_strong_cbn_with_ref

(* Strong test interp *)

let test_strong_cbv_interp, test_strong_cbnd_interp =
  let f = test_all_interp ~test_interp:test_strong_gen in
  (f, f)

(* Strong test all *)

let test_strong_cbv_all =
  test_all_gen ~test_ref:test_strong_cbv_with_ref ~test_interp:test_strong_gen

let test_strong_cbnd_all =
  test_all_gen ~test_ref:test_strong_cbn_with_ref ~test_interp:test_strong_gen

(* Strong test opti *)

let test_strong_cbv_opti = test_opti_gen ~test_opti:test_opti_strong_cbv

let test_strong_cbnd_opti = test_opti_gen ~test_opti:test_opti_strong_cbnd
