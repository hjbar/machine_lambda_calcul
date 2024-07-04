open Lambda
open De_bruijn
open Printing
open Utils
open State
open Writing

(* Utils *)

let print_debug term reference result =
  print_newline ();

  print_flush "Terme à réduire : ";
  pp_lambda term;
  print_newline ();

  print_flush "Interp reference -> ";
  pp_lambda reference;
  print_newline ();

  print_flush "Interp test -> ";
  pp_lambda result;
  println_newline ()

(* Terms *)

let weak_terms, strong_terms =
  match generate_mode with
  | Off -> begin
    println_flush "Parsing tests ...";

    let weak_terms_htbl, strong_terms_htbl =
      match testing_mode with
      | WeakOnly -> (Option.some @@ weak_terms_htbl (), None)
      | StrongOnly -> (None, Option.some @@ strong_terms_htbl ())
      | All -> (Option.some @@ weak_terms_htbl (), Option.some @@ strong_terms_htbl ())
    in

    Sys.command "clear" |> ignore;

    (weak_terms_htbl, strong_terms_htbl)
  end
  | _ -> (None, None)

(* General functions of testing *)

let test_body reference_interp fun_interp reference_name fun_name ~(version : kind_reduction) =
  if debug then begin
    let reference_name = String.capitalize_ascii reference_name in
    let fun_name = String.capitalize_ascii fun_name in
    let msg = Format.sprintf "Testing between %s & %s : " reference_name fun_name in
    print_flush msg
  end;

  let htbl = Option.get @@ match version with Weak -> weak_terms | Strong -> strong_terms in

  try
    Hashtbl.iter
      begin
        fun term () ->
          let term = de_bruijn_to_lambda term in

          let reference = reference_interp term in
          let result = fun_interp term in

          if not @@ alpha_equiv reference result then begin
            if debug then begin
              println_warning "ERROR";
              print_debug term reference result
            end;

            raise (Return ())
          end
      end
      htbl;

    if debug then begin
      println_data "OK";
      print_newline ()
    end;

    false
  with
  | Return () -> true
  | exn ->
    if debug then begin
      print_warning "ERROR";
      println_newline ();
      print_flush @@ Printexc.to_string exn;
      println_newline ()
    end;

    true

let test_opti ~ref_interp ~naif_interp ~cps_interp ~defunc_interp ~(version : kind_reduction) =
  let test_equiv ref term kind =
    if not @@ alpha_equiv ref term then begin
      if debug then begin
        print_flush @@ Format.sprintf "Testing between beta_reduce & %s" kind;
        println_warning "ERROR";
        print_debug term ref term
      end;

      raise (Return ())
    end
  in

  let htbl = Option.get @@ match version with Weak -> weak_terms | Strong -> strong_terms in

  try
    Hashtbl.iter
      begin
        fun term () ->
          let term = de_bruijn_to_lambda term in
          let ref = ref_interp term in

          let naif = naif_interp term in
          test_equiv ref naif "naif";

          let cps = cps_interp term in
          test_equiv ref cps "cps";

          let defunc = defunc_interp term in
          test_equiv ref defunc "defunc"
      end
      htbl;

    false
  with
  | Return () -> true
  | exn ->
    if debug then begin
      print_warning "ERROR : ";
      print_flush @@ Printexc.to_string exn;
      println_newline ()
    end;
    true

(* Functions for testing weak evaluator *)

let test_weak = test_body ~version:Weak

let test_weak_cbn_with_ref f s = test_weak beta_reduce_weak_cbn f "beta reduction" s

let test_weak_cbv_with_ref f s = test_weak beta_reduce_weak_cbv f "beta reduction" s

let test_weak_cbnd_with_ref f s = test_weak beta_reduce_weak_cbnd f "beta reduction" s

let test_weak_gen f1 f2 s1 s2 = test_weak f1 f2 s1 s2

(* Functions for testing opti weak evaluator *)

let test_opti_weak = test_opti ~version:Weak

let test_opti_weak_cbn = test_opti_weak ~ref_interp:beta_reduce_weak_cbn

let test_opti_weak_cbv = test_opti_weak ~ref_interp:beta_reduce_weak_cbv

let test_opti_weak_cbnd = test_opti_weak ~ref_interp:beta_reduce_weak_cbnd

(* Functions for testing strong evaluator *)

let test_strong = test_body ~version:Strong

let test_strong_cbn_with_ref f s = test_strong beta_reduce_strong_cbn f "beta reduction" s

let test_strong_cbv_with_ref f s = test_strong beta_reduce_strong_cbv f "beta reduction" s

let test_strong_cbnd_with_ref f s = test_strong beta_reduce_strong_cbn f "beta reduction" s

let test_strong_gen f1 f2 s1 s2 = test_strong f1 f2 s1 s2

(* Functions for testing opti strong evaluator *)

let test_opti_strong = test_opti ~version:Strong

let test_opti_strong_cbn = test_opti_strong ~ref_interp:beta_reduce_strong_cbn

let test_opti_strong_cbv = test_opti_strong ~ref_interp:beta_reduce_strong_cbv

let test_opti_strong_cbnd = test_opti_strong ~ref_interp:beta_reduce_strong_cbn
