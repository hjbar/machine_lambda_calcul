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
  print_newline ()

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

let test_random_body reference_interp fun_interp reference_name fun_name
  ~(version : kind_reduction) =
  if debug then begin
    let version = match version with Weak -> "Weak" | Strong -> "Strong" in
    let reference_name = String.capitalize_ascii reference_name in
    let fun_name = String.capitalize_ascii fun_name in
    let msg =
      Format.sprintf "Strat (%s) - testing between %s & %s : " version reference_name fun_name
    in

    print_flush msg
  end;

  let htbl = Option.get @@ match version with Weak -> weak_terms | Strong -> strong_terms in
  let cpt = ref max_terms_tested in

  let () =
    try
      Hashtbl.iter
        begin
          fun term () ->
            let term = de_bruijn_to_lambda term in

            let reference = reference_interp term in
            let result = fun_interp term in

            if not @@ alpha_equiv reference result then begin
              if debug then print_debug term reference result;
              failwith "ERROR"
            end;

            decr cpt;
            if !cpt = 0 then raise (Return ())
        end
        htbl
    with Return () -> ()
  in

  if debug then begin
    println_data "OK";
    print_newline ()
  end

(* Functions for testing weak evaluator *)

let random_weak = test_random_body ~version:Weak

let test_random_weak_cbn_with_reference f s =
  random_weak beta_reduce_weak_cbn f "beta reduction" s

let test_random_weak_cbv_with_reference f s =
  random_weak beta_reduce_weak_cbv f "beta reduction" s

let test_random_weak_cbnd_with_reference f s =
  random_weak beta_reduce_weak_cbnd f "beta reduction" s

let test_random_weak f1 f2 s1 s2 = random_weak f1 f2 s1 s2

(* Functions for testing strong evaluator *)

let random_strong = test_random_body ~version:Strong

let test_random_strong_cbn_with_reference f s =
  random_strong beta_reduce_strong_cbn f "beta reduction" s

let test_random_strong_cbv_with_reference f s =
  random_strong beta_reduce_strong_cbv f "beta reduction" s

let test_random_strong_cbnd_with_reference f s =
  random_strong beta_reduce_strong_cbnd f "beta reduction" s

let test_random_strong f1 f2 s1 s2 = random_strong f1 f2 s1 s2
