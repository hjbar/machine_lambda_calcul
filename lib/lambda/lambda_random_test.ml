open Lambda_def
open Lambda_print
open Lambda_utils
open Lambda_enum
open Lambda_globals

(* Constants *)

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

(* Reference Evaluator *)

let beta_reduce_weak_cbn = beta_reduce_weak_cbn ~max_recur:max_recursion

let beta_reduce_strong_cbn = beta_reduce_strong_cbn ~max_recur:max_recursion

let beta_reduce_weak_cbv = beta_reduce_weak_cbv ~max_recur:max_recursion

let beta_reduce_strong_cbv = beta_reduce_strong_cbv ~max_recur:max_recursion

let beta_reduce_weak_cbnd = beta_reduce_weak_cbnd ~max_recur:max_recursion

let beta_reduce_strong_cbnd = beta_reduce_strong_cbnd ~max_recur:max_recursion

(* Generator of weak lambda_term  *)

let random_choose l =
  if List.is_empty l then failwith "Any element in the list";

  let arr = Array.of_list l in
  let idx = Random.full_int @@ Array.length arr in
  arr.(idx)

let rec create_term_weak (n : int) : lambda_term =
  let gensym, gensym_reset = get_gensym ~kind:Test in
  let rec loop n vars =
    match n with
    | 0 -> Var (random_choose vars)
    | n when Random.int 100 <= 80 ->
      let x = gensym () in
      Abs (x, loop (n - 1) (x :: vars))
    | n -> App (loop (n / 2) vars, loop (n / 2) vars)
  in
  gensym_reset ();
  try loop n [] with _ -> create_term_weak n

(* Generators of lambda_terms  *)

let p n m =
  let c1 = n + m in
  let c2 = n + m + 1 in
  (c1 * c2 / 2) + n

let solve k =
  let rec loop n m =
    if n > k && m = 0 then failwith "Any solution to find n & m";

    let calc = p n m in
    if calc < k then loop n (m + 1)
    else if calc > k then loop (n + 1) 0
    else (n, m)
  in
  loop 0 0

let rec create_term_strong (i : int) : lambda_term =
  let gensym n = Format.sprintf "x%d" n in

  let rec loop i =
    match i mod 2 = 0 && i <> 0 with
    | false ->
      let j = i / 2 in
      Var (gensym j)
    | true -> begin
      let j = (i - 1) / 2 in
      match j mod 2 = 0 with
      | true ->
        let k = j / 2 in
        let n, m = solve k in
        App (loop n, loop m)
      | false ->
        let k = (j - 1) / 2 in
        let n, m = solve k in
        Abs (gensym n, loop m)
    end
  in

  try loop i with _ -> create_term_strong i

(* Random testing *)

let test_random_body reference_interp fun_interp reference_name fun_name
  ~gen_fun ~version ~rep ~prof =
  let gen_term gen prof ~version =
    let interp =
      match version with
      | Weak -> beta_reduce_weak_cbv
      | Strong -> beta_reduce_strong_cbv
    in
    let rec loop () =
      let term = gen @@ max 3 (Random.full_int prof) in
      try
        ignore (interp term);
        term
      with _ -> loop ()
    in
    loop ()
  in

  let rec loop n =
    match n with
    | 0 -> ()
    | n ->
      let term = gen_term gen_fun prof ~version in

      let reference = reference_interp term in
      let result = fun_interp term in

      if not @@ alpha_equiv reference result then begin
        if debug then print_debug term reference result;
        failwith "ERROR"
      end;

      loop (n - 1)
  in

  if debug then begin
    let version = match version with Weak -> "Weak" | Strong -> "Strong" in
    let reference_name = String.capitalize_ascii reference_name in
    let fun_name = String.capitalize_ascii fun_name in
    let msg =
      Format.sprintf "Random (%s) testing between %s & %s : " version
        reference_name fun_name
    in

    print_flush msg;
    loop rep;
    println_data "OK";
    print_newline ()
  end
  else loop rep

let random_testing f eval1 eval2 s1 s2 l =
  List.iter (fun (rep, prof) -> f rep prof eval1 eval2 s1 s2) l

let test_list ~version =
  let base, max_recur, up =
    match version with
    | Weak -> (1000, max_recursion / 100, fun prof -> prof * 2)
    | Strong -> (10000, max_recursion, fun prof -> prof * 10)
  in

  let rec loop rep prof acc =
    match rep with
    | 1 -> List.rev @@ ((rep, prof) :: acc)
    | _ -> loop (rep / 10) (up prof) ((rep, prof) :: acc)
  in

  loop max_recur base []

let test_list_weak = test_list ~version:Weak

let test_list_strong = test_list ~version:Strong

(* Functions for testing weak evaluator *)

let random_weak rep prof =
  test_random_body ~gen_fun:create_term_weak ~version:Weak ~rep ~prof

let test_random_weak_cbn_with_reference f s =
  random_testing random_weak beta_reduce_weak_cbn f "beta reduction" s
    test_list_weak

let test_random_weak_cbv_with_reference f s =
  random_testing random_weak beta_reduce_weak_cbv f "beta reduction" s
    test_list_weak

let test_random_weak_cbnd_with_reference f s =
  random_testing random_weak beta_reduce_weak_cbnd f "beta reduction" s
    test_list_weak

let test_random_weak f1 f2 s1 s2 =
  random_testing random_weak f1 f2 s1 s2 test_list_weak

(* Functions for testing strong evaluator *)

let random_strong rep prof =
  test_random_body ~gen_fun:create_term_strong ~version:Strong ~rep ~prof

let test_random_strong_cbn_with_reference f s =
  random_testing random_strong beta_reduce_strong_cbn f "beta reduction" s
    test_list_strong

let test_random_strong_cbv_with_reference f s =
  random_testing random_strong beta_reduce_strong_cbv f "beta reduction" s
    test_list_strong

let test_random_strong_cbnd_with_reference f s =
  random_testing random_strong beta_reduce_strong_cbnd f "beta reduction" s
    test_list_strong

let test_random_strong f1 f2 s1 s2 =
  random_testing random_strong f1 f2 s1 s2 test_list_strong
