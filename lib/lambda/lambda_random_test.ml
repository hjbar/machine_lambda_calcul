open Lambda_def
open Lambda_print
open Lambda_utils

(* Generator of lambda_term  *)

let random_choose l =
  match l with
  | [] -> failwith "Any element in the list"
  | [ x ] -> x
  | x :: l' ->
    List.fold_left (fun acc x -> if Random.int 100 < 50 then x else acc) x l'

let gensym =
  let cpt = ref (-1) in
  fun () ->
    incr cpt;
    Format.sprintf "x%d" !cpt

let rec create_term_naif (n : int) : lambda_term =
  let rec loop n vars =
    match n with
    | 0 -> Var (random_choose vars)
    | n when Random.int 100 <= 80 ->
      let x = gensym () in
      Abs (x, loop (n - 1) (x :: vars))
    | n -> App (loop (n / 2) vars, loop (n / 2) vars)
  in

  try loop n [] with _ -> create_term_naif n

let p n m =
  let c1 = n + m in
  let c2 = n + m + 1 in
  (c1 * c2 / 2) + n

let solve k =
  let rec loop n m =
    let calc = p n m in
    if calc < k then loop n (m + 1)
    else if m = 0 && calc > k then failwith "Any solution"
    else if calc > k then loop (n + 1) 0
    else (n, m)
  in
  loop 0 0

let var_name n = Format.sprintf "x%d" n

let rec create_term (i : int) : lambda_term =
  match i mod 2 = 0 with
  | false ->
    let j = i / 2 in
    Var (var_name j)
  | true ->
    let j = (i - 1) / 2 in
    if j mod 2 = 0 then
      let k = j / 2 in
      let n, m = solve k in
      App (create_term n, create_term m)
    else
      let k = (j - 1) / 2 in
      let n, m = solve k in
      Abs (var_name n, create_term m)

(* Random testing *)

let test_random_body (reference_interp : lambda_term -> lambda_term)
  (fun_interp : lambda_term -> lambda_term) (reference_name : string)
  (fun_name : string) ~(gen_fun : int -> lambda_term) ~(version : string)
  ~(rep : int) ~(prof : int) : unit =
  let rec loop n =
    match n with
    | 0 -> ()
    | n ->
      let term = gen_fun @@ max 3 (Random.int prof) in

      let reference = reference_interp term in
      let result = fun_interp term in

      if not @@ alpha_equiv reference result then failwith "ERROR";
      loop (n - 1)
  in

  let version = String.capitalize_ascii version in
  let reference_name = String.capitalize_ascii reference_name in
  let fun_name = String.capitalize_ascii fun_name in
  let msg =
    Format.sprintf "Random (%s) testing between %s & %s : " version
      reference_name fun_name
  in
  print_flush msg;

  Random.self_init ();
  loop rep;
  println_flush "OK";
  print_newline ()

let random_testing f eval1 eval2 s1 s2 l =
  List.iter
    begin
      fun (rep, prof) -> f rep prof eval1 eval2 s1 s2
    end
    l

let random_naif rep prof =
  test_random_body ~gen_fun:create_term_naif ~version:"naif" ~rep ~prof

let random_avance rep prof =
  test_random_body ~gen_fun:create_term ~version:"avancé" ~rep ~prof

let test_random_naif f1 f2 s1 s2 =
  random_testing random_naif f1 f2 s1 s2
    [ (10000, 5); (1000, 10); (100, 15); (10, 20); (5, 25) ]

let test_random f1 f2 s1 s2 =
  random_testing random_avance f1 f2 s1 s2
    [ (10000, 5); (1000, 10); (100, 15); (10, 20); (5, 25) ]
