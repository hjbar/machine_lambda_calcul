open Lambda_def
open Lambda_print
open Lambda_utils

let test_eval t res f msg =
  let t' = f t in
  pp_result t t';
  if not @@ alpha_equiv t' res then failwith msg

let test_weak_bis f s1 s2 with_omega pp =
  let s_maj = String.capitalize_ascii s2 in
  println_flush @@ s_maj ^ "_interp :";

  (* TEST 1 *)
  let t = Abs ("x", Var "x") in
  let res = Abs ("x", Var "x") in
  let msg_error = "Error with test1 in " ^ s1 ^ "_" ^ s2 in
  test_eval t res f msg_error;

  (* TEST 2 *)
  if with_omega then begin
    ignore
      (let omega =
         let delta = Abs ("x", App (Var "x", Var "x")) in
         App (delta, delta)
       in
       f omega |> pp_result omega;
       let msg_error = "Error with test2 in " ^ s1 ^ "_" ^ s2 in
       failwith msg_error )
  end;

  if pp then begin
    println_flush @@ s1 ^ "_" ^ s2 ^ " : OK";
    print_newline ()
  end

let test_weak f s1 s2 with_omega = test_weak_bis f s1 s2 with_omega true

let test_strong f s1 s2 with_omega =
  test_weak_bis f s1 s2 with_omega false;

  (* TEST 3 *)
  let church_int n =
    let rec loop = function
      | 0 -> Var "x"
      | n -> App (Var "f", loop @@ (n - 1))
    in
    Abs ("f", Abs ("x", loop n))
  in

  let church_add n1 n2 =
    let add =
      Abs
        ( "m"
        , Abs
            ( "n"
            , Abs
                ( "f"
                , Abs
                    ( "x"
                    , App
                        ( App (Var "m", Var "f")
                        , App (App (Var "n", Var "f"), Var "x") ) ) ) ) )
    in
    App (App (add, n1), n2)
  in

  let deux = church_int 2 in
  let trois = church_int 3 in
  let t = church_add deux trois in

  let cinq = church_int 5 in
  let msg_error = "Error with test1 in " ^ s1 ^ "_" ^ s2 in
  test_eval t cinq f msg_error;

  println_flush @@ s1 ^ "_" ^ s2 ^ " : OK";
  print_newline ()
