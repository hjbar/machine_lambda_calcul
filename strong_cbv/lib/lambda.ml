(* Define the type of lambda_terms *)

type lambda_term =
  | Var of string
  | App of lambda_term * lambda_term
  | Abs of string * lambda_term

(* Some functions on lambda_terms *)

module Env = Map.Make (String)

let alpha_equiv e1 e2 =
  let uid =
    let cpt = ref 0 in
    fun () ->
      incr cpt;
      !cpt
  in

  let rec loop e1 e2 m1 m2 k =
    match (e1, e2) with
    | Var x1, Var x2 -> begin
      let b =
        match (Env.find_opt x1 m1, Env.find_opt x2 m2) with
        | Some n1, Some n2 -> n1 = n2
        | _ -> x1 = x2
      in
      k b
    end
    | App (e1, e2), App (e1', e2') ->
      loop e1 e1' m1 m2 @@ fun b -> b && loop e2 e2' m1 m2 k
    | Abs (s1, e1), Abs (s2, e2) ->
      let id = uid () in
      loop e1 e2 (Env.add s1 id m1) (Env.add s2 id m2) k
    | _ -> k false
  in

  loop e1 e2 Env.empty Env.empty Fun.id

(* Some functions of printing *)

let print_newline () = Format.printf "\n%!"

let print_flush = Format.printf "%s%!"

let println_flush = Format.printf "%s\n%!"

let pp_lambda e =
  let rec loop fmt e =
    match e with
    | Var x -> Format.fprintf fmt "%s" x
    | Abs (s, e) -> Format.fprintf fmt "λ %s. %a" s loop e
    | App (e1, e2) -> begin
      match (e1, e2) with
      | Abs _, Abs _ | App _, App _ ->
        Format.fprintf fmt "(%a) (%a)" loop e1 loop e2
      | _, Var _ | Var _, _ -> Format.fprintf fmt "%a %a" loop e1 loop e2
      | _ -> Format.fprintf fmt "%a (%a)" loop e1 loop e2
    end
  in
  loop Format.std_formatter e

let pp_result t res =
  pp_lambda t;
  print_flush " -> ";
  pp_lambda res;
  print_newline ()

(* Some functions of testing *)

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
