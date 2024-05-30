(* Definitions of types *)

open Lambda_ext

(* Functions for variable names *)

let gensym : unit -> string =
  let cpt = ref (-1) in
  fun () ->
    incr cpt;
    Format.sprintf "x%d" !cpt

(* Functions for interp *)

let rec n (b : extended_terms) : extended_terms =
  print_newline ();
  print_newline ();
  pp_lambda_ext b;
  print_newline ();
  print_newline ();
  r @@ v b

and r : value -> extended_terms = function
  | Cst x -> Var x
  | Lam (x, b) ->
    let y = gensym () in
    let t = App (Abs (x, b), Ext [ Cst y ]) in
    Abs (y, n t)
  | Lst l -> begin
    let t_opt =
      List.fold_left
        begin
          Fun.flip
            begin
              fun t -> function
                | None -> Some (r t)
                | Some t' -> Some (App (t', r t))
            end
        end
        None l
    in
    Option.get t_opt
  end

and v (t : extended_terms) : value =
  match beta_reduce t with
  | Var x -> Cst x
  | App (t1, t2) -> Lst [ v t1; v t2 ]
  | Abs (x, t) -> Lam (x, t)
  | Ext l -> Lst l

(* Functions of eval *)

let eval (t : lambda_term) : lambda_term =
  if true then failwith "DEFUNC TODO";
  t |> term_to_extended |> n |> extended_to_term
