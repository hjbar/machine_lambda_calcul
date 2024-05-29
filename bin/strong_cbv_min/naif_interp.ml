(* Definitions of types *)

open Lambda

type extended_terms =
  | Var of string
  | App of extended_terms * extended_terms
  | Abs of string * extended_terms
  | Ext of value list

and value =
  | Cst of string
  | Lam of string * extended_terms
  | Lst of value list

(* Some utils functions *)

let rec extended_to_term : extended_terms -> lambda_term = function
  | Var x -> Var x
  | App (t1, t2) -> App (extended_to_term t1, extended_to_term t2)
  | Abs (x, t) -> Abs (x, extended_to_term t)
  | Ext _ -> assert false

let rec term_to_extended : lambda_term -> extended_terms = function
  | Var x -> Var x
  | App (t1, t2) -> App (term_to_extended t1, term_to_extended t2)
  | Abs (x, t) -> Abs (x, term_to_extended t)

let rec pp_extended : extended_terms -> unit = function
  | Var x -> print_flush @@ Format.sprintf "%s" x
  | App (t1, t2) -> begin
    print_flush "(";
    pp_extended t1;
    pp_extended t2;
    print_flush ")"
  end
  | Abs (x, t) -> begin
    print_flush @@ Format.sprintf "(%s" x;
    pp_extended t;
    print_flush ")"
  end
  | Ext l -> begin
    print_flush "[";
    List.iter
      (fun t ->
        pp_value t;
        print_flush ", " )
      l;
    print_flush "]"
  end

and pp_value : value -> unit = function
  | Cst x -> print_flush @@ Format.sprintf "%s" x
  | Lam (x, t) -> begin
    print_flush @@ Format.sprintf "(%s" x;
    pp_extended t;
    print_flush ")"
  end
  | Lst l -> begin
    print_flush "[";
    List.iter
      (fun t ->
        pp_value t;
        print_flush ", " )
      l;
    print_flush "]"
  end

(* Functions for variable names *)

let gensym : unit -> string =
  let cpt = ref (-1) in
  fun () ->
    incr cpt;
    Format.sprintf "x%d" !cpt

(* Functions for interp *)

let rec n (b : extended_terms) : extended_terms =
  pp_extended b;
  print_newline ();
  print_newline ();
  r @@ v b

and r : value -> extended_terms = function
  | Cst x -> Var x
  | Lam (x, b) ->
    let y = gensym () in
    let t = n @@ App (Abs (x, b), Ext [ Cst y ]) in
    Abs (y, t)
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

and v : extended_terms -> value = function
  | Var x -> Cst x
  | App (t1, t2) -> Lst [ v @@ n t1; v @@ n t2 ]
  | Abs (x, t) -> Lam (x, n t)
  | Ext l -> Lst l

(* Functions of eval *)

let eval (t : lambda_term) : lambda_term =
  if true then failwith "NAIF TODO";
  term_to_extended t |> n |> extended_to_term
