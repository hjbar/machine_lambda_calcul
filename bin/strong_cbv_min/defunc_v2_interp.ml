(* Definitions of types *)

open Lambda_ext

(* Functions for variable names *)

let gensym : unit -> string =
  let cpt = ref (-1) in
  fun () ->
    incr cpt;
    Format.sprintf "x%d" !cpt

(* Functions for interp *)

let rec n (b : extended_terms) (k : extended_terms -> extended_terms) :
  extended_terms =
  v b @@ fun b' -> r b' k

and r (value : value) (k : extended_terms -> extended_terms) : extended_terms =
  match value with
  | Cst x -> k @@ Var x
  | Lam (x, b) ->
    let y = gensym () in
    let t = App (Abs (x, b), Ext [ Cst y ]) in
    n t @@ fun t' -> k @@ Abs (y, t')
  | Lst l -> begin
    Cps.map r l @@ fun l' ->
    let t_opt =
      List.fold_left
        begin
          Fun.flip
            begin
              fun t -> function None -> Some t | Some t' -> Some (App (t', t))
            end
        end
        None l'
    in
    k @@ Option.get t_opt
  end

and v (t : extended_terms) (k : value -> extended_terms) : extended_terms =
  match beta_reduce t with
  | Var x -> k @@ Cst x
  | App (t1, t2) ->
    v t1 @@ fun t1' ->
    v t2 @@ fun t2' -> k @@ Lst [ t1'; t2' ]
  | Abs (x, t) -> k @@ Lam (x, t)
  | Ext l -> k @@ Lst l

(* Functions of eval *)

let eval (t : lambda_term) : lambda_term =
  ignore @@ failwith "DEFUNC TODO";
  let t' = term_to_extended t |> Fun.flip n @@ Fun.id in
  extended_to_term t'
