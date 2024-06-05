(* Definitions of types *)

open Lambda_ext

(* Functions for variable names *)

let gensym : unit -> string =
  let cpt = ref (-1) in
  fun () ->
    incr cpt;
    Format.sprintf "x%d" !cpt

(* Functions for interp *)

let rec n (b : extended_terms) (k : extended_terms list) : extended_terms =
  v b [] |> Fun.flip r k

and r (value : value) (k : extended_terms list) : extended_terms =
  match value with
  | Cst x -> apply_ext (Var x) k
  | Lam (x, b) ->
    let y = gensym () in
    let t = App (Abs (x, b), Ext [ Cst y ]) in
    apply_ext t k
  | Lst l -> begin
    let t_opt =
      List.fold_left
        begin
          Fun.flip
            begin
              fun t -> function
                | None -> Some (r t k)
                | Some acc -> Some (App (r t k, acc))
            end
        end
        None l
    in
    apply_ext (Option.get t_opt) k
  end

and v (t : extended_terms) (k : value list) : value =
  match beta_reduce t with
  | App (t1, t2) ->
    let v1 = apply_val t1 k in
    let v2 = apply_val t2 k in
    Lst [ v1; v2 ]
  | res -> apply_val res k

and apply_ext (_b : extended_terms) (_k : extended_terms list) : extended_terms
    =
  failwith "TODO"

and apply_val (_t : extended_terms) (_k : value list) : value = failwith "TODO"

(* Functions of eval *)

let eval (t : lambda_term) : lambda_term =
  t |> term_to_extended |> Fun.flip n [] |> extended_to_term
