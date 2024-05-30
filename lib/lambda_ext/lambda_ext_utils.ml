open Lambda
open Lambda_ext_def

let subst (t1 : extended_terms) (s : string) (t2 : extended_terms) :
  extended_terms =
  let rec loop t l k =
    match t with
    | Var x -> k @@ if x = s && not (List.mem x l) then t2 else t
    | App (t1, t2) ->
      loop t1 l @@ fun t1' ->
      loop t2 l @@ fun t2' -> k @@ App (t1', t2')
    | Abs (str, _) when str = s -> k t
    | Abs (str, t1) -> loop t1 (str :: l) @@ fun t1' -> k @@ Abs (str, t1')
    | Ext _ -> k t
  in
  loop t1 [] Fun.id

let beta_reduce (t : extended_terms) : extended_terms =
  let rec loop t k =
    match t with
    | Var _ -> k t
    | App (t1, t2) -> begin
      loop t1 @@ fun t1' ->
      loop t2 @@ fun t2' ->
      match (t1', t2') with
      | Abs (s, t1''), t2'' -> loop (subst t1'' s t2'') k
      | _ -> k @@ App (t1', t2')
    end
    | Abs (s, t) -> loop t @@ fun t' -> k @@ Abs (s, t')
    | Ext _ -> k t
  in
  loop t Fun.id

let rec extended_to_term : extended_terms -> lambda_term = function
  | Var x -> Var x
  | App (t1, t2) -> App (extended_to_term t1, extended_to_term t2)
  | Abs (x, t) -> Abs (x, extended_to_term t)
  | Ext _ -> assert false

let rec term_to_extended : lambda_term -> extended_terms = function
  | Var x -> Var x
  | App (t1, t2) -> App (term_to_extended t1, term_to_extended t2)
  | Abs (x, t) -> Abs (x, term_to_extended t)
