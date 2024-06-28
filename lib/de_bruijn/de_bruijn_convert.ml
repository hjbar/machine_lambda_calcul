open Lambda
open Parse
open De_bruijn_def

let rec expr_to_de_bruijn (e : expr) : de_bruijn_term =
  match e with
  | Var x -> Var x
  | App (e1, e2) ->
    let t1 = expr_to_de_bruijn e1 in
    let t2 = expr_to_de_bruijn e2 in
    App (t1, t2)
  | Abs e ->
    let t = expr_to_de_bruijn e in
    Abs t

let lambda_to_de_bruijn (t : lambda_term) : de_bruijn_term =
  let rec loop (t : lambda_term) (d : int) (l : string list) : de_bruijn_term =
    match t with
    | Var x ->
      let n' = List.find_index (fun n -> n = x) l |> Option.value ~default:d in
      Var n'
    | App (t1, t2) ->
      let t1' = loop t1 d l in
      let t2' = loop t2 d l in
      App (t1', t2')
    | Abs (x, t1) ->
      let t1' = loop t1 (d + 1) (x :: l) in
      Abs t1'
  in
  loop t 0 []

let de_bruijn_to_lambda (t : de_bruijn_term) : lambda_term =
  let uid =
    let cpt = ref ~-1 in
    fun () ->
      incr cpt;
      !cpt
  in

  let bind n = Format.sprintf "#x%d" n in
  let free n = Format.sprintf "x%d" n in

  let rec loop (t : de_bruijn_term) (l : int list) : lambda_term =
    match t with
    | Var n -> begin
      match List.nth_opt l n with None -> Var (free n) | Some n' -> Var (bind n')
    end
    | App (t1, t2) ->
      let t1' = loop t1 l in
      let t2' = loop t2 l in
      App (t1', t2')
    | Abs t1 ->
      let id = uid () in
      let t1' = loop t1 (id :: l) in
      Abs (bind id, t1')
  in

  loop t []
