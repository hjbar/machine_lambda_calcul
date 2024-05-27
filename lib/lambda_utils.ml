open Lambda_def

let alpha_equiv e1 e2 =
  let module Env = Map.Make (String) in
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
