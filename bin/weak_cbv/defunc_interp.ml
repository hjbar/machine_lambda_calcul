open Lambda

(* Define some types *)

module StringMap = Map.Make (String)

type env = Env of (lambda_term * env) StringMap.t

let get_env e = match e with Env e -> e

let set_env e = Env e

(* Functions of interp *)

let rec interp (t : lambda_term) (e : env) (k : (lambda_term * env) list) :
  lambda_term * env =
  match t with
  | Var x ->
    let t', e' = get_env e |> StringMap.find x in
    interp t' e' k
  | Abs _ -> apply t e k
  | App (t1, t2) -> interp t1 e ((t2, e) :: k)

and apply (t : lambda_term) (e : env) (k : (lambda_term * env) list) :
  lambda_term * env =
  match k with
  | [] -> (t, e)
  | (t', e') :: k' -> begin
    match t with
    | Abs (x, t) ->
      let e = get_env e |> StringMap.add x (t', e') |> set_env in
      interp t e k'
    | _ -> assert false
  end

(* Functions of eval *)

let eval t =
  ignore @@ failwith "DEFUNC TODO";
  interp t (Env StringMap.empty) [] |> fst
