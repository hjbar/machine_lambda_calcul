open Lambda

(* Generator of weak lambda_term  *)

let random_choose l =
  if List.is_empty l then failwith "Any element in the list";

  let arr = Array.of_list l in
  let idx = Random.full_int @@ Array.length arr in
  arr.(idx)

let rec create_term_weak (n : int) : lambda_term =
  let gensym, gensym_reset = get_gensym ~kind:Test in

  let rec loop n vars =
    match n with
    | 0 -> Var (random_choose vars)
    | n when Random.int 100 <= 80 ->
      let x = gensym () in
      Abs (x, loop (n - 1) (x :: vars))
    | n -> App (loop (n / 2) vars, loop (n / 2) vars)
  in

  gensym_reset ();
  try loop n [] with _ -> create_term_weak n

(* Generators of lambda_terms  *)

let p n m =
  let c1 = n + m in
  let c2 = n + m + 1 in
  (c1 * c2 / 2) + n

let solve k =
  let rec loop n m =
    if n > k && m = 0 then failwith "Any solution to find n & m";

    let calc = p n m in
    if calc < k then loop n (m + 1) else if calc > k then loop (n + 1) 0 else (n, m)
  in
  loop 0 0

let rec create_term_strong (i : int) : lambda_term =
  let gensym n = Format.sprintf "x%d" n in

  let rec loop i =
    match i mod 2 = 0 && i <> 0 with
    | false ->
      let j = i / 2 in
      Var (gensym j)
    | true -> begin
      let j = (i - 1) / 2 in
      match j mod 2 = 0 with
      | true ->
        let k = j / 2 in
        let n, m = solve k in
        App (loop n, loop m)
      | false ->
        let k = (j - 1) / 2 in
        let n, m = solve k in
        Abs (gensym n, loop m)
    end
  in

  try loop i with _ -> create_term_strong i
