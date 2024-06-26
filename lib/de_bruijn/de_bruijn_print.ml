open De_bruijn_def

let rec aux fmt t =
  match t with
  | Var x -> Format.pp_print_int fmt x
  | App (t1, t2) -> Format.fprintf fmt "(%a) (%a)" aux t1 aux t2
  | Abs t1 -> Format.fprintf fmt "λ. %a" aux t1

let pp_de_bruijn t = aux Format.std_formatter t

(*
let de_bruijn_to_string t =
  let buffer = Buffer.create 100 in
  let fmt = Format.formatter_of_buffer buffer in
  aux fmt t;
  let res = Buffer.contents buffer in

  if res = "" then failwith "TROP NUL";

  res
*)
(*
let de_bruijn_to_string t : string =
  let buffer = Buffer.create 100 in
  let fmt = Format.formatter_of_buffer buffer in
  aux fmt t;
  let res = Buffer.contents buffer in
  Buffer.clear buffer;
  res
  *)

let de_bruijn_to_string t =
  let rec loop t =
    match t with
    | Var x -> string_of_int x
    | App (t1, t2) -> Format.sprintf "(%s) (%s)" (loop t1) (loop t2)
    | Abs t1 -> Format.sprintf "λ. %s" (loop t1)
  in
  loop t
