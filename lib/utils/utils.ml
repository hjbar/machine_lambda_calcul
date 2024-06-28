(* Exception *)

exception Return of unit

(* Enum *)

type kind_reduction =
  | Weak
  | Strong

type kind_gensym =
  | Interp
  | Test

(* Gensym *)

let gensym_general var =
  let cpt = ref ~-1 in

  let gensym () =
    incr cpt;
    Format.sprintf "%s%d" var !cpt
  in
  let reset () = cpt := 0 in

  (gensym, reset)

let get_gensym ~kind =
  match kind with Interp -> gensym_general "#x" | Test -> gensym_general "x"
