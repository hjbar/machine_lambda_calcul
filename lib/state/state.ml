(* Debug printing *)

let debug = true

(* Testing parameters *)

type testing_state =
  | Fast
  | Low
  | Mid
  | High
  | Full

let testing_state = Fast

(* Generate parameters *)

type generate_state =
  | Off
  | Low
  | Mid
  | High

let generate_mode = Off
