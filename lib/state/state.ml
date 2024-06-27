(* Debug printing *)

let debug = true

(* Testing parameters *)

type testing_state =
  | Fast
  | Low
  | Mid
  | High
  | Full

let testing_speed = Full

type kind_testing =
  | WeakOnly
  | StrongOnly
  | All

let testing_mode = WeakOnly

(* Generate parameters *)

type which_generation_state =
  | Weak
  | Strong
  | All

let which_generation_mode = All

type generate_state =
  | Off
  | Low
  | Mid
  | High
  | VeryHigh

let generate_mode = Off
