(* Files for generating lambda-terms *)

let weak_lambda_file = "tests/weak_lambda_terms.lamb"

let strong_lambda_file = "tests/strong_lambda_terms.lamb"

let infinite_lambda_file = "tests/infinite_lambda_terms.lamb"

(* Debug printing *)

let debug = false

(* Testing parameters *)

type testing_state =
  | Fast
  | Low
  | Mid
  | High
  | Full

let testing_speed = Fast

type kind_testing =
  | WeakOnly
  | StrongOnly
  | All

let testing_mode = All

let with_sem = false

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

(* Values for testing *)

let max_terms_tested =
  match testing_speed with
  | Fast -> 10000
  | Low -> 500000
  | Mid -> 1000000
  | High -> 2000000
  | Full -> max_int

(* Parameters of generating & writing *)

let max_recursions, max_rep, prof_list =
  match generate_mode with
  | Off -> (0, 0, [])
  | Low -> (10000, 100000, [ 1; 10; 100 ])
  | Mid -> (25000, 500000, [ 1; 10; 100; 1000 ])
  | High -> (50000, 1000000, [ 1; 10; 100; 1000; 10000 ])
  | VeryHigh -> (100000, 2500000, [ 1; 10; 100; 1000; 10000; 100000 ])

let mod_indicator = max_rep / 10
