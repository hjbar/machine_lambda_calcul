open State

(* Files for lambda-terms *)

let weak_lambda_file = "tests/weak_lambda_terms.lamb"

let strong_lambda_file = "tests/strong_lambda_terms.lamb"

let infinite_lambda_file = "tests/infinite_lambda_terms.lamb"

(* Parameters for testing *)

let max_terms_tested =
  match testing_state with
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
  | Mid -> (50000, 500000, [ 1; 10; 100; 1000 ])
  | High -> (100000, 1000000, [ 1; 10; 100; 1000; 10000 ])
