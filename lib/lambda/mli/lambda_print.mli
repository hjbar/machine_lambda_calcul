open Lambda_def

val red_string : string -> string

val green_string : string -> string

val blue_string : string -> string

val print_newline : unit -> unit

val print_flush : string -> unit

val println_flush : string -> unit

val pp_lambda : lambda_term -> unit

val pp_struct : lambda_term -> unit

val pp_result : lambda_term -> lambda_term -> unit
