open Printing
open Testing

let test () =
  let error = ref false in

  let () =
    try
      println_flush "Strong_cbv_min tests :";
      print_newline ();

      test_random_strong_cbv_with_reference Naif_interp.eval "naif";
      test_random_strong_cbv_with_reference Cps_interp.eval "cps";
      test_random_strong_cbv_with_reference Defunc_interp.eval "defunc";

      test_random_strong Naif_interp.eval Cps_interp.eval "naif" "cps";
      test_random_strong Naif_interp.eval Defunc_interp.eval "naif" "defunc";
      test_random_strong Defunc_interp.eval Cps_interp.eval "defunc" "cps";

      println_ok "Strong_cbv_min tests : OK";
      print_newline ()
    with err ->
      begin
        println_warning @@ Printexc.to_string err;
        print_newline ();

        println_error "Strong_cbv_min tests : ERROR";
        print_newline ();
        error := true
      end
  in

  if !error then println_error "Strong_cbv_min : ERROR" else println_ok "Strong_cbv_min : OK"
