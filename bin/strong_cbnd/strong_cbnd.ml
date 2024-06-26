open Printing
open Testing

let test () =
  let error = ref false in

  let () =
    try
      println_flush "Strong_cbnd tests :";
      print_newline ();

      test_random_strong_cbnd_with_reference Naif_interp.eval "naif";
      test_random_strong_cbnd_with_reference Cps_interp.eval "cps";
      test_random_strong_cbnd_with_reference Defunc_interp.eval "defunc";

      test_random_strong Naif_interp.eval Cps_interp.eval "naif" "cps";
      test_random_strong Naif_interp.eval Defunc_interp.eval "naif" "defunc";
      test_random_strong Cps_interp.eval Defunc_interp.eval "cps" "defunc";

      println_ok "Strong_cbnd tests : OK";
      print_newline ()
    with err ->
      begin
        println_warning @@ Printexc.to_string err;
        print_newline ();

        println_error "Strong_cbnd Random : ERROR";
        print_newline ();
        error := true
      end
  in

  if !error then println_error "Strong_cbnd : ERROR" else println_ok "Strong_cbnd : OK"
