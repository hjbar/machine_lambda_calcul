open Printing
open Testing

let test () =
  let error = ref false in

  let () =
    try
      println_flush "Weak_cbn tests :";
      print_newline ();

      test_random_weak_cbn_with_reference Naif_interp.eval "naif";
      test_random_weak_cbn_with_reference Cps_interp.eval "cps";
      test_random_weak_cbn_with_reference Defunc_interp.eval "defunc";

      test_random_weak Naif_interp.eval Cps_interp.eval "naif" "cps";
      test_random_weak Naif_interp.eval Defunc_interp.eval "naif" "defunc";
      test_random_weak Cps_interp.eval Defunc_interp.eval "cps" "defunc";

      println_ok "Weak_cbn tests : OK";
      print_newline ()
    with err ->
      begin
        println_warning @@ Printexc.to_string err;
        print_newline ();

        println_error "Weak_cbn tests : ERROR";
        print_newline ();
        error := true
      end
  in

  if !error then println_error "Weak_cbn : ERROR" else println_ok "Weak_cbn : OK"
