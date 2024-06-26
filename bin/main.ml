open State
open Printing
open Testing

let print_sep () =
  println_flush "---------------------------------------";
  println_flush "---------------------------------------";
  println_flush "---------------------------------------";
  println_flush "---------------------------------------";
  println_flush "---------------------------------------"

let () =
  Random.self_init ();

  match generate_mode with
  | Off ->
    print_sep ();
    Weak_cbn.test ();

    print_sep ();
    Weak_cbv.test ();

    print_sep ();
    Weak_cbnd.test ();

    print_sep ();
    Strong_cbv.test ();

    print_sep ();
    Strong_cbv_min.test ();

    print_sep ();
    Strong_cbnd.test ();

    print_sep ();
    Strong_cbnd_min.test ()
  | _ -> generate_terms ()
