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
  | Off -> begin
    match testing_mode with
    | WeakOnly ->
      print_sep ();
      Weak_cbn.test ();

      print_sep ();
      Weak_cbv.test ();

      print_sep ();
      Weak_cbnd.test ()
    | StrongOnly ->
      print_sep ();
      Strong_cbv.test ();

      print_sep ();
      Strong_cbv_min.test ();

      print_sep ();
      Strong_cbnd.test ();

      print_sep ();
      Strong_cbnd_min.test ()
    | All ->
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
  end
  | _ -> generate_terms ()
