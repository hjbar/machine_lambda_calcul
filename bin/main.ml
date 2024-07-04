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
      Weak_cbnd.test ();

      print_sep ()
    | StrongOnly ->
      print_sep ();
      Strong_cbv_readback.test ();

      print_sep ();
      Strong_cbnd_readback.test ();

      if with_sem then begin
        print_sep ();
        Strong_cbv_sem.test ();

        print_sep ();
        Strong_cbnd_sem.test ()
      end;

      print_sep ()
    | All ->
      print_sep ();
      Weak_cbn.test ();

      print_sep ();
      Weak_cbv.test ();

      print_sep ();
      Weak_cbnd.test ();

      print_sep ();
      Strong_cbv_readback.test ();

      print_sep ();
      Strong_cbnd_readback.test ();

      if with_sem then begin
        print_sep ();
        Strong_cbv_sem.test ();

        print_sep ();
        Strong_cbnd_sem.test ()
      end;

      print_sep ()
  end
  | _ -> generate_terms ()
