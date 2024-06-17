let print_sep () =
  Lambda.println_flush "---------------------------------------";
  Lambda.println_flush "---------------------------------------";
  Lambda.println_flush "---------------------------------------";
  Lambda.println_flush "---------------------------------------";
  Lambda.println_flush "---------------------------------------"

let () =
  Random.self_init ();

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
  Strong_cbnd.test ()
