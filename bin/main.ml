let () =
  print_newline ();
  print_newline ();

  Weak_cbn.test ();
  Weak_cbv.test ();
  Weak_cbnd.test ();
  Strong_cbv.test ();
  Strong_cbv_min.test ();
  Strong_cbnd.test ()
