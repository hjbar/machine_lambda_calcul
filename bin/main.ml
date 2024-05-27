let () =
  Weak_cbn.test ();
  Weak_cbnd.test ();
  Strong_cbv.test ();
  Strong_cbnd.test ()
