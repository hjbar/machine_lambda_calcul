let () =
  let () = print_newline () in
  let () = print_newline () in

  Weak_cbn.test ();
  Weak_cbnd.test ();
  Strong_cbv.test ();
  Strong_cbnd.test ();

  ()
