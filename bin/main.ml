let () =
  let () = print_newline () in
  let () = print_newline () in

  let () = Weak_cbn.test () in
  let () = Weak_cbnd.test () in
  let () = Strong_cbv.test () in
  let () = Strong_cbv_min.test () in
  let () = Strong_cbnd.test () in

  ()
