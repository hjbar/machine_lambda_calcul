open Testing

let test () =
  let naif_interp = Naif_interp.eval in
  let cps_interp = Cps_interp.eval in
  let defunc_interp = Defunc_interp.eval in
  let version_name = "weak_cbnd" in

  test_weak_cbnd_all ~naif_interp ~cps_interp ~defunc_interp ~version_name
