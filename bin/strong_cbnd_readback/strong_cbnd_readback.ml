open Testing

let test () =
  let naif_interp = Naif_interp.eval in
  let cps_interp = Cps_interp.eval in
  let defunc_interp = Defunc_interp.eval in
  let version_name = "strong_cbnd_readback" in

  test_strong_cbnd_all ~naif_interp ~cps_interp ~defunc_interp ~version_name
