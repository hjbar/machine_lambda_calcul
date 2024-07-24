open Lambda

(* Generators of terms *)

val generate_terms : unit -> unit

(* Weak test ref *)

val test_weak_cbn_ref :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

val test_weak_cbv_ref :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

val test_weak_cbnd_ref :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

(* Weak test interp *)

val test_weak_interp :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

val test_weak_cbv_interp :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

val test_weak_cbnd_interp :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

(* Weak test all *)

val test_weak_cbn_all :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

val test_weak_cbv_all :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

val test_weak_cbnd_all :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

(* Weak test opti *)

val test_weak_cbn_opti :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

val test_weak_cbv_opti :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

val test_weak_cbnd_opti :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

(* Strong test ref *)

val test_strong_cbv_ref :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

val test_strong_cbnd_ref :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

(* Strong test interp *)

val test_strong_cbv_interp :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

val test_strong_cbnd_interp :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

(* Strong test all *)

val test_strong_cbv_all :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

val test_strong_cbnd_all :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

(* Strong test opti *)

val test_strong_cbv_opti :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit

val test_strong_cbnd_opti :
     naif_interp:(lambda_term -> lambda_term)
  -> cps_interp:(lambda_term -> lambda_term)
  -> defunc_interp:(lambda_term -> lambda_term)
  -> version_name:string
  -> unit
