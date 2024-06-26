open Lambda
open De_bruijn
open Printing
open Parse
open State
open Globals
open Generators

(* Interpters of reference *)

let weak_interp = beta_reduce_weak_cbv ~max_recur:max_recursions

let strong_interp = beta_reduce_strong_cbv ~max_recur:max_recursions

(* Writing utils *)

let open_out_file file = open_out_gen [ Open_append; Open_creat ] 0o666 file

let write_term out_c t = output_string out_c @@ Format.sprintf "%s ;\n" (de_bruijn_to_string t)

(* File to htbl *)

let file_to_htbl file =
  let terms = parse_file file in
  let htbl = Hashtbl.create 16 in

  List.iter
    begin
      fun t ->
        let t' = expr_to_de_bruijn t in
        Hashtbl.replace htbl t' ()
    end
    terms;

  htbl

let weak_terms_htbl () = file_to_htbl weak_lambda_file

let strong_terms_htbl () = file_to_htbl strong_lambda_file

let infinite_terms_htbl () = file_to_htbl infinite_lambda_file

(* General terms writing *)

let generate_term ~out_htbl ~inf_htbl ~out_c ~inf_c ~gen ~interp ~prof =
  let t = gen @@ max 3 (Random.full_int prof) in
  let t' = lambda_to_de_bruijn t in

  try
    if not @@ Hashtbl.mem out_htbl t' then begin
      ignore @@ interp t;
      Hashtbl.replace out_htbl t' ();
      write_term out_c t'
    end
  with _ -> if not @@ Hashtbl.mem inf_htbl t' then write_term inf_c t'

(* Weak terms writing *)

let generate_weak_term ~weak_htbl ~inf_htbl ~weak_c ~inf_c ~prof =
  let gen = create_term_weak in
  let interp = weak_interp in
  generate_term ~out_htbl:weak_htbl ~out_c:weak_c ~inf_htbl ~inf_c ~gen ~interp ~prof

let generate_weak_terms ~weak_htbl ~inf_htbl ~weak_c ~inf_c ~rep ~prof =
  if debug then println_flush "Début generate_weak_terms";

  for _ = 0 to rep do
    generate_weak_term ~weak_htbl ~inf_htbl ~weak_c ~inf_c ~prof
  done;

  if debug then println_flush "Fin generate_weak_terms"

(* Strong terms writing *)

let generate_strong_term ~strong_htbl ~inf_htbl ~strong_c ~inf_c ~prof =
  let gen = create_term_strong in
  let interp = strong_interp in
  generate_term ~out_htbl:strong_htbl ~out_c:strong_c ~inf_htbl ~inf_c ~gen ~interp ~prof

let generate_strong_terms ~strong_htbl ~inf_htbl ~strong_c ~inf_c ~rep ~prof =
  if debug then println_flush "Début generate_strong_terms";

  for _ = 0 to rep do
    generate_strong_term ~strong_htbl ~inf_htbl ~strong_c ~inf_c ~prof
  done;

  if debug then println_flush "Fin generate_strong_terms"

(* Testing params lists *)

let test_list ~prof ~version =
  let base, rep, up =
    match version with
    | Weak -> (max 1 (prof / 100), max_rep, fun prof -> prof * 2)
    | Strong -> (prof * prof, max_rep, fun prof -> prof * 10)
  in

  let rec loop rep prof acc =
    match rep with
    | 1 -> (rep, prof) :: acc
    | _ -> loop (rep / 10) (up prof) ((rep, prof) :: acc)
  in

  loop rep base []

let test_list_weak = test_list ~version:Weak

let test_list_strong = test_list ~version:Strong

(* Generate terms *)

let generate_terms () =
  if debug then println_flush "Début generate_terms";

  let weak_c = open_out_file weak_lambda_file in
  let strong_c = open_out_file strong_lambda_file in
  let inf_c = open_out_file infinite_lambda_file in

  let weak_htbl = weak_terms_htbl () in
  let strong_htbl = strong_terms_htbl () in
  let inf_htbl = infinite_terms_htbl () in

  List.iter
    begin
      fun prof_base ->
        List.iter
          begin
            fun (rep, prof) ->
              generate_weak_terms ~weak_htbl ~inf_htbl ~weak_c ~inf_c ~rep ~prof
          end
          (test_list_weak ~prof:prof_base);

        List.iter
          begin
            fun (rep, prof) ->
              generate_strong_terms ~strong_htbl ~inf_htbl ~strong_c ~inf_c ~rep ~prof
          end
          (test_list_strong ~prof:prof_base)
    end
    prof_list;

  close_out weak_c;
  close_out strong_c;
  close_out inf_c;

  if debug then println_flush "Début generate_terms"
