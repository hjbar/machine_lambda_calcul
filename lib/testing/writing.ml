open Lambda
open De_bruijn
open Printing
open Utils
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

let file_to_htbl file limit =
  let terms = parse_file file in
  let htbl = Hashtbl.create 16 in

  let () =
    try
      List.iteri
        begin
          fun i t ->
            if i = limit then raise (Return ());

            let t' = expr_to_de_bruijn t in
            Hashtbl.replace htbl t' ()
        end
        terms
    with Return () -> ()
  in

  htbl

let weak_terms_htbl ?(limit = max_int) () = file_to_htbl weak_lambda_file limit

let strong_terms_htbl ?(limit = max_int) () = file_to_htbl strong_lambda_file limit

let infinite_terms_htbl ?(limit = max_int) () = file_to_htbl infinite_lambda_file limit

(* General terms writing *)

let write_generate_term t ~out_htbl ~inf_htbl ~out_c ~inf_c ~interp =
  let t' = lambda_to_de_bruijn t in

  try
    begin
      if Hashtbl.mem out_htbl t' then ();

      interp t |> ignore;
      Hashtbl.replace out_htbl t' ();
      write_term out_c t'
    end
  with _ ->
    begin
      if Hashtbl.mem inf_htbl t' then ();

      Hashtbl.replace inf_htbl t' ();
      write_term inf_c t'
    end

(* Weak terms writing *)

let test_list ~rep =
  let aux prof =
    let base = max 1 (prof / 100) in
    let rep = rep in
    let up prof = prof * 2 in

    let rec loop rep prof acc =
      match rep with
      | 1 -> (rep, prof) :: acc
      | _ -> loop (rep / 10) (up prof) ((rep, prof) :: acc)
    in
    loop rep base []
  in

  List.fold_left (fun acc prof -> aux prof @ acc) [] prof_list

let generate_weak_term ~weak_htbl ~inf_htbl ~weak_c ~inf_c ~prof =
  let t = create_term_weak @@ max 3 (Random.full_int prof) in

  let out_htbl = weak_htbl in
  let out_c = weak_c in
  let interp = weak_interp in

  write_generate_term t ~out_htbl ~out_c ~inf_htbl ~inf_c ~interp

let generate_weak_terms ~weak_htbl ~inf_htbl ~weak_c ~inf_c =
  if debug then println_flush "Début generate_weak_terms";

  let () =
    List.iteri
      begin
        fun i (rep, prof) ->
          if debug then println_flush @@ Format.sprintf "%dième iteration" (i + 1);

          for j = 0 to rep do
            if debug && j <> 0 && j mod mod_indicator = 0 then
              println_flush @@ Format.sprintf "%dième term" j;

            generate_weak_term ~weak_htbl ~inf_htbl ~weak_c ~inf_c ~prof
          done
      end
      (test_list ~rep:max_rep)
  in

  if debug then println_flush "Fin generate_weak_terms"

(* Strong terms writing *)

let generate_strong_term i ~strong_htbl ~inf_htbl ~strong_c ~inf_c =
  let t = create_term_strong i in

  let out_htbl = strong_htbl in
  let out_c = strong_c in
  let interp = strong_interp in

  write_generate_term t ~out_htbl ~out_c ~inf_htbl ~inf_c ~interp

let generate_strong_terms ~strong_htbl ~inf_htbl ~strong_c ~inf_c ~limit =
  if debug then println_flush "Début generate_strong_terms";

  for i = 0 to limit * 2 do
    if debug && i <> 0 && i mod mod_indicator = 0 then
      println_flush @@ Format.sprintf "%dième term" i;

    generate_strong_term i ~strong_htbl ~inf_htbl ~strong_c ~inf_c
  done;

  if debug then println_flush "Fin generate_strong_terms"

(* Generate terms *)

let generate_terms () =
  if debug then println_flush "Début generate_terms\nParsing terms...";

  if debug then println_flush "Parsing terms...";

  let inf_c = open_out_file infinite_lambda_file in
  let inf_htbl = infinite_terms_htbl () in

  let () =
    match which_generation_mode with
    | Weak ->
      let weak_c = open_out_file weak_lambda_file in
      let weak_htbl = weak_terms_htbl () in

      generate_weak_terms ~weak_htbl ~inf_htbl ~weak_c ~inf_c;
      close_out weak_c
    | Strong ->
      let strong_c = open_out_file strong_lambda_file in
      let strong_htbl = strong_terms_htbl () in

      generate_strong_terms ~strong_htbl ~inf_htbl ~strong_c ~inf_c ~limit:max_rep;
      close_out strong_c
    | All ->
      let weak_c = open_out_file weak_lambda_file in
      let strong_c = open_out_file strong_lambda_file in

      let weak_htbl = weak_terms_htbl () in
      let strong_htbl = strong_terms_htbl () in

      generate_weak_terms ~weak_htbl ~inf_htbl ~weak_c ~inf_c;
      generate_strong_terms ~strong_htbl ~inf_htbl ~strong_c ~inf_c ~limit:max_rep;

      close_out weak_c;
      close_out strong_c
  in

  close_out inf_c;

  if debug then println_flush "Fin generate_terms"
