let open_in_file file = open_in_gen [ Open_rdonly; Open_creat ] 0o666 file

let parse_file file =
  if not @@ Sys.file_exists State.tests_dir then Sys.mkdir State.tests_dir 0o777;
  let out_c = open_in_file file in

  try
    let lb = Lexing.from_channel out_c in
    let prog = Lambparser.program Lamblexer.token lb in
    close_in out_c;
    prog
  with _ ->
    begin
      close_in out_c;
      failwith @@ Format.sprintf "Error during parsing %s" file
    end
