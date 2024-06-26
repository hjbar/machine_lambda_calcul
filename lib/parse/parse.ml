include Lamb

let open_in_file file = open_in_gen [ Open_rdonly; Open_creat ] 0o666 file

let parse_file file =
  let c = open_in_file file in

  try
    let lb = Lexing.from_channel c in
    let prog = Lambparser.program Lamblexer.token lb in
    close_in c;
    prog
  with _ ->
    begin
      close_in c;
      failwith @@ Format.sprintf "Error during parsing %s" file
    end
