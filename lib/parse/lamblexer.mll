{
  open Lexing
  open Lambparser

  let max_line = State.max_terms_tested
  let num_line lexbuf = lexbuf.lex_curr_p.pos_lnum
}

let digit = ['0'-'9']
let number = digit+

rule token = parse
  | ['\n']  {
    new_line lexbuf;
    if num_line lexbuf > max_line then EOF else token lexbuf
  }

  | [' ' '\t' '\r']+      { token lexbuf }

  | "//" [^ '\n']* "\n"   { new_line lexbuf; token lexbuf }
  | "/*"                  { comment lexbuf; token lexbuf  }

  | number as n           { INT  (int_of_string n) }
  | "("                   { LPAR }
  | ")"                   { RPAR }
  | "."                   { DOT  }
  | ";"                   { SEMI }

  | "λ"                   { ABS  }

  | eof                   { EOF  }
  | _                     { failwith ("unknown character : " ^ lexeme lexbuf) }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { failwith "unterminated comment" }
