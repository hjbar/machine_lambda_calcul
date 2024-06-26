{
  open Lexing
  open Lambparser
}

let digit = ['0'-'9']
let number = digit+

rule token = parse
  | ['\n']                { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+      { token lexbuf }

  | "//" [^ '\n']* "\n"   { new_line lexbuf; token lexbuf }
  | "/*"                  { comment lexbuf; token lexbuf }

  | number as n           { INT(int_of_string n) }
  | "("                   { LPAR }
  | ")"                   { RPAR }
  | "."                   { DOT }
  | ";"                   { SEMI }

  | "λ"                   { ABS }

  | eof                   { EOF }
  | _                     { failwith ("unknown character : " ^ lexeme lexbuf) }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { failwith "unterminated comment" }
