%{
  open Lamb
%}


(* LISTE LEXEMES *)
%token <int> INT
%token ABS
%token LPAR RPAR DOT SEMI
%token EOF


(* POINT D'ENTREE *)
%start program
%type <Lamb.program> program

%%

let program :=
 | terms=decl*; EOF; { terms }

let decl ==
 | term=expr; SEMI; { term }

let expr :=
 | LPAR; e=expr; RPAR; { e }

 | LPAR; e1=expr; RPAR; LPAR; e2=expr; RPAR; { App(e1, e2) }
 | n=INT; { Var(n) }
 | ABS; DOT; ~=expr; <Abs>
