%{
  open Lamb
%}


(* LISTE LEXEMES *)
%token <int> INT
%token ABS
%token LPAR RPAR
%token DOT SEMI
%token EOF


(* POINT D'ENTREE *)
%start program
%type <Lamb.program> program

%%


let program :=
 | terms=hashtbl; EOF; { terms }

let hashtbl :=
 | e=decl; {
    let htbl = Hashtbl.create 16 in
    Hashtbl.replace htbl e ();
    htbl
 }

 | e=decl; h=hashtbl; {
    Hashtbl.replace h e ();
    h
 }

let decl ==
 | term=expr; SEMI; { term }

let expr :=
 | LPAR; e=expr; RPAR; { e }

 | LPAR; e1=expr; RPAR; LPAR; e2=expr; RPAR; { App(e1, e2) }
 | n=INT; { Var(n) }
 | ABS; DOT; e=expr; { Abs(e) }
