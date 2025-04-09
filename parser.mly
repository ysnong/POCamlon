%{
  open Ast
%}

%token LET BATTLE POKEMON
%token ELECTRIC WATER FIRE
%token <int> INT
%token <string> STRING
%token <string> IDENT
%token EQ LBRACKET RBRACKET LPAREN RPAREN COMMA
%token EOF

%start <expr> main
%%

main:
| command EOF { $1 }

command:
| LET IDENT EQ expr { Let($2, $4, Int 0) }  (* e.g. let pikachu = ... *)
| expr { $1 }

expr:
| INT { Int $1 }
| IDENT { Var $1 }
| POKEMON STRING type_name LBRACKET move_list RBRACKET INT 
  { PokeMon($2, $3, $5, $7) }
| BATTLE expr expr { Battle($2, $3) }

| LPAREN expr RPAREN { $2 }

type_name:
| ELECTRIC { Electric }
| WATER { Water }
| FIRE { Fire }

move_list:
| STRING { [$1] }
| move_list COMMA STRING { $1 @ [$3] }
| /* empty */ { [] }    (* allow an empty list *)
