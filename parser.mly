%{
  open Ast
%}

%token LET BATTLE POKEMON
%token ELECTRIC WATER FIRE
%token STATALL
%token <int> INT
%token <string> STRING
%token <string> IDENT
%token EQ LBRACKET RBRACKET LPAREN RPAREN COMMA
%token LT GT PLUS MINUS TIMES EQQ
%token DOT
%token PRINT
%token IF THEN ELSE
%token EOF
%token FUN ARROW
%token TYPEOF
%token IN
%token <bool> BOOL

%left EQQ
%left LT GT
%left PLUS MINUS
%left TIMES

%start <expr> main
%%

main:
| command EOF { $1 }

command:
| LET IDENT EQ expr { Let($2, $4, Int 0) }  (* e.g. let pikachu = ... *)
| expr { $1 }

expr:
| expr DOT IDENT { FieldAccess($1, $3) }
| expr simple_expr { App($1, $2) }
| simple_expr { $1 }

simple_expr:
| INT { Int $1 }
| IDENT { Var $1 }
| POKEMON STRING type_name LBRACKET move_list RBRACKET INT { PokeMon($2, $3, $5, $7) }
| BATTLE expr expr { Battle($2, $3) }
| LPAREN expr RPAREN { $2 }
| PRINT expr { Print $2 }
| simple_expr PLUS simple_expr { Primop("+", $1, $3) }
| simple_expr MINUS simple_expr { Primop("-", $1, $3) }
| simple_expr TIMES simple_expr { Primop("*", $1, $3) }
| simple_expr LT simple_expr { Primop("<", $1, $3) }
| simple_expr GT simple_expr { Primop(">", $1, $3) }
| simple_expr EQQ simple_expr { Primop("==", $1, $3) }
| IF expr THEN expr ELSE expr { If($2, $4, $6) }
| STATALL expr { StatAll($2) }
| FUN IDENT ARROW expr { Fun($2, $4) }
| TYPEOF expr { TypeOf($2) }
| LET IDENT EQ expr IN expr { Let($2, $4, $6) }
| BOOL { Bool $1 }
| STRING { String $1 }

type_name:
| ELECTRIC { Electric }
| WATER { Water }
| FIRE { Fire }

move_list:
| STRING { [$1] }
| move_list COMMA STRING { $1 @ [$3] }
| /* empty */ { [] }    (* allow an empty list *)
