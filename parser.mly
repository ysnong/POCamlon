%{
  open Ast
  open Global_env
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
%token TYPE BAR

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
| INT { Int $1 }
| IDENT {
    match $1 with
    | "true" -> Bool true
    | "false" -> Bool false
    | s ->
      if List.exists (fun (_, ctors) -> List.mem s ctors) (StringMap.bindings !Global_env.user_types)
      then Constructor s
      else Var s
  }
| POKEMON STRING type_name LBRACKET move_list RBRACKET INT
  { PokeMon($2, $3, $5, $7) }
| BATTLE expr expr { Battle($2, $3) }
| expr DOT IDENT { FieldAccess($1, $3) }
| LPAREN expr RPAREN { $2 }
| PRINT expr { Print $2 }
| expr PLUS expr   { Primop("+", $1, $3) }
| expr MINUS expr  { Primop("-", $1, $3) }
| expr TIMES expr  { Primop("*", $1, $3) }
| expr LT expr     { Primop("<", $1, $3) }
| expr GT expr     { Primop(">", $1, $3) }
| expr EQQ expr     { Primop("==", $1, $3) }
| IF expr THEN expr ELSE expr  { If($2, $4, $6) }
| STATALL expr { StatAll($2) }
| FUN IDENT ARROW expr { Fun($2, $4) }
| expr expr            { App($1, $2) }
| TYPEOF expr     { TypeOf($2) }
| LET IDENT EQ expr IN expr { Let($2, $4, $6) }
| BOOL { Bool $1 }
| STRING { String $1 }
| TYPE IDENT EQ constructor_list { TypeDecl($2, $4) }

constructor_list:
  | IDENT { [$1] }
  | constructor_list BAR IDENT { $1 @ [$3] }

type_name:
| ELECTRIC { Electric }
| WATER { Water }
| FIRE { Fire }

move_list:
| STRING { [$1] }
| move_list COMMA STRING { $1 @ [$3] }
| /* empty */ { [] }    (* allow an empty list *)
