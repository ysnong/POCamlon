
(* The type of tokens. *)

type token = 
  | WATER
  | STRING of (string)
  | RPAREN
  | RBRACKET
  | POKEMON
  | LPAREN
  | LET
  | LBRACKET
  | INT of (int)
  | IDENT of (string)
  | FIRE
  | EQ
  | EOF
  | ELECTRIC
  | COMMA
  | BATTLE

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)
