(* ast.ml *)

type poketype = 
  | Electric 
  | Water
  | Fire
  (* etc. *)

type expr =
  | Int of int
  | Var of string
  | Let of string * expr * expr
  | PokeMon of string * poketype * string list * int
  | FieldAccess of expr * string 
  | Battle of expr * expr
  | Print of expr
