(* ast.ml *)

type poketype = 
  | Electric 
  | Water
  | Fire
  (* etc. *)


type pokerec = {
  name: string;
  ptype: poketype;
  moves: string list;
  hp: int;
  attack: int;
  defense: int;
  special_atk: int;
  special_def: int;
  speed: int;
}

type value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VPoketype of poketype
  | VList of value list
  | VPokemon of pokerec

type env = (string * value) list
  
type expr =
  | Int of int
  | Var of string
  | Bool of bool
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Primop of string * expr * expr
  | PokeMon of string * poketype * string list * int
  | FieldAccess of expr * string 
  | Battle of expr * expr
  | Print of expr
  | StatAll of expr
