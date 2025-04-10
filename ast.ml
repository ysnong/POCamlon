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

type tp =
  | TInt of int
  | TString of string
  | TBool of bool
  | TPoketype of poketype
  | TList of tp list
  | TPokemon of pokerec
  | TConstr of string * value list

type env = (string * tp) list
  
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
  | TypeDef of (string * (string * expr list) list) * expr
  | Constr of string * expr list