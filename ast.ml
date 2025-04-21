(* ast.ml *)

type poketype = 
  | Electric 
  | Water
  | Fire
(* can add more later *)


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
| TInt 
| TString 
| TBool 
| TPoketype 
| TList of tp
| TPokemon
| TVar of string
| TFun of tp * tp

type value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VPoketype of poketype
  | VList of value list
  | VPokemon of pokerec
  | VFun of string * expr * env

and env = (string * value) list
  
and expr =
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
  | Fun of string * expr   
  | App of expr * expr     
  | TypeOf of expr
  | String of string
  | TypeDecl of string * (string list) 
  | Constructor of string