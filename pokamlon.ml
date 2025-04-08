(* Type of info fields we can query from a pokamlon *)
type info_field = 
| Name
| PType
| Stats
| Nature

(* Stats that define a pokamlon's battle performance *)
type stats = {
  hp: int;
  attack: int;
  defense: int;
  special_atk: int;
  special_def: int;
  speed: int;
}

(*
  A recursive, polymorphic nature type.
  - Base holds one value of type 'a (e.g., a concrete nature like Timid).
  - Nested wraps around another nature, allowing stacking/multiple effects.
  - Nil represents absence of nature.
*)

type 'a nature =
| Nil
| Base of 'a
| Nested of 'a nature


(* Type of skill: Physical uses attack, Special uses special_atk *)
type skill_type =
| Physical
| Special
  

(*
  A pokamlon is a parametric type, where:
  - 'a can be any type for ptype (e.g., pokamlon_type, string, etc.)
  - nature is recursive and can stack or nest
*)
type 'a pokamlon = {
  name: string;
  ptype: 'a;
  stats: stats;
  nature: 'a nature;
  skills: 'a skill list;
  exp: int;
  evolution: ('a pokamlon -> 'a pokamlon) option; (* Optional Evolution *)
} 
(*
A polymorphic skill type:
- 'a represents the same type used in the pokamlon's ptype
- skill_name: name of the move
- power: base power of the move
- effect: function that takes a user and a target, returning the modified target
*)
and 'a skill = {
  skill_name: string;
  skill_type: skill_type;
  base_power: int;
  level: int;
  effects: 'a pokamlon -> 'a pokamlon -> 'a pokamlon;
}

(* ===================================== Pokamlon Section ===================================== *)

(* 
Helper function: Convert stats into a readable string 
*)
let string_of_stats (s : stats) =
  Printf.sprintf "HP: %d, Atk: %d, Def: %d, SpAtk: %d, SpDef: %d, Speed: %d"
    s.hp s.attack s.defense s.special_atk s.special_def s.speed

(*
Helper function: Recursively convert a nature into a readable string
*)  
let rec string_of_nature (n : 'a nature) : string =
  match n with
  | Nil -> "Nil"
  | Base _ -> "Base"
  | Nested inner -> "Nested(" ^ string_of_nature inner ^ ")"

(*
Get a specific piece of information from a pokamlon.
Since we don't know how to convert the polymorphic ptype ('a) into a string,
we use a placeholder for now.
*)
let get_pokamlon_info (p : 'a pokamlon) (field : info_field) : string =
  match field with
  | Name -> p.name
  | PType -> "<ptype value>"  (* can't convert 'a to string safely *)
  | Stats -> string_of_stats p.stats
  | Nature -> string_of_nature p.nature

let gain_exp (amount : int) (p : 'a pokamlon) : 'a pokamlon =
  { p with exp = p.exp + amount }

let evolve_if_ready (p : 'a pokamlon) : 'a pokamlon =
  match p.evolution with
  | Some evolve when p.exp >= 100 -> evolve p  (* Example threshold *)
  | _ -> p

(* ===================================== Event Section ===================================== *)
type 'a event=
| Damage of int
| Status of string
| Custome of 'a

(* Predefined events example *)
let tackle : string event = Damage 40
let paralysis : string event = Status "Paralyzed"

let handle_event event target = 
  match event with
  | Damage n -> 
    { target with stats = { target.stats with hp = target.stats.hp - n}}
  | Status s ->
    print_endline (target.name ^ "is now" ^ s);
    target
  | Custome f -> f target

(* ===================================== Inventory Section ===================================== *)  
(* Inventory with items, potions, pokemon *)


(* ===================================== Skill selection matchup ===================================== *)  
(* efficient pattern matching by compiling to decision trees *)