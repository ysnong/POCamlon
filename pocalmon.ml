(* ===================================== pocalmon INTIALIZATION ===================================== *)  
(* type safety: progress and preservation 
No crashes or undefined behavior if type-checks pass
safety makes sure that errors are caught during compilation

progress: Well-typed programs either evaluate or are already done
ie skills can always be applied if valid or evaluated in steps

preservation: evaluation doesn't break typing
ie Using a skill on a pocalmon gives another well-typed pokamlon

pokebag = stores pocalmon in list
evolution holds tree
*)


(* Pocalmon info *)
type info_field = 
| Name
| PType
| Stats
| Nature
| Skills
| Exp
| Evolution

(* Pocalmon stats *)
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
  A pocalmon is a parametric type, where:
  - 'a can be any type for ptype (e.g., pocalmon_type, string, etc.)
  - nature is recursive and can stack or nest
*)
type 'a pocalmon = {
  name: string;
  ptype: 'a;
  stats: stats;
  nature: 'a nature;
  skills: 'a skill list;
  exp: int;
  evolution: ('a pocalmon -> 'a pocalmon) option; (* Optional Evolution *)
} 

(*
- 'a represents the same type used in the pocalmon's ptype
- skill_name: name of the move
- power: base power of the move
- effect: function that takes a user and a target, returning the modified target
*)
and 'a skill = {
  skill_name: string;
  skill_type: skill_type;
  base_power: int;
  level: int;
  effects: 'a pocalmon -> 'a pocalmon -> 'a pocalmon;
}

(* ===================================== Pokebag/Evolution Section ===================================== *)

type 'a pocalbag = 'a pocalmon list

type 'a evolution_tree =
  | Base of 'a pocalmon
  | Evolved of 'a pocalmon * 'a evolution_tree list

let rec count_pocalmon_in_tree (tree : 'a evolution_tree) : int =
  match tree with
  | Base _ -> 1
  | Evolved (_, evolutions) ->
      1 + List.fold_left (fun acc e -> acc + count_pocalmon_in_tree e) 0 evolutions

(* ===================================== pocalmon Section ===================================== *)

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

let string_of_skill (s : 'a skill) : string =
  Printf.sprintf "%s (Lv.%d, %s, Power: %d)"
    s.skill_name
    s.level
    (match s.skill_type with Physical -> "Physical" | Special -> "Special")
    s.base_power

let string_of_skills skills =
  match skills with
  | [] -> "No skills"
  | _ ->
    skills
    |> List.map string_of_skill
    |> String.concat ", "

(*
Get a specific piece of information from a pocalmon.
Since we don't know how to convert the polymorphic ptype ('a) into a string,
we use a placeholder for now.
*)
let get_pocalmon_info (p : 'a pocalmon) (field : info_field) : string =
  match field with
  | Name -> p.name
  | PType -> "<ptype value>"  (* can't convert 'a to string safely *)
  | Stats -> string_of_stats p.stats
  | Nature -> string_of_nature p.nature
  | Skills -> string_of_skills p.skills
  | Exp -> string_of_int p.exp
  | Evolution -> (match p.evolution with
    | None -> "No evolution"
    | Some _ -> "Can evolve")



let gain_exp (amount : int) (p : 'a pocalmon) : 'a pocalmon =
  { p with exp = p.exp + amount }

let evolve_if_ready (p : 'a pocalmon) : 'a pocalmon =
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


(* ===================================== Skill selection matchup ===================================== *)  
(* efficient pattern matching by compiling to decision trees? *)