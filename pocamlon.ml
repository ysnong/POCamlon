(* ===================================== pocamlon INTIALIZATION ===================================== *)  
(* type safety: progress and preservation 
No crashes or undefined behavior if type-checks pass
safety makes sure that errors are caught during compilation

progress: Well-typed programs either evaluate or are already done
ie skills can always be applied if valid or evaluated in steps

preservation: evaluation doesn't break typing
ie Using a skill on a pocamlon gives another well-typed pokamlon

pokebag = stores pocamlon in list
evolution holds tree
*)


(* pocamlon info *)
type info_field = 
| Name
| PType
| Stats
| Nature
| Skills
| Exp
| Evolution

(* pocamlon stats *)
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
| Special (* Depending on their type*)
  

type 'a pocamlon = {
  name: string;
  ptype: string;
  stats: stats;
  nature: 'a nature;
  skills: 'a skill list;
  exp: int;
  evolution: ('a pocamlon -> 'a pocamlon) option; (* Optional Evolution *)
} 

(*
- 'a represents the same type used in the pocamlon's ptype
- skill_name: name of the move
- power: base power of the move
- effect: function that takes a user and a target, returning the modified target
*)
and 'a skill = {
  skill_name: string;
  skill_type: skill_type;
  base_power: int;
  level: int;
  effects: 'a pocamlon -> 'a pocamlon -> 'a pocamlon;
}

(* ===================================== Pokebag/Evolution Section ===================================== *)

type 'a pocamlbag = 'a pocamlon list


type 'a evolution_tree =
  | Base of 'a pocamlon
  | Evolved of 'a pocamlon * 'a evolution_tree list

let rec count_pocamlon_in_tree (tree : 'a evolution_tree) : int =
  match tree with
  | Base _ -> 1
  | Evolved (_, evolutions) ->
      1 + List.fold_left (fun acc e -> acc + count_pocamlon_in_tree e) 0 evolutions

(*
Find the first pocamlon with enough EXP to evolve
*)
let rec find_ready (tree : 'a evolution_tree) : 'a pocamlon option =
  match tree with
  | Base p
  | Evolved (p,[]) -> if p.exp >= 100 then Some p else None
  | Evolved (_, evolutions) -> List.find_map find_ready evolutions

type stat_field = HP | Attack | Defense | SpecialAtk | SpecialDef | Speed

type 'a filter_criteria =
  | NameStartsWith of char
  | HasType of string
  | StatAbove of stat_field * int
  | HasSkill of string
  | ExpAbove of int
  | ExpBelow of int
  (* Nature? Evolution? *)

let get_stat stat_field stats =
  match stat_field with
  | HP -> stats.hp
  | Attack -> stats.attack
  | Defense -> stats.defense
  | SpecialAtk -> stats.special_atk
  | SpecialDef -> stats.special_def
  | Speed -> stats.speed

let filter_pocamlbag (criteria : 'a filter_criteria) (bag : 'a pocamlon list) : 'a pocamlon list =
  match criteria with
  | NameStartsWith c ->
      List.filter (fun p -> String.length p.name > 0 && p.name.[0] = c) bag
  | HasType target_type -> (* type is string *)
      List.filter (fun p -> p.ptype = target_type) bag
  | StatAbove (field, threshold) ->
      List.filter (fun p -> get_stat field p.stats > threshold) bag
  | HasSkill skill_name ->
      List.filter (fun p ->
        List.exists (fun s -> s.skill_name = skill_name) p.skills
      ) bag
  | ExpAbove min_exp ->
      List.filter (fun p -> p.exp > min_exp) bag
  | ExpBelow max_exp ->
      List.filter (fun p -> p.exp < max_exp) bag

(* ===================================== pocamlon Section ===================================== *)

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
Helper function: Convert a single skill into a readable string
*)
let string_of_skill (s : 'a skill) : string =
  Printf.sprintf "%s (Lv.%d, %s, Power: %d)"
    s.skill_name
    s.level
    (match s.skill_type with Physical -> "Physical" | Special -> "Special")
    s.base_power
  
(*
Helper function: Convert a list of skills into a readable string
*)
let string_of_skills skills =
  match skills with
  | [] -> "No skills"
  | _ ->
    skills
    |> List.map string_of_skill
    |> String.concat ", "
(*
Get a specific piece of information from a pocamlon.
Since we don't know how to convert the polymorphic ptype ('a) into a string,
we use a placeholder for now.
*)
let get_pocamlon_info (p : 'a pocamlon) (field : info_field) : string =
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


let gain_exp (amount : int) (p : 'a pocamlon) : 'a pocamlon =
  { p with exp = p.exp + amount }

let evolve_if_ready (p : 'a pocamlon) : 'a pocamlon =
  match p.evolution with
  | Some evolve when p.exp >= 100 -> evolve p  (* can change threshold *)
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