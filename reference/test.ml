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


(* ===================================== Pokebag/Evolution Section ===================================== *)

(* type 'a pocamlbag = 'a pocamlon list


type 'a evolution_tree =
  | Base of 'a pocamlon
  | Evolved of 'a pocamlon * 'a evolution_tree list

let rec count_pocamlon_in_tree (tree : 'a evolution_tree) : int =
  match tree with
  | Base _ -> 1
  | Evolved (_, evolutions) ->
      1 + List.fold_left (fun acc e -> acc + count_pocamlon_in_tree e) 0 evolutions *)

(*
Find the first pocamlon with enough EXP to evolve
*)
(* let rec find_ready (tree : 'a evolution_tree) : 'a pocamlon option =
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
  | Custome f -> f target *)

(* ===================================== Pattern Matching/Inference? ===================================== *)  

(* pocamlon info *)
type 'a pocamlon = {
  name: string;
  hp: int
  exp: int;
} 

type pattern =
  | PVar of string
  | PInt of int
  | PBool of bool
  | PWild                     (* _ *)
  | PStat of stat_field * pattern
  | PName of string           (* match name field *)
  | PAnd of pattern * pattern (* pattern1 & pattern2 *)

type expr =
| Var of string
| Int of int
| Bool of bool
| Fn of string * expr            (* fun x -> expr *)
| Apply of expr * expr           (* f x *)
| Let of string * expr * expr    (* let x = e1 in e2 *)
(* | GetStat of stat_field * expr   p.stats.hp, etc. *)
| ExpOf of expr                  (* get p.exp *)
| Comparison of expr * expr      (* >, =, < *)
(* | Match of expr * (pattern * expr) list *)

type tp =
| TInt
| TBool
| TPocamlon
| TArrow of tp * tp
| TVar of string  (* Type variable for inference *)

type ctx = (string * tp) list


(* let rec infer_pattern (t : tp) (p : pattern) : (string * tp) list =
  match p with
  | PVar x -> [(x, t)]
  | PInt _ -> if t = TInt then [] else failwith "Expected int in pattern"
  | PBool _ -> if t = TBool then [] else failwith "Expected bool in pattern"
  | PWild -> []
  | PName name -> if t = TPokamlon then [] else failwith "Expected pokamlon for name match"
  | PStat (_, subpat) ->
      if t = TPokamlon then infer_pattern TInt subpat
      else failwith "Expected Pokamlon for stat match"
  | PAnd (p1, p2) ->
      infer_pattern t p1 @ infer_pattern t p2 *)

(* let counter = ref 0
let fresh_var () =
  let n = !counter in
  counter := n + 1;
  "'a" ^ string_of_int n *)

let rec infer (ctx : ctx) (e : expr) : tp =
  match e with
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var x -> List.assoc x ctx
  | GetStat (_, _) -> TInt
  | ExpOf _ -> TInt
  | Comparison (e1, e2) ->
      let t1 = infer ctx e1 in
      let t2 = infer ctx e2 in
      if t1 = t2 then TBool else failwith "Type mismatch in comparison"
  | Fn (x, body) ->
      let tx = TVar (fresh_var ()) in
      let t_body = infer ((x, tx) :: ctx) body in
      TArrow (tx, t_body)
  | Apply (e1, e2) ->
      let tf = infer ctx e1 in
      let tx = infer ctx e2 in
      (match tf with
        | TArrow (a, b) when a = tx -> b
        | TArrow (_, _) -> failwith "Function argument mismatch"
        | _ -> failwith "Applied non-function")
  | Let (x, e1, e2) ->
      let t1 = infer ctx e1 in
      infer ((x, t1) :: ctx) e2
  | Match (e, branches) ->
      let t_matched = infer ctx e in
      let rec infer_branches = function
        | [] -> failwith "Empty match"
        | (pat, expr) :: rest ->
            let new_ctx = infer_pattern t_matched pat @ ctx in
            let t_expr = infer new_ctx expr in
            match rest with
            | [] -> t_expr
            | _ ->
                let t_rest = infer_branches rest in
                if t_expr = t_rest then t_expr
                else failwith "Match branches return different types"
      in
      infer_branches branches