open Ast

type stats = {
  hp: int;
  attack: int;
  defense: int;
  special_atk: int;
  special_def: int;
  speed: int;
}

type pokerec = {
  name: string;
  ptype: poketype;
  moves: string list;
  stats: stats;
}

type value =
  | VInt of int
  | VString of string
  | VPoketype of poketype
  | VList of value list
  | VPokemon of pokerec

type env = (string * value) list

let rec eval (env: env) (e: expr) : (env * value) =
  match e with

  | Int n ->
    (env, VInt n)

  | Var x ->
    (match List.assoc_opt x env with
     | Some v -> (env, v)
     | None -> failwith ("Unbound variable: " ^ x))

  | Let(x, rhs, _) ->
    let (env', v) = eval env rhs in
    ((x, v) :: env', v)

  | PokeMon(name, ptype, moves, hp) ->
    let poke = { name;
      ptype;
      moves;
      stats = {
        hp;
        attack = 55;
        defense = 40;
        special_atk = 50;
        special_def = 50;
        speed = 90; }} in
    (env, VPokemon poke)

  | FieldAccess (e, field) ->
    let (env', v) = eval env e in  (* Evaluate the left-hand side expression (e.g., pikachu) *)
    (match v with
      | VPokemon p ->  (* Ensure the value is a VPokemon *)
          (match field with
          (*| "hp" -> (env', VInt p.hp)  (* Access the hp field *)*)
          | "name" -> (env', VString p.name)  (* Access the name field *)
          | "ptype" -> (env', VPoketype p.ptype)  (* Convert ptype to string *)
          | "moves" -> (env', VList (List.map (fun m -> VString m) p.moves))  (* Convert moves list to VStrings *)
          | _ -> failwith ("Unknown field: " ^ field))  (* Handle unknown fields *)
      | _ -> failwith "Field access on non-Pokemon value")  (* Error if the value is not a VPokemon *)

  | Battle(e1, e2) ->
    let (env', v1) = eval env e1 in
    let (env'', v2) = eval env' e2 in
    (match v1, v2 with
     | VPokemon p1, VPokemon p2 ->
       let p2_hp_after = p2.stats.hp - 55 (* a pretend damage formula *)
       in
         Printf.printf "%s uses %s! %s's HP: %d.\n"
           p1.name
           (match p1.moves with
            | m::_ -> m
            | [] -> "Struggle")
           p2.name
           p2_hp_after;
         let new_p2 = {p2 with stats = { p2.stats with hp = p2_hp_after }} in
         (env'', VPokemon new_p2)
     | _ ->
       failwith "battle requires two Pokémon.")
  | Print e ->  (* Handling Print expression *)
    let (_, v) = eval env e in  (* Evaluate the expression inside Print *)
    (match v with
    | VInt n -> Printf.printf "%d\n" n; (env, VInt n)  (* Print integer *)
    | VString s -> Printf.printf "%s\n" s; (env, VString s)  (* Print string *)
    | _ -> failwith "Print expects an integer or string.")  (* Error for other types *)
  
  | StatAll e1 ->
  let (_, v) = eval env e1 in
  (match v with
    | VPokemon p ->
      Printf.printf
        "%s's stats:\nHP: %d, Atk: %d, Def: %d, SpAtk: %d, SpDef: %d, Speed: %d\n"
        p.name p.stats.hp p.stats.attack p.stats.defense
        p.stats.special_atk p.stats.special_def p.stats.speed;
      (env, VInt 0)
    | _ -> failwith "StatAll expects a Pokémon")

  | StatField (e1, field) ->
    let (_, v) = eval env e1 in
    (match v with
      | VPokemon p ->
        let stat_val =
          match field with
          | HP -> p.stats.hp
          | Attack -> p.stats.attack
          | Defense -> p.stats.defense
          | SpecialAtk -> p.stats.special_atk
          | SpecialDef -> p.stats.special_def
          | Speed -> p.stats.speed
        in
        Printf.printf "%s's %s: %d\n"
          p.name
          (match field with
            | HP -> "HP" | Attack -> "Attack" | Defense -> "Defense"
            | SpecialAtk -> "SpecialAtk" | SpecialDef -> "SpecialDef" | Speed -> "Speed")
          stat_val;
        (env, VInt stat_val)
      | _ -> failwith "StatField expects a Pokémon")