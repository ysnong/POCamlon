open Ast
open Eval_op

let string_of_value t =
  match t with
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | VString s -> s
  | VFun _ -> "<function>"
  | VPokemon p -> p.name
  | VList _ -> "haha"
  | VPoketype _ -> "<type>"
let type_env = ref Infer.StringMap.empty

let rec eval (env: env) (e: expr) : (env * value) =
  match e with

  | Int n ->
    (env, VInt n)
  | Bool b ->
    (env, VBool b)
  | Var x ->
    (match List.assoc_opt x env with
     | Some v -> (env, v)
     | None -> failwith ("Unbound variable: " ^ x))
  | String s ->
  (env, VString s)

  | If(cond, e1, e2) ->
    let (_, v_cond) = eval env cond in
    (match v_cond with
     | VBool true -> eval env e1
     | VBool false-> eval env e2  
     | _ -> failwith "Condition is not VBool")

  | Primop(op, e1, e2) ->
    let (_, v1) = eval env e1 in
    let (_, v2) = eval env e2 in
    (env, eval_op op v1 v2)

  | Let(x, rhs, body) ->
    let t_rhs = Infer.infer !type_env rhs in
    type_env := Infer.StringMap.add x t_rhs !type_env;
    let (env_rhs, v_rhs) = eval env rhs in
    let env' = (x, v_rhs) :: env_rhs in
    eval env' body

  | PokeMon(name, ptype, moves, hp) ->
    let poke = {name; ptype; moves; hp; 
      attack = 55;
      defense = 40;
      special_atk = 50;
      special_def = 50;
      speed = 90;
    } in
    (env, VPokemon poke)

  | FieldAccess (e, field) ->
    let (env', v) = eval env e in  
    (match v with
      | VPokemon p ->  
          (match field with
          | "hp" -> (env', VInt p.hp)
          | "attack" -> (env', VInt p.attack)  
          | "defense" -> (env', VInt p.defense)  
          | "special_atk" -> (env', VInt p.special_atk)  
          | "special_def" -> (env', VInt p.special_def) 
          | "speed" -> (env', VInt p.speed)
          | "name" -> (env', VString p.name)  
          | "ptype" -> (env', VPoketype p.ptype)  
          | "moves" -> (env', VList (List.map (fun m -> VString m) p.moves)) 
          | _ -> failwith ("Unknown field: " ^ field))  
      | _ -> failwith "Field access on non-Pokemon value") 

  | Battle(e1, e2) ->
    let (env', v1) = eval env e1 in
    let (env'', v2) = eval env' e2 in
    (match e1, e2, v1, v2 with
      | Var _, Var name2, VPokemon p1, VPokemon p2 ->
          let p2_hp_after = max 0 (p2.hp - 55) in
          Printf.printf "%s uses %s! %s's HP: %d.\n"
            p1.name
            (match p1.moves with | m::_ -> m | [] -> "Struggle")
            p2.name p2_hp_after;
          let new_p2 = { p2 with hp = p2_hp_after } in
          let updated_env = (name2, VPokemon new_p2) :: List.remove_assoc name2 env'' in
          (updated_env, VPokemon new_p2)
      | _ -> failwith "battle requires two Pokémon.")
      
  | Print e ->  (* Handling Print expression *)
    let (_, v) = eval env e in  (* Evaluate the expression inside Print *)
    (match v with
    | VInt n -> Printf.printf "%d\n" n; (env, VInt n)  (* Print integer *)
    | VString s -> Printf.printf "%s\n" s; (env, VString s)  (* Print string *)
    | VBool b -> Printf.printf "%s\n" (string_of_bool b); (env, VBool b)
    | _ -> failwith "Print expects an integer or string.")  (* Error for other types *)
  
  | StatAll e1 ->
  let (_, v) = eval env e1 in
  (match v with
    | VPokemon p ->
      Printf.printf
        "%s's stats:\nHP: %d, Atk: %d, Def: %d, SpAtk: %d, SpDef: %d, Speed: %d\n"
        p.name p.hp p.attack p.defense
        p.special_atk p.special_def p.speed;
      (env, VInt 0)
    | _ -> failwith "StatAll expects a Pokémon")

  | Fun(param, body) ->
    (env, VFun(param, body, env))
  
  | App(e1, e2) ->
    let (_, v1) = eval env e1 in
    let (_, v2) = eval env e2 in
    (match v1 with
     | VFun(param, body, closure_env) ->
         let new_env = (param, v2) :: closure_env in
         let (_, result) = eval new_env body in
         (env, result)
     | _ -> failwith "Trying to apply non-function")

  | TypeOf e ->
    let t = Infer.infer !type_env e in
    Printf.printf "[DEBUG] Type inferred: %s\n" (Infer.string_of_typ t);
    (env, VString (Infer.string_of_typ t))
