open Ast
open Eval_op

let rec eval ?(type_env=[]) (env: env) (e: expr) : (env * value) =
  match e with

  | Int n ->
    (env, TInt n)
  | Bool b ->
    (env, TBool b)
  | Var x ->
    (match List.assoc_opt x env with
     | Some v -> (env, v)
     | None -> failwith ("Unbound variable: " ^ x))

  | If(cond, e1, e2) ->
    let (_, v_cond) = eval env cond in
    (match v_cond with
     | TBool true -> eval env e1
     | TBool false-> eval env e2  
     | _ -> failwith "Condition is not TBool")

  | Primop(op, e1, e2) ->
    let (_, v1) = eval env e1 in
    let (_, v2) = eval env e2 in
    (env, eval_op op v1 v2)

  | Let(x, rhs, _) ->
    let (env', v) = eval env rhs in
    ((x, v) :: env', v)

  | PokeMon(name, ptype, moves, hp) ->
    let poke = {name; ptype; moves; hp; 
      attack = 55;
      defense = 40;
      special_atk = 50;
      special_def = 50;
      speed = 90;
    } in
    (env, TPokemon poke)

  | FieldAccess (e, field) ->
    let (env', v) = eval env e in  
    (match v with
      | TPokemon p ->  
          (match field with
          | "hp" -> (env', TInt p.hp)
          | "attack" -> (env', TInt p.attack)  
          | "defense" -> (env', TInt p.defense)  
          | "special_atk" -> (env', TInt p.special_atk)  
          | "special_def" -> (env', TInt p.special_def) 
          | "speed" -> (env', TInt p.speed)
          | "name" -> (env', TString p.name)  
          | "ptype" -> (env', TPoketype p.ptype)  
          | "moves" -> (env', TList (List.map (fun m -> TString m) p.moves)) 
          | _ -> failwith ("Unknown field: " ^ field))  
      | _ -> failwith "Field access on non-Pokemon tp") 

  | Battle(e1, e2) ->
    let (env', v1) = eval env e1 in
    let (env'', v2) = eval env' e2 in
    (match e1, e2, v1, v2 with
      | Var _, Var name2, TPokemon p1, TPokemon p2 ->
          let p2_hp_after = max 0 (p2.hp - 55) in
          Printf.printf "%s uses %s! %s's HP: %d.\n"
            p1.name
            (match p1.moves with | m::_ -> m | [] -> "Struggle")
            p2.name p2_hp_after;
          let new_p2 = { p2 with hp = p2_hp_after } in
          let updated_env = (name2, TPokemon new_p2) :: List.remove_assoc name2 env'' in
          (updated_env, TPokemon new_p2)
      | _ -> failwith "battle requires two Pokémon.")
  | Print e ->  (* Handling Print expression *)
    let (_, v) = eval env e in  (* Evaluate the expression inside Print *)
    (match v with
    | TInt n -> Printf.printf "%d\n" n; (env, TInt n)  (* Print integer *)
    | TString s -> Printf.printf "%s\n" s; (env, TString s)  (* Print string *)
    | TBool b -> Printf.printf "%s\n" (string_of_bool b); (env, TBool b)
    | TConstr (name, vals) ->
      let contents = List.map (function
        | TInt n -> string_of_int n
        | TString s -> "\"" ^ s ^ "\""
        | _ -> "..."
      ) vals |> String.concat ", " in
      Printf.printf "%s(%s)\n" name contents;
      (env, TConstr (name, vals))
    | _ -> failwith "Print expects an integer or string.")  (* Error for other types *)
  
  | StatAll e1 ->
  let (_, v) = eval env e1 in
  (match v with
    | TPokemon p ->
      Printf.printf
        "%s's stats:\nHP: %d, Atk: %d, Def: %d, SpAtk: %d, SpDef: %d, Speed: %d\n"
        p.name p.hp p.attack p.defense
        p.special_atk p.special_def p.speed;
      (env, TInt 0)
    | _ -> failwith "StatAll expects a Pokémon")

  | TypeDef ((name, variants), body) ->
    let new_type_env = (name, variants) :: type_env in
    eval ~type_env:new_type_env env body 

  | Constr (name, args) ->
    let values = List.map (fun e -> snd (eval ~type_env env e)) args in
    (env, VConstr (name, values))