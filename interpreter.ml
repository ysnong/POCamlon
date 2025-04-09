open Ast
open Eval_op

type pokerec = {
  name: string;
  ptype: poketype;
  moves: string list;
  hp: int;
}

type value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VPoketype of poketype
  | VList of value list
  | VPokemon of pokerec

type env = (string * value) list

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

  | Let(x, rhs, _) ->
    let (env', v) = eval env rhs in
    ((x, v) :: env', v)

  | PokeMon(name, ptype, moves, hp) ->
    let poke = { name; ptype; moves; hp } in
    (env, VPokemon poke)

  | FieldAccess (e, field) ->
    let (env', v) = eval env e in  (* Evaluate the left-hand side expression (e.g., pikachu) *)
    (match v with
      | VPokemon p ->  (* Ensure the value is a VPokemon *)
          (match field with
          | "hp" -> (env', VInt p.hp)  (* Access the hp field *)
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
       let p2_hp_after = p2.hp - 55 (* a pretend damage formula *)
       in
         Printf.printf "%s uses %s! %s's HP: %d.\n"
           p1.name
           (match p1.moves with
            | m::_ -> m
            | [] -> "Struggle")
           p2.name
           p2_hp_after;
         let new_p2 = {p2 with hp = p2_hp_after} in
         (env'', VPokemon new_p2)
     | _ ->
       failwith "battle requires two Pokémon.")
  | Print e ->  (* Handling Print expression *)
    let (_, v) = eval env e in  (* Evaluate the expression inside Print *)
    (match v with
    | VInt n -> Printf.printf "%d\n" n; (env, VInt n)  (* Print integer *)
    | VString s -> Printf.printf "%s\n" s; (env, VString s)  (* Print string *)
    | VBool b -> Printf.printf "%s\n" s; (env, VString s)
    | _ -> failwith "Print expects an integer or string.")  (* Error for other types *)
