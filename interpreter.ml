open Ast

type pokerec = {
  name: string;
  ptype: poketype;
  moves: string list;
  hp: int;
}

type value =
  | VInt of int
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
    let poke = { name; ptype; moves; hp } in
    (env, VPokemon poke)

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
