type stats = {
  hp: int;
  attack: int;
  defense: int;
  special_atk: int;
  special_def: int;
  speed: int;
}

type 'a nature =
| Nil
| Base of 'a
| Nested of 'a nature

(* Parametric polymorphism *)
type 'a pokamlon = {
  name: string;
  ptype: 'a;
  stats: stats;
  nature: 'a nature;
}

(* get the name of the pokamlon *)
let get_name (p : 'a pokamlon) : string = p.name;

type 'a event=
| Damage of int
| Status of string
| Custome of 'a

(* Predefined events example *)
let tackle : string event = Damage 40
let paralysis : string event = Status "Paralyzed"

(* Custom event with custom payload *)
let burn_effect : ('a pokamlon -> 'a pokamlon) event =
  Custome (fun p -> { p with stats = {p.stats with hp = p.stats.hp - 10}})

let handle_event event target = 
  match event with
  | Damage n -> 
    { target with stats = { target.stats with hp = target.stats.hp - n}}
  | Status s ->
    print_endline (target.name ^ "is now" ^ s);
    target
  | Custome f -> f target
