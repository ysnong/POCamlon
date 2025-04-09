(* user defined typed
tree case check
generalization

normal presentation time, and normal length of report, but a bit more in the coding (aka tree case check) *)

type pokamlon_type =
  | Normal
  | Fighting
  | Flying
  | Poison
  | Ground
  | Rock
  | Bug
  | Ghost
  | Steel
  | Fire
  | Water
  | Grass
  | Electric
  | Psychic
  | Ice
  | Dragon
  | Dark
  | Fairy

type stats = {
  hp: int;
  attack: int;
  defense: int;
  special_atk: int;
  special_def: int;
  speed: int;
}

type pokamlon = {
  name: string;
  ptype: pokamlon_type;
  stats: stats;
}

type nature =
  | Hardy
  | Lonely
  | Adamant
  | Naughty
  | Brave
  | Bold
  | Docile
  | Impish
  | Lax
  | Relaxed
  | Modest
  | Mild
  | Bashful
  | Rash
  | Quiet
  | Calm
  | Gentle
  | Careful
  | Quirky
  | Sassy
  | Timid
  | Hasty
  | Jolly
  | Naive
  | Serious


(* User can use to define Pokedex or backpack or evol map*)
(* Sample N-ary tree *)
(* let my_tree = 
  Node ("root", [
    Node ("home", [
      Node ("user", [
        Leaf 2000;
        Node ("docs", [NLeaf 150; NLeaf 2500])
      ]);
      Node ("guest", [NLeaf 500])
    ]);
    Node ("var", [NLeaf 1000])
  ]) *)
type 'a tree = 
  | Leaf of 'b
  | Node of 'a * 'a tree list

type 'a my_list =
| Nil
| Cons of 'a * 'a my_list


let rec tree_length tree =
  match tree with
  | Leaf _ -> 1
  | Node (_, children) -> 
      1 + List.fold_left (fun acc child -> acc + tree_length child) 0 children

(* evol = find in pokedex or evol-map for next form of evolution*)

(* swap pokemon = subsitute *)

(* improper execution of battle order = exception *)
exception BadBattleOrder of string 


(* order = exp *)
type order = 
    | Attack
    | ETC.


(* battle = eval  *)
(* let rec battle (some pokemon list) (battle order)=  *)









