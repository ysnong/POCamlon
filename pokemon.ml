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