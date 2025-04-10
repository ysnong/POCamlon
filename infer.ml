open Ast

let rec string_of_typ t =
  match t with
  | TInt -> "type: TInt"
  | TBool -> "type: TBool"
  | TString -> "type: TString"
  | TPokemon -> "type: TPokemon"
  | TList t1 -> "type : (" ^ string_of_typ t1 ^ ") list"
  (* | TFun(t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")" *)
  | TVar x -> x

(* below is chatgpt generated, probably dont make sense but give u an idea *)
exception TypeError of string

let counter = ref 0
let fresh_var () =
  let id = "t" ^ string_of_int !counter in
  incr counter; TVar id

module StringMap = Map.Make(String)

type type_env = typ StringMap.t

let rec infer (env : type_env) (e : expr) : typ =
  match e with
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var x ->
    (match StringMap.find_opt x env with
     | Some t -> t
     | None -> raise (TypeError ("Unbound variable: " ^ x)))

  | If(cond, e1, e2) ->
    let t_cond = infer env cond in
    if t_cond <> TBool then raise (TypeError "Condition must be boolean");
    let t1 = infer env e1 in
    let t2 = infer env e2 in
    if t1 = t2 then t1
    else raise (TypeError "If branches have mismatched types")

  | Primop(op, e1, e2) ->
    let t1 = infer env e1 in
    let t2 = infer env e2 in
    (match op with
     | "+" | "-" | "*" | "/" ->
         if t1 = TInt && t2 = TInt then TInt
         else raise (TypeError "Arithmetic ops require integers")
     | "<" | ">" | "=" ->
         if t1 = t2 then TBool
         else raise (TypeError "Comparison types must match")
     | _ -> raise (TypeError ("Unknown operator: " ^ op)))

  | Let(x, rhs, body) ->
    let t_rhs = infer env rhs in
    let env' = StringMap.add x t_rhs env in
    infer env' body

  | PokeMon(_, _, _, _) ->
    TPokemon

  | FieldAccess(e1, field) ->
    let t = infer env e1 in
    if t <> TPokemon then raise (TypeError "Field access only valid on Pokémon");
    (match field with
     | "hp" | "attack" | "defense"
     | "special_atk" | "special_def" | "speed" -> TInt
     | "name" -> TString
     | "ptype" -> TPoketype
     | "moves" -> TList TString
     | _ -> raise (TypeError ("Unknown field: " ^ field)))

  | Battle(e1, e2) ->
    let t1 = infer env e1 in
    let t2 = infer env e2 in
    if t1 = TPokemon && t2 = TPokemon then TPokemon
    else raise (TypeError "Both battle operands must be Pokémon")

  | Print e1 ->
    let t = infer env e1 in
    (match t with
     | TInt | TString | TBool -> t
     | _ -> raise (TypeError "Can only print int, string, or bool"))

  | StatAll e1 ->
    let t = infer env e1 in
    if t = TPokemon then TInt
    else raise (TypeError "StatAll expects a Pokémon")
