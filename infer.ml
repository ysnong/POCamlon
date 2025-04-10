open Ast

(* pretty printer types *)
let rec string_of_typ t =
  match t with
  | TInt -> "type: TInt"
  | TBool -> "type: TBool"
  | TString -> "type: TString"
  | TPoketype -> "type: TPoketype"
  | TPokemon -> "type: TPokemon"
  | TList t1 -> "type: (" ^ string_of_typ t1 ^ ") list"
  | TFun(t1, t2) -> "type: (" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
  | TVar x -> x

exception TypeError of string

type subst = (string * tp) list
let rec apply_subst (s : subst) (t : tp) : tp =
  match t with
  | TInt | TBool | TString | TPokemon | TPoketype -> t
  | TList t1 -> TList (apply_subst s t1)
  | TFun(t1, t2) -> TFun(apply_subst s t1, apply_subst s t2)
  | TVar x ->
      (match List.assoc_opt x s with
       | Some t' -> apply_subst s t'
       | None -> TVar x)

let rec occurs (x : string) (t : tp) : bool =
  match t with
  | TVar y -> x = y
  | TFun(t1, t2) -> occurs x t1 || occurs x t2
  | TList t1 -> occurs x t1
  | _ -> false

let rec unify (t1 : tp) (t2 : tp) : subst =
  match t1, t2 with
  | TInt, TInt
  | TBool, TBool
  | TString, TString
  | TPokemon, TPokemon
  | TPoketype, TPoketype -> []

  | TList a, TList b -> unify a b

  | TFun(a1, a2), TFun(b1, b2) ->
      let s1 = unify a1 b1 in
      let s2 = unify (apply_subst s1 a2) (apply_subst s1 b2) in
      s2 @ s1

  | TVar x, t | t, TVar x ->
      if occurs x t then
        raise (TypeError ("Occurs check failed for " ^ x))
      else
        [(x, t)]

  | _ ->
      raise (TypeError "Cannot unify types")

(* Fresh type variables for polymorphism*)
let counter = ref 0
let fresh_var () =
  let id = "t" ^ string_of_int !counter in
  incr counter; TVar id

module StringMap = Map.Make(String)

type type_env = tp StringMap.t

let rec infer (env : type_env) (e : expr) : tp =
  match e with
  | Int _ -> TInt
  | Bool _ -> TBool
  | String _ -> TString
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
          let _ = unify t1 TInt in
          let _ = unify t2 TInt in
          TInt
      | "<" | ">" | "=" ->
          let _ = unify t1 t2 in
          TBool
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

  | Fun (x, body) ->
    let tv = fresh_var () in
    let env' = StringMap.add x tv env in
    let t_body = infer env' body in
    TFun(tv, t_body)

  | TypeOf _ -> raise (TypeError "TypeOf should not be used in type inference directly")

  | App(e1, e2) ->
    let t1 = infer env e1 in
    let t2 = infer env e2 in
    let t_ret = fresh_var () in
    let s = unify t1 (TFun(t2, t_ret)) in
    apply_subst s t_ret
  (*
  | App (e1, e2) ->
    let t_fun = infer env e1 in
    let t_arg = infer env e2 in
    (* for unification let t_ret = fresh_var () *) 
    match t_fun with
    | TFun (t_param, t_result) ->
        if t_param = t_arg then t_result
        else raise (TypeError "Function argument type mismatch")
    | _ -> raise (TypeError "Attempted to call a non-function") *)