type value =
  | VInt of int

let rec eval (env : (string * value) list) (e : Ast.expr) : value =
  match e with
  | Ast.Int n -> VInt n
  | Ast.Var x -> List.assoc x env
  | Ast.Binop(op, e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match op, v1, v2 with
       | Ast.Add, VInt a, VInt b -> VInt (a + b)
       | Ast.Mult, VInt a, VInt b -> VInt (a * b))
  | Ast.Let(x, e1, e2) ->
      let v1 = eval env e1 in
      eval ((x, v1) :: env) e2
  | _ -> failwith "Not implemented yet (Pokémon features coming soon!)"