open Ast

let eval_op op v1 v2 =
  match op, v1, v2 with
  | "<", TInt n1, TInt n2 -> TBool (n1 < n2)
  | ">", TInt n1, TInt n2 -> TBool (n1 > n2)
  | "==", TInt n1, TInt n2 -> TBool (n1 = n2)
  | "+", TInt n1, TInt n2 -> TInt (n1 + n2)
  | "-", TInt n1, TInt n2 -> TInt (n1 - n2)
  | "*", TInt n1, TInt n2 -> TInt (n1 * n2)
  | _ -> failwith ("Unsupported operator or operand types: " ^ op)