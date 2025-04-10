open Ast

let eval_op op v1 v2 =
  match op, v1, v2 with
  | "<", VInt n1, VInt n2 -> VBool (n1 < n2)
  | ">", VInt n1, VInt n2 -> VBool (n1 > n2)
  | "==", VInt n1, VInt n2 -> VBool (n1 = n2)
  | "+", VInt n1, VInt n2 -> VInt (n1 + n2)
  | "-", VInt n1, VInt n2 -> VInt (n1 - n2)
  | "*", VInt n1, VInt n2 -> VInt (n1 * n2)
  | _ -> failwith ("Unsupported operator or operand types: " ^ op)