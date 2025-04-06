(* TODO: Write a good set of tests for eval. *)
let exp1=Fn (["p"; "q"],
             Var "q")
let exp2=Fn ([],
             B true)
    
    
let exp3=Fn (["x"],
             Var "x")
let exp4=Fn (["x"],
             Primop (Plus,
                     [Primop (Times, [Var "x"; Var "x"]);
                      Primop (Times, [Var "x"; Var "x"])]))
let exp5=Rec("q",Primop (Plus, [I 3; I 0]))
let exp6=(Rec ("v",B true))
         
                          
let e3=Rec("s",Fn([],B true))
let e4=Rec("f", Fn (["x"], 
                    If (Primop (Equals, [Var "x"; I 1]), 
                        I 1, 
                        Primop(Plus,[Var "x";Apply (Var "f", [Primop(Minus, [Var "x"; I 1])])])
                       )))
    
    
let eval_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec and Apply!
  *)
  (Let ("x", I 1, Primop (Plus, [Var "x"; I 5])), I 6);
  
  
  ( Apply (exp1, [I 3; B true]),B true );
  ( Apply (exp2, []),B true );
  ( Apply (exp3, [I 5]),I 5 );
  ( Apply (exp4, [I 2]),I 8 );
  (Let ("q", exp3, exp5), I 3);
  
  (* ( Apply(e2, [I 6;I 3]),I 2);*)
  (Apply(e3, []), B true);
  (Apply(e4, [I 10]), I 55);
  
  (e4,Fn (["x"],
          If (Primop (Equals, [Var "x"; I 1]), I 1,
              Primop (Plus,
                      [Var "x";
                       Apply
                         (Rec ("f",
                               Fn (["x"],
                                   If (Primop (Equals, [Var "x"; I 1]), I 1,
                                       Primop (Plus,
                                               [Var "x"; Apply (Var "f", [Primop (Minus, [Var "x"; I 1])])])))),
                          [Primop (Minus, [Var "x"; I 1])])]))))
  
  
   
  
  
  
]

(* TODO: Implement the missing cases of eval. *)
let rec eval exp =
  match exp with
  (* Values evaluate to themselves *)
  | I _ -> exp
  | B _ -> exp
  | Fn _ -> exp

  (* This evaluator is _not_ environment-based. Variables should never
     appear during evaluation since they should be substituted away when
     eliminating binding constructs, e.g. function applications and lets.
     Therefore, if we encounter a variable, we raise an error.
*)
  | Var x -> raise (Stuck (Free_variable x))

  (* Primitive operations: +, -, *, <, = *)
  | Primop (po, args) ->
      let args = List.map eval args in
      begin
        match eval_op po args with
        | None -> raise (Stuck Bad_primop_args)
        | Some v -> v
      end

  | If (e, e1, e2) ->
      begin
        match eval e with
        | B true -> eval e1
        | B false -> eval e2
        | _ -> raise (Stuck If_non_true_false)
      end

  | Let (x, e1, e2) ->
      let e1 = eval e1 in
      eval (subst (e1, x) e2)

  | Rec (f, e) -> 
      
      eval (subst (Rec(f,e), f) e)

  | Apply (e, es) -> 
                       
      begin match eval e with
        

        | Fn(namel,efn) -> 
            if List.length namel=List.length es then
              let expname=   List.map2  (fun x y -> (x,y)) es namel
              in 
              eval (subst_list expname efn)
            else raise (Stuck Arity_mismatch)
        | _ -> raise (Stuck Apply_non_fn)
      end


(* Part 2: unify *)
(* TODO: Implement missing cases. *)
let rec unify (cs : (tp * tp) list) : tp TVarMap.t =
  (* helper function *)
  let rec occurs (x : name) (tp : tp) : bool =
    match tp with
    | Int | Bool -> false
    | Arrow (ts,b) -> List.exists (occurs x) ts || occurs x b
    | TVar y -> x = y
  in
  match cs with
  | [] -> TVarMap.empty
  | (t1,t2) :: cs when t1 = t2 -> unify cs
  | ((Arrow (ts,t) as t1), (Arrow (ss,s) as t2)) :: cs ->
      raise NotImplemented
  | (TVar a, t) :: cs -> raise NotImplemented
  | (t, TVar a) :: cs -> raise NotImplemented
  | (t1, t2) :: _ -> type_mismatch t1 t2


(* Part 3: infer *)

(* TODO: Write a good set of tests for infer. *)
let infer_tests = [
  (* 
     The infer_tests will NOT have various infer bugs to uncover this time. 
     It's only for the purpose of checking your work.
  *)
  (([("x", Int)], Var "x"), Int)
]

(* set up and helper functions for infer *)
(* list of constraints *)
let constraints = ref [] ;;
(* helper function to add a constraint *)
let constrain t1 t2 = constraints := (t1,t2) :: !constraints ;;
let ctr = ref 0 ;;
(* helper function that generates a new TVar with names like tv1, tv2, ... *)
let freshvar () =
  let n = !ctr in
  ctr := 1 + !ctr;
  TVar ("tv" ^ string_of_int n) ;;


(* TODO: Implement the missing cases for infer *)
let infer (gamma : context) (e : exp) : tp =
  let rec infer_aux (gamma : context) (e : exp) : tp =
    match e with
    | I _ -> Int
    | B _ -> Bool
    | Var x -> begin match lookup x gamma with
        | Some t -> t
        | None -> free_variable x
      end
    | If (e1,e2,e3) ->
        let t1 = infer_aux gamma e1 in
        let t2 = infer_aux gamma e2 in
        let t3 = infer_aux gamma e3 in
        constrain t1 Bool;
        constrain t2 t3;
        t2
    | Primop (op, args) ->
        (* assume the arity matches, since we shouldn't
           be dealing with invalid ASTs by this point... *)
        raise NotImplemented
    | Fn (xs, b) ->
        raise NotImplemented
    | Rec (x, b) ->
        raise NotImplemented
    | Let (x, e, b) ->
        raise NotImplemented
    | Apply (f,args) ->
        raise NotImplemented
  in
  
  let inferred_type = infer_aux gamma e in
  let unify_solution = unify !constraints in
  apply_type_substitution unify_solution inferred_type