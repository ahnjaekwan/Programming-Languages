(* Name: Jaekwan Ahn

   UID: 604057669

   Others With Whom I Discussed Things: None

   Other Resources I Consulted: Discussion Slides and Piazza Posts
   
*)

(* EXCEPTIONS *)

(* This is a marker for places in the code that you have to fill in.
   Your completed assignment should never raise this exception. *)
exception ImplementMe of string

(* This exception is thrown when a type error occurs during evaluation
   (e.g., attempting to invoke something that's not a function).
   You should provide a useful error message.
*)
exception DynamicTypeError of string

(* This exception is thrown when pattern matching fails during evaluation. *)  
exception MatchFailure  

(* EVALUATION *)

(* See if a value matches a given pattern.  If there is a match, return
   an environment for any name bindings in the pattern.  If there is not
   a match, raise the MatchFailure exception.
*)

let rec patMatch (pat:mopat) (value:movalue) : moenv =
  match (pat, value) with
      (* an integer pattern matches an integer only when they are the same constant;
	 no variables are declared in the pattern so the returned environment is empty *)
      (IntPat(i), IntVal(j)) when i=j -> Env.empty_env()
    | (BoolPat(b1), BoolVal(b2)) when b1=b2 -> Env.empty_env()
    | (WildcardPat, _) -> Env.empty_env()
    | (VarPat(s), var) -> Env.add_binding s var (Env.empty_env())
    | (TuplePat(t), TupleVal(l)) -> 
        (
          match (t,l) with
              ([],[]) -> (Env.empty_env())
            | ([],_) -> raise MatchFailure
            | (_,[]) -> raise MatchFailure 
            | (hd1::rest1,hd2::rest2) -> Env.combine_envs (patMatch hd1 hd2) (patMatch (TuplePat(rest1)) (TupleVal(rest2)))
        )
    | (DataPat(d,o1), DataVal(s,o2)) when d=s -> 
        (
          match (o1,o2) with
              (None,None) -> Env.empty_env()
            | (None, _) -> raise MatchFailure
            | (_, None) -> raise MatchFailure
            | (Some p1,Some p2) -> patMatch p1 p2  
        )
    | _ -> raise MatchFailure 

    
(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)
let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
      (* an integer constant evaluates to itself *)
      IntConst(i) -> IntVal(i)
    | BoolConst(b) -> BoolVal(b)
    | Var(s) -> ( try Env.lookup s env
                  with Env.NotBound -> raise (DynamicTypeError "No such variable in environment")
                )
    | BinOp(e1,op,e2) -> 
        ( let a1 = evalExpr e1 env in
          let a2 = evalExpr e2 env in
          match (a1,a2) with 
              (IntVal(i1),IntVal(i2)) ->
                (match op with
                    Plus -> IntVal(i1 + i2)
                  | Minus -> IntVal(i1 - i2)
                  | Times -> IntVal(i1 * i2)
                  | Eq -> BoolVal(i1 = i2)
                  | Gt -> BoolVal(i1 > i2)
                )
            | _ -> raise (DynamicTypeError "The Binary operators apply only to integers")
        )
    | Negate(e1) -> 
        ( let a1 = evalExpr e1 env in
          match a1 with
              IntVal(i) -> let ans = (-1) * i in IntVal(ans)
            | _ -> raise (DynamicTypeError "The Negate operator apply only to integers")
        )
    | If(e1,e2,e3) -> 
        ( let a1 = evalExpr e1 env in
          match a1 with
              BoolVal(b) -> if b then evalExpr e2 env else evalExpr e3 env
            | _ -> raise (DynamicTypeError "The If operator apply only to (bool,moexpr,moexpr)")
        )
        
    | Function(p,e1) -> FunctionVal(None, p, e1, env)
    | FunctionCall(e1,e2) -> 
        ( let a1 = evalExpr e1 env in
          let a2 = evalExpr e2 env in 
          match a1 with
              FunctionVal(s,fp,fe,fenv) ->
                ( let pm = patMatch fp a2 in
                  match s with
                      None -> let fenv1 = Env.combine_envs fenv pm in
                              evalExpr fe fenv1
                    | Some(var) ->  let fenv1 = Env.add_binding var a1 fenv in
                                    let fenv2 = Env.combine_envs fenv1 pm in
                                    evalExpr fe fenv2
                )
            | _ -> raise (DynamicTypeError "The FunctionCall operator apply only to (moexpr,moexpr)")
        )
    | Match(e1,l) ->
      (
        match l with
            [] -> raise MatchFailure
          | (p,e2)::rest -> ( try let ans = evalExpr e1 env in
                                  let env1 = patMatch p ans in
                                  evalExpr e2 (Env.combine_envs env env1)
                              with MatchFailure -> evalExpr (Match(e1,rest)) env
                            )
      )
    | Tuple(l) -> 
        (
          match l with
              [] -> TupleVal([])
            | hd::rest -> let ans1 = evalExpr hd env in
                          let ans2 = evalExpr (Tuple(rest)) env in
                          ( match ans2 with
                                TupleVal(ans3) -> TupleVal(ans1::ans3)
                              | _ -> raise (DynamicTypeError "The Tuple operator apply only to moexpr list")
                          ) 
        )
    | Data(s,e1) ->
        (
          match e1 with
              None -> DataVal(s,None)
            | Some e2 -> DataVal(s, Some (evalExpr e2 env))
        )

(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moresult =
  match d with
      (* a top-level expression has no name and is evaluated to a value *)
      Expr(e) -> (None, evalExpr e env)
    | Let(s,e) -> (Some s, evalExpr e env)
    | LetRec(s,e) -> 
        (
          match e with
              Function(p,e1) -> (Some s, FunctionVal(Some s,p,e1,env))
            | _ -> raise (DynamicTypeError "The Let Rec operator is invalid here")
        )

