(* Problem 1: Vectors and Matrices *)

(* type aliases for vectors and matrices *)            
type vector = float list                                 
type matrix = vector list

(* Part (a): vplus function *)
let (vplus : vector -> vector -> vector) =
  List.map2 (+.)


(* Part (b): mplus function *)
let (mplus : matrix -> matrix -> matrix) =
  List.map2 vplus


(* Part (c): dotprod function and its helper functions *)
let (sumlist : vector -> float) = 
  List.fold_left (+.) 0.0

let (dotprod_help : vector -> vector -> vector) =
  List.map2 ( *. )

let dotprod (v1 : vector) (v2 : vector) : float =
  let result = dotprod_help v1 v2 in
    sumlist result


(* Part (d): transpose function and its helper functions *)
let appendHelp m1 e1 =
  match m1 with
    [] -> []
  | h::t -> if h = [] then [e1] :: t 
            else
              let initialVectorLength = List.length (List.hd m1) in
              let sameLength l v  = (List.length v) = l in
              let differentLength l v = (List.length v) < l in
              let remain = List.filter (sameLength initialVectorLength) m1 in
              let update = List.filter (differentLength initialVectorLength) m1 in
              match update with
              [] -> (match remain with
                      [] -> m1
                    | newhd::newtail -> 
                        let newVector = newhd @ [e1] in
                          newVector :: newtail) 
              | newhd::newtail ->
                    let newVector = newhd @ [e1] in 
                        remain @ [newVector] @ newtail

let addEmpty e = []

let transpose (m1 : matrix) : matrix =
  let m2 = List.map addEmpty (List.hd m1) in
  let concated = List.concat m1 in
    List.fold_left appendHelp m2 concated


(* Part (e): mmult function and its helper functions *)
let float_to_list e = [e]

let vmult m2 v1 =
  let transposedM2 = transpose m2 in
  let temp = List.map (dotprod v1) transposedM2 in 
  let m3 = List.map float_to_list temp in
  let temp1 = transpose m3 in
    List.flatten temp1

let mmult (m1 : matrix) (m2 : matrix) : matrix =
  List.map (vmult m2) m1

   



(* Problem 2: Calculators *)           
           
(* a type for arithmetic expressions *)
type op = Add | Sub | Mult | Div
type exp = Num of float | BinOp of exp * op * exp

(* Part (a): evalExp function and its helper function *)
let calc op1 firstOp secondOp = 
  match op1 with
        Add -> firstOp +. secondOp
      | Sub -> firstOp -. secondOp
      | Mult -> firstOp *. secondOp
      | Div -> firstOp /. secondOp

let rec evalExp exp1 =
  match exp1 with
    Num(float1) -> float1
  | BinOp(exp2,op1,exp3) -> calc op1 (evalExp exp2) (evalExp exp3)

(* a type for stack instructions *)	  
type instr = Push of float | Swap | Calculate of op

(* Part (b): execute function and its helper functions *)
let execPush f floatlist =
  floatlist @ [f]

let execSwap floatlist =
  let revList = List.rev floatlist in
  match revList with
    [] -> []
  | firstItem::rest ->
    match rest with
      [] -> []
    | secondItem::rest1 ->
        let swapped = [secondItem] @ [firstItem] @ rest1 in
          List.rev swapped

let execCalculate op1 floatlist =
  let revList = List.rev floatlist in
  match revList with
    [] -> []
  | secondOperand::rest ->
    match rest with
      [] -> []
    | firstOperand::rest1 ->
        let answer = calc op1 firstOperand secondOperand in
        let result = answer :: rest1 in
          List.rev result

let popElem floatlist = 
  match floatlist with
    [] -> 0.0
  | _ -> List.nth floatlist ((List.length floatlist) - 1)

let rec execute_help instrlist floatlist = 
  match instrlist with
    [] -> popElem floatlist
  | hd::rest -> match hd with
                  Push(float1) -> 
                    execute_help rest (execPush float1 floatlist)
                | Swap ->
                    execute_help rest (execSwap floatlist) 
                | Calculate(op1) -> 
                    execute_help rest (execCalculate op1 floatlist)

let execute (instrlist : instr list) : float =
  execute_help instrlist []


(* Part (c): compile function *)  
let rec compile (exp1 : exp) : instr list =
  match exp1 with
    Num(float1) -> [Push(float1)]
  | BinOp(exp2,op1,exp3) -> 
      (compile exp2) @ (compile exp3) @ [Calculate(op1)]


(* Part (d): decompile function and its helper function *)
let rec decompile_help instrlist explist = 
  match instrlist with
  [] -> explist
  | hd::rest -> match hd with
                  Push(float1) ->
                    decompile_help rest (explist @ [Num(float1)])
                | Calculate(op1) -> 
                  (
                  let revExplist = List.rev explist in
                  match revExplist with
                    [] -> explist
                  | nullcase::[] -> explist
                  | operand2::operand1::rest1 -> let newExplist = List.rev rest1 in
                    decompile_help rest (newExplist @ [BinOp(operand1,op1,operand2)])
                  )
                | Swap ->
                  let revExplist = List.rev explist in
                  match revExplist with
                    [] -> explist
                  | nullcase::[] -> explist
                  | operand2::operand1::rest1 -> let newExplist = List.rev rest1 in
                    decompile_help rest (newExplist @ [operand2] @ [operand1])

let decompile (instrlist : instr list) : exp =
  let explist = decompile_help instrlist [] in
  List.hd explist


(* Part (e): EXTRA CREDIT *) 
let rec compileOpt_help exp1 (instrlist,maxSize) stackSize =
  match exp1 with
    Num(float1) -> ([Push(float1)],1)
  | BinOp(exp2,op1,exp3) -> 
      match exp2,exp3 with
        Num(float2),Num(float3) ->
          if stackSize+2 > maxSize then
            (instrlist@[Push(float2)]@[Push(float3)]@[Calculate(op1)], stackSize+2)
          else
            (instrlist@[Push(float2)]@[Push(float3)]@[Calculate(op1)], maxSize)
      | Num(float2),BinOp(exp4,op2,exp5) ->
          let next = compileOpt_help exp3 (instrlist,maxSize) stackSize in
          let max1 = (snd next) in
          if op1 = Sub || op1 = Div then
            if stackSize+2 > max1 then
              (instrlist@(fst next)@[Push(float2)]@[Swap]@[Calculate(op1)], stackSize+2)
            else
              (instrlist@(fst next)@[Push(float2)]@[Swap]@[Calculate(op1)], max1)
          else
            if stackSize+2 > max1 then
              (instrlist@(fst next)@[Push(float2)]@[Calculate(op1)], stackSize+2)
            else
              (instrlist@(fst next)@[Push(float2)]@[Calculate(op1)], max1)
      | BinOp(exp4,op2,exp5),Num(float2) ->
          let next = compileOpt_help exp2 (instrlist,maxSize) stackSize in
          let max1 = (snd next) in
          if stackSize+2 > max1 then
            (instrlist@(fst next)@[Push(float2)]@[Calculate(op1)], stackSize+2)
          else
            (instrlist@(fst next)@[Push(float2)]@[Calculate(op1)], max1)
      | BinOp(exp4,op2,exp5),BinOp(exp6,op3,exp7) ->   
          let next = compileOpt_help exp2 (instrlist,maxSize) (stackSize+1) in
          let next1 = compileOpt_help exp3 (instrlist,maxSize) (stackSize+1) in
          let max1 = (snd next) - 1 in
          let max2 = (snd next1) - 1 in
          if max1 >= max2 then
            if stackSize+2 > max1 then
              (instrlist@(fst next)@(fst next1)@[Calculate(op1)], stackSize+2)
            else
              (instrlist@(fst next)@(fst next1)@[Calculate(op1)], max1)
          else
            if op1 = Sub || op1 = Div then
              if stackSize+2 > max2 then
                (instrlist@(fst next1)@(fst next)@[Calculate(op1)], stackSize+2)
              else
                (instrlist@(fst next1)@(fst next)@[Calculate(op1)], max2)
            else
              if stackSize+2 > max2 then
                (instrlist@(fst next1)@(fst next)@[Swap]@[Calculate(op1)], stackSize+2)
              else
                (instrlist@(fst next1)@(fst next)@[Swap]@[Calculate(op1)], max2)

let rec compileOpt (exp1 : exp) : (instr list * int) =
  compileOpt_help exp1 ([],0) 0