
(* A simple test harness for the MOCaml interpreter. *)

(* put your tests here:
   each test is a pair of a MOCaml declaration and the expected
   result:
     - the MOCaml declaration is a string of exactly what you would type into the interpreter prompt,
       without the trailing ";;"
     - the expected result is a string of exactly what you expect the interpreter to print as a result
   use the string "dynamic type error" as the result if a DynamicTypeError is expected to be raised.
   use the string "match failure" as the result if a MatchFailure is expected to be raised.
   use the string "implement me" as the result if an ImplementMe exception is expected to be raised

   call the function runtests() to run these tests
*)
let tests = [
    (* YOU NEED TO ADD A LOT MORE TESTS! *)
("let five = 5","val five = 5");
("let add5 = function x -> five + x", "val add5 = <fun>");
("add5 2", "7");
("let five = 4", "val five = 4");
("add5 6", "11");
("add5 (-5)", "0");
("let add2Tuple = function (x,y) -> x + y", "val add2Tuple = <fun>");
("let add5Tuple = function (x,y,z,w,v) -> x + y + z + w + v", "val add5Tuple = <fun>");
("add2Tuple (1, (-1))", "0");
("add2Tuple (2,5)", "7");
("add2Tuple (true,1)", "dynamic type error"); 
("add5Tuple (1,1,1,1,1)", "5");
("let matchfun = function x -> match x with 0 -> true | _ -> false", "val matchfun = <fun>");
("matchfun 0", "true");
("matchfun (-9)", "false");
("matchfun false", "false"); (* this is false because no illegal ops are actually happening, types are irrelevant in matching *)
("let rec len l = match l with [] -> 0 | _::t -> 1 + (len t)", "val len = <fun>");
("len []", "0");
("len [1;2;3]", "3");
("let rec fact x = match x with 0 -> 1 | _ -> x * (fact (x-1))", "val fact = <fun>");
("fact 0", "1");
("fact 1", "1");
("fact 2", "2");
("fact 5", "120");
("let rec map f = function l -> match l with [] -> [] | h::t -> (f h)::(map f t);;", "val map = <fun>");
("map (function x -> x + 1) [1;2;3]", "Cons (2, Cons (3, Cons (4, Nil)))");
("map (function x -> x + 1) [1;true;3]", "dynamic type error");
("let positive = function x -> if x>0 then true else false", "val positive = <fun>");
("positive 5", "true");
("positive (-5)", "false");
("positive 0", "false");

("let posDynTest = function x -> if x>0 then true else 5", "val posDynTest = <fun>");
("posDynTest 5", "true");
("posDynTest (-1)", "5");
    
(* tuple matching *)
("let rec foldright (f,l,b) = match l with [] -> b | h::t -> foldright (f, t, (f h b))", "val foldright = <fun>");
("foldright ((function x -> function y -> x + y), ([1;2;3]),0)", "6");
    
("let rec foldright f = function l -> function b -> match l with [] -> b | h::t -> foldright f t (f h b)", "val foldright = <fun>");
("foldright (function x -> function y -> x + y) [1;2;3] 0", "6");
    
("let weirdMatch = function (x,y) -> match (x,y) with (x,y,z) -> 0 | (x,y) -> x+y", "val weirdMatch = <fun>");
("match (1,2) with (x,y,z) -> 0 | (x,y) -> x+y", "3");
("weirdMatch (4,2)", "6");
("match (1,2,false) with (x,y,z) -> 0 | (x,y) -> x+y", "0");
("match (1,2,4) with (x,y,z) -> 0 | (x,y) -> x+y", "0");
    
(* repeated function name *)
("let rec repeat repeat = repeat + 7", "val repeat = <fun>");
("repeat 2", "9");

(* point *)
("let point = Point(1)", "val point = Point 1");
("match point with Point(0) -> 0 | Point(1) -> 1", "1");
("let matchPoint = function p -> match p with Point(0) -> 0 | Point(x) -> x", "val matchPoint = <fun>");
("matchPoint point", "1");
("let point2 = Point(5)", "val point2 = Point 5");
("matchPoint point2", "5");
("matchPoint (Point(-3))", "-3");
(* Data Patterns/Constructors*)
("Nil", "Nil");
("let x = HelloWorld", "val x = HelloWorld");
("let x = Hello 1", "val x = Hello 1");
("let x = Ocaml true", "val x = Ocaml true");
("let x = Ocaml (IsLove true)", "val x = Ocaml IsLove true");

(* foldleft & others *)
("let rec foldleft f =
  function acc -> function l ->
    match l with 
      [] -> acc  
    | h::t -> foldleft f (f acc h) t", "val foldleft = <fun>");
("let rev = function l -> foldleft (function acc -> function e -> e::acc) [] l", "val rev = <fun>");
("let alist = [1;2;3]", "val alist = Cons (1, Cons (2, Cons (3, Nil)))");
("rev alist", "Cons (3, Cons (2, Cons (1, Nil)))");
("rev [true;false;false;true;false]", "Cons (false, Cons (true, Cons (false, Cons (false, Cons (true, Nil)))))");

("let filter = function p -> function l -> rev (foldleft (function acc -> function e -> if (p e) then e::acc else acc) [] l)", "val filter = <fun>");
("let nums = [1;2;3;2]", "val nums = Cons (1, Cons (2, Cons (3, Cons (2, Nil))))");
("filter (function x -> if x=2 then false else true) nums", "Cons (1, Cons (3, Nil))");

(* check if two lists are the same: matching lists in tuples *)
("let rec checkListsEqual l1 = function l2 ->
  match (l1,l2) with
    ([],[]) -> true
  | (_::_, []) -> false
  | ([], _::_) -> false
  | (h1::t1, h2::t2) -> if h1=h2 then (checkListsEqual t1 t2) else false", "val checkListsEqual = <fun>");
("checkListsEqual [1;2;3] [1;2;3]", "true");
("checkListsEqual [true;false;true] [true;false;true]", "dynamic type error");
("checkListsEqual [true;2;3] [true;2;3]", "dynamic type error");
("checkListsEqual [true;2;3] [1;2;3]", "dynamic type error");
("checkListsEqual [1;2;3;4] [1;2;3]", "false");
("checkListsEqual [1;2;3] [1;2;3;4]", "false");
("checkListsEqual [] [1;2;3]", "false");
("checkListsEqual [] []", "true");
("checkListsEqual [1;2;3] []", "false");
		]

(* The Test Harness
   You don't need to understand the code below.
*)
  
let testOne test env =
  let decl = main token (Lexing.from_string (test^";;")) in
  let res = evalDecl decl env in
  let str = print_result res in
  match res with
      (None,v) -> (str,env)
    | (Some x,v) -> (str, Env.add_binding x v env)
      
let test tests =
  let (results, finalEnv) =
    List.fold_left
      (fun (resultStrings, env) (test,expected) ->
	let (res,newenv) =
	  try testOne test env with
	      Parsing.Parse_error -> ("parse error",env)
	    | DynamicTypeError _ -> ("dynamic type error",env)
	    | MatchFailure -> ("match failure",env)
	    | ImplementMe s -> ("implement me",env) in
	(resultStrings@[res], newenv)
      )
      ([], Env.empty_env()) tests
  in
  List.iter2
    (fun (t,er) r ->
      let out = if er=r then "ok" else "expected " ^ er ^ " but got " ^ r in
      print_endline
	(t ^ "....................." ^ out))
      tests results

(* CALL THIS FUNCTION TO RUN THE TESTS *)
let runtests() = test tests
  
