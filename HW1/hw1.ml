(*	HW1 for CS131
	Jaekwan Ahn
	UID: 604057669

	I used lecture and discussion notes while doing this hw.
*)

(* Problem 1 *)

(*	(a) member x s : checks whether the item is a member of the set s *)
let rec member x s =
	match s with
		[] -> false
	|	h::t -> if h = x then true else (member x t)
(*	Compare x with every element in s and 
	if there exists any element equal to x, then return true, otherwise return false *)


(*  (b) add x s : returns a set that s union {x} *)
let add x s =
	if (member x s) then s else x :: s
(*	Check if x is a member of s by using 'member' function defined above, and
	if so put x in s, otherwise just return s *)


(*	(c) union s1 s2 : returns a set that represents s1 union s2 *)
let rec union s1 s2 =
	match s1 with
		[] -> s2
	|	h::t -> (union t (add h s2))
(*	Transfer each element from s1 to s2 for each iteration by using 'add' function defined above *)


(*	(d) fastUnion s1 s2 : returns a set that represents s1 union s2
	Each input set has its elements in sorted order, from least to greatest
	The returned set should also be in sorted order *)
let rec fastUnion s1 s2 =
	match s1 with
		[] -> s2
	|	h1::t1 ->
			if s2 = [] then s1
			else	let h2::t2 = s2 in
					if h1 < h2 then h1 :: (fastUnion t1 s2)
					else if h1 = h2 then h1 :: (fastUnion t1 t2)
					else h2 :: (fastUnion s1 t2)
(*	Compare first elements of s1 and s2 and put the smaller one first in the front
	When compile, it has "Warning" in line 42 saying that [] is not matched
	but indeed in line 41, I actually covered the case of [] *)


(*	(e) intersection s1 s2 : returns a set that represents s1 intersect s2 
	Do not implement recursively; instead consist of a single call to either List.map or List.filter *)
let intersection s1 s2 =
	List.filter (fun x -> member x s2) s1
(*	Simply use List.filter on every elements of s1 if also existing in s2 *)


(* (f) setify l : takes a list l which may contain duplicates and removes all duplicates *)
let rec setify l =
	match l with
		[] -> []
	|	h::t -> if (member h t) then (setify t)
				else h :: (setify t)
(*	By using 'member' function defined above, 
	if one element already exists in the rest, then ignore it *)


(* (g) powerset s : returns a set that represents the power set of s *)
(* helper function *)
let rec addlist x s =
	match s with
		[] -> []
	|	h::t -> (add x h) :: (addlist x t)

let rec powerset s =
	match s with
		[] -> [[]]
	|	h::t -> (addlist h (powerset t)) @ (powerset t)
(*	'addlist' is adding x into every list elements of s 
	i.e. addlist 1 [[] [2]] -> [[1] [1;2]]
	By using 'addlist' recursively on the first element of s, I implemented 'powerset' *)



(* Problem 2 *)

(*	(a) partition p l : similar to filter except that it returns a pair of lists,
	where the first list contains the elements satisfy the predicate
	and the second list contains the elements that falsify the predicate *)
let rec partition p l =
	match l with
		[] -> ([],[])
	|	h::t ->	let rest = partition p t in
				let (l1,l2) = rest in
				if p h then (h::l1,l2) else (l1,h::l2)
(*	Refer to lecture note of implementation of 'filter'
	In this case, use "let in" expression twice to produce a tuple of lists *)


(*	(b) whle p f x : while loop using recursion 
	Iteratively invokes the function f to create new values, 
	starting from x, while the result of invoking p is true *)
let rec whle p f x =
	if p x then whle p f (f x)
	else x
(*	Use "If and else" *)


(*	(c) pow n f : raises a function f to a given power n
	Assume that the given number is nonnegative *)
let rec pow n f =
	if n = 0 then fun v -> v
	else fun v -> f ((pow (n-1) f) v)
(*	Refer to lecture note of implementation of 'twice' *)


(* end of HW1 *)