exception Undefined; (*Indicates that the value being seeked is undefined*)
exception NoItem; (*Indicates that index is inappropriate for given list*)
exception Mismatch; (*Indicates that two lists do not have same size*)

(*
	length: It is helper function for many other functions, it gives length of the list
	Signature: 'a list -> int
*)
fun length [] = 0
| length (x::xs) = 1 + length xs
;



(*
	avg_list: This function gives average of all elements in real list. Notice that foldl is used with 0.0 as other argument to get sum of real elements.
	Raises: Undefined, if list is null
	Signature: real list -> real
*)
fun avg_list [] = raise Undefined
| avg_list lst = 
	(*(length lst) gives int, so for division we need to convert it to real*)
	(foldl op+ 0.0 lst)/(real(length lst))



(*
	get_index: gives item in a zero indexed list
	Raises: NoItem, if index is inappropriate for given list
	Signature: 'a list -> int -> 'a
*)
fun get_index [] ind = raise NoItem
| get_index (x::xs) 0 = x (*we check for ind = 0 as we are returning as per zero indexed list*)
| get_index (x::xs) ind = get_index xs (ind-1)
;


(*
	get_odd_midpoint: gives middle element of odd lengthed list	
	Raises: Undefined, if null list is given
	Signature: 'a list -> 'a
*)
fun get_odd_midpoint [] = raise Undefined
| get_odd_midpoint lst = get_index lst ((length lst) div 2); 
(*a div b is equivalent to floor(a/b) for real valued a and b, hence for zero indexed list 5/2 would give 2 which is 3rd and middle element*)


(*
	get_even_midpoint: gives average of two middle elements of real list
	Raises: Undefined, if null list is given
	Signature: real list-> real
*)
fun get_even_midpoint [] = raise Undefined
| get_even_midpoint lst =
let
	val ind = (length lst) div 2;
	val a = get_index lst (ind-1);
	val b = get_index lst (ind);
in
	(a+b)/2.0
end;


(*
	get_median: gives middle element of sorted list if list is odd lengthed, otherwise gives average of middle two elements of sorted list.
	Raises: Undefined, if list is null
	Signature: real list-> real
*)
fun get_median [] = raise Undefined
| get_median lst =
let
	(*
		quicksort: sorts a real valued list
		Signature: real list-> real list
		Courtesy: Lecture Notes and Xingjian Guo
	*)
	fun quicksort [] = []
	| quicksort [x:real] = [x] (*x:real specifically mentioned to make type of function as real*)
	| quicksort (a::bs) =
	let
		fun deposit (x, (left, right)) =
		if x < a
		then (x::left, right)
		else (left, x::right);

		val (left, right) = foldr deposit ([], []) bs;
	in
		quicksort left @ (a:: quicksort right)
	end;

	val len1 = (length lst) div 2; (*gives index of middle element if list is odd lengthed*)
	val len2 = ((length lst)-1) div 2; (*gives index of another middle element if list is even lengthed*)

	(*
		Note: if list is odd then, len1 and len2 would be same (for e.g. (7/2) = 3 and (6/3) = 3) otherwise, they won't be same. This fact is utilised to find median below.
	*)
in
	if len1 = len2
	then get_odd_midpoint (quicksort lst)
	else get_even_midpoint (quicksort lst)
end;


(*
	listsum: gives true if sum of all elements in int list is equal to int provided as argument, gives false otherwise. Notice how foldl is used to achieve sum of ints similar to previous use for getting sum of reals
	Raises: Undefined, if list is null
	Signature: int list -> int -> bool
*)
fun listsum [] v = raise Undefined
| listsum lst v = v = (foldl op+ 0 lst)


(*
	isten: gives true if sum of all elements in int list is equal to 10.
	Raises: Undefined, if list is null
	Signature: int list -> bool
*)
fun isten lst = listsum lst 10;


(*
	zip: takes two lists, and gives list of tuples of corresponding elements
	Raises: Mismatch if two lists do not have same length
	Signature: 'a list * 'b list -> ('a * 'b) list
*)
fun zip ([], y::ys) = raise Mismatch
| zip (x::xs, []) = raise Mismatch
| zip ([], []) = []
| zip (x::xs, y::ys) = [(x,y)]@zip(xs,ys)
;


(*
	unzip: takes list of tuples and gives two corresponding list of elements
	Signature: ('a * 'b) list -> 'a list * 'b list 
*)
fun unzip [] = ([], [])
| unzip ((x,y)::xsys) =
let val (xs, ys) = unzip xsys (*get unzipped versions of later elements first*)
in (x::xs, y::ys) (*Prepend current elements to unzipped lists of later elements*)
end
;



(*
	scan_left: gives list of each values taken by accumulator during processing of a fold
	Signature: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a list
*)
fun scan_left F y [] = [y] (*this is last accumulator value, all other values must be prepended to it*)
| scan_left F y (x::xs) =
let
	val a = F y x (*calculate next accumulator value*)
in
	y::(scan_left F a xs) (*prepend current accumulator value to the list of later ones*)
end
;



(*
	fact_list: gives list of each value from 1 uptill n
	Signature: int -> int list
*)
fun fact_list 0 = []
| fact_list n = 
let
	
	(*
		countup: gives list of elements from 1 to n
		Signature: int -> int list
	*)
	fun countup n =
	let
		fun countup' 1 l = l
		| countup' i l = countup' (i-1) (i::l)
	in
		countup' n []
	end
in
	if n < 0
	then raise Undefined
	else
	(*Notice: factorial of a value n is multiplication of accumulated factorial of n-1 and n, thus I have utilised this fact to use scan_left here to get factorial of all element from 1 to n*)
	scan_left (fn x => fn y => x*y) 1 (countup n)
end;