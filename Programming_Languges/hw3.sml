(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 		 | Variable of string
		 		 | UnitP
		 		 | ConstP of int
		 		 | TupleP of pattern list
		 		 | ConstructorP of string * pattern

datatype valu = Const of int
	      	  | Unit
	      	  | Tuple of valu list
	      	  | Constructor of string * valu

fun g f1 f2 p =
    let 
		val r = g f1 f2 
    in
		case p of
		    Wildcard          => f1 ()
		  | Variable x        => f2 x
		  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
		  | ConstructorP(_,p) => r p
		  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     	 | UnitT
	     	 | IntT
	     	 | TupleT of typ list
	     	 | Datatype of string

fun only_capitals str_l =
	List.filter (fn str => Char.isUpper(String.sub(str, 0))) str_l;

fun longest_string1 str_l =
	List.foldl (fn (str1, str2) => if String.size str1 > String.size str2 then str1 else str2 ) "" str_l;

fun longest_string2 str_l =
	List.foldl (fn (str1, str2) => if String.size str1 >= String.size str2 then str1 else str2) "" str_l;

fun longest_string_helper func str_l =
	List.foldl (fn (str1, str2) => if func (String.size str1, String.size str2) then str1 else str2) "" str_l;

val longest_string3 = longest_string_helper (fn (a, b) => a > b);

val longest_string4 = longest_string_helper (fn (a, b) => a >= b);

val longest_capitalized = longest_string1 o only_capitals;

val rev_string = String.implode o List.rev o String.explode

fun first_answer f lst =
	case lst of
		[] => raise NoAnswer
	  | x::xs' => (case f x of
	  				  NONE => first_answer f xs'
					| SOME v => v)

fun all_answers f lst =
	let
		fun helper (lst, acc) =
			case lst of
				[] => SOME acc
			  | x::xs' => (case f x of
			  				   NONE => NONE
							 | SOME lst1 => helper (xs', lst1@acc))
	in
		helper (lst, [])
	end

val count_wildcards = g (fn _ => 1) (fn x => 0);

val count_wild_and_variable_lengths = g (fn _ => 1) (fn x => String.size x);

fun count_some_var (str, p) = g (fn _ => 0) (fn x => if x = str then 1 else 0) p;

fun check_pat p =
	let
		fun get_all_var p =
			case p of
				Wildcard => []
			  | Variable s => [s]
			  | UnitP => []
			  | ConstP _ => []
			  | TupleP p_lst => foldl (fn (p1, acc) => acc @ get_all_var (p1)) [] p_lst
			  | ConstructorP (_, p) => get_all_var p 

		val all_var = get_all_var p

		fun check_distinct (lst : string list) =
			case lst of
				[] => true
			  | x::xs' => not (List.exists (fn y => x = y) xs') andalso check_distinct xs'
	in
		check_distinct (all_var)
	end

fun match (v, p) =
	case (v, p) of
		(_, Wildcard) => SOME []
	  | (v, Variable s) => SOME [(s, v)]
	  | (Unit, UnitP) => SOME []
	  | (Const c1, ConstP c2) => if c1 = c2 then SOME [] else NONE
	  | (Tuple vl, TupleP pl) => if List.length vl = List.length pl
	  							 then all_answers match (ListPair.zip(vl, pl))
								 else NONE
	  | (Constructor (s1, value), ConstructorP (s2, pattern)) => if s1 = s2 then match(value, pattern) else NONE
	  | _ => NONE 

fun first_match value pattern_lst =
	SOME (first_answer match (List.map (fn p => (value, p)) pattern_lst))
	handle NoAnswer => NONE

(* Challenge Problem *)
fun combine_more_general (p : typ option, t : typ option) =
	case (p, t) of
		(NONE, _) => NONE
	  | (_, NONE) => NONE
	  | (SOME Anything, SOME Anything) => SOME Anything
	  | (SOME UnitT, SOME Anything) => SOME UnitT
	  | (SOME IntT, SOME Anything) => SOME IntT
	  | (SOME (TupleT pattern), SOME Anything) => SOME (TupleT pattern)
	  | (SOME (Datatype s), SOME Anything) => SOME (Datatype s)
	  | (SOME Anything, SOME UnitT) => SOME UnitT
	  | (SOME UnitT, SOME UnitT) => SOME UnitT
	  | (_, SOME UnitT) => NONE
	  | (SOME Anything, SOME IntT) => SOME IntT
	  | (SOME IntT, SOME IntT) => SOME IntT
	  | (_, SOME IntT) => NONE
	  | (SOME Anything, SOME (TupleT pattern)) => SOME (TupleT pattern)
	  | (SOME (TupleT type1), SOME (TupleT type2)) => (let
	  									   				  fun combine_list_type (a, b) =
										   	  				 if not (List.length a = List.length b)
											  				 then NONE
											  				 else (case a of
											  		   				   [] => SOME []
													 				 | x::xs' => (let
													 								 val type_head = combine_more_general(SOME x, SOME (hd b))
																					 val type_tail = combine_list_type (xs', tl b)
																  				  in 
																 					 if not (isSome(type_head) andalso isSome(type_tail))
													 			 					 then NONE
																					 else SOME (valOf(type_head)::valOf(type_tail))
																  				  end))
										 			   in
														  case combine_list_type (type1, type2) of
														  	  NONE => NONE
														    | SOME typ_lst => SOME (TupleT typ_lst)
										 			   end)
	  | (_, SOME (TupleT t)) => NONE
	  | (SOME Anything, SOME (Datatype s)) => SOME (Datatype s)
	  | (SOME (Datatype s1), SOME (Datatype s2)) => if s1 = s2 then SOME (Datatype s1) else NONE
	  | (_, SOME (Datatype _)) => NONE

(* Correct except I forget about the case where when there is no typ 
that all the patterns in the list can have due to a constructor given an argument with wrong type *)
fun typecheck_patterns (datatype_lst : (string * string * typ) list, pattern_lst : pattern list) =
	case pattern_lst of
		[] => SOME Anything
	  | x::xs' => let
	  				 val t = typecheck_patterns(datatype_lst, xs')
				  in
				  	 case x of
					 	Wildcard => combine_more_general(SOME Anything, t)
					  | Variable _ => combine_more_general(SOME Anything, t)
					  | UnitP => combine_more_general(SOME UnitT, t)
					  | ConstP _ => combine_more_general(SOME IntT, t)
					  | TupleP pl => (let
					  					fun get_each_type (a_lst : pattern list) =
											case a_lst of
												[] => SOME []
											  | head::tail => case typecheck_patterns (datatype_lst, [head]) of
											  					  NONE => NONE
																| SOME head_type => (case get_each_type(tail) of
																						NONE => NONE
																					  | SOME tail_type => SOME (head_type::tail_type))
										val all_element_type = get_each_type (pl)
									 in 
										case all_element_type of
											NONE => NONE
										  | SOME lst => combine_more_general(SOME (TupleT lst), t)
									 end)
					  | ConstructorP (s, d)=> (let
					  							  fun find_data_type (str : string, lst : (string * string * typ) list) =
												 	 case lst of
														 [] => NONE
													   | (a,b,c)::rest => if a = str then SOME b else find_data_type(s, rest)

												  val type_name = find_data_type(s, datatype_lst)
											   in
											  	  case type_name of
												 	  NONE => NONE
												    | SOME name => combine_more_general(SOME (Datatype name), t)
											   end)
				  end
												 