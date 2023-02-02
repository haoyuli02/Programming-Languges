(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str, str_list) =
   case str_list of
       [] => NONE
     | head::tail => if same_string(str, head)
                     then SOME tail
                     else case all_except_option (str, tail) of
                              NONE => NONE
                            | SOME lst => SOME (head::lst)

fun get_substitutions1 (str_list_list, s) =
   case str_list_list of
       [] => []
     | head::tail => case all_except_option(s, head) of
                         NONE => get_substitutions1(tail, s)
                       | SOME lst => lst @ get_substitutions1(tail, s)

fun get_substitutions2 (str_list_list, s) =
   let fun tail_rec (str_list_list, acc) =
          case str_list_list of
              [] => acc
            | head::tail => case all_except_option(s, head) of
                                NONE => tail_rec (tail, acc)
                              | SOME lst => tail_rec (tail, acc @ lst)
   in
      tail_rec(str_list_list, [])
   end

fun similar_names (str_list_list, full_name) =
   let
      val {first=x, middle=y, last=z} = full_name
      val similar_names = get_substitutions2(str_list_list, x)
      fun dfs (similar_names, acc) =
         case similar_names of
             [] => acc
           | head::tail => dfs(tail, {first=head,middle=y,last=z}::acc)
   in
      full_name::dfs(similar_names,[])
   end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (card_suit, card_rank) =
   case card_suit of
       Clubs => Black
     | Spades => Black
     | Diamonds => Red
     | Hearts => Red

fun card_value (card_suit, card_rank) =
   case card_rank of
       Num n => n 
     | Ace => 11
     | _ => 10

fun remove_card (cs, c, e) =
   case cs of
       [] => raise e 
     | head::tail => if head = c
                     then tail
                     else head::remove_card(tail, c, e)

fun all_same_color cs =
   case cs of
       [] => true
     | _::[] => true
     | head::(neck::tail) => (card_color(head) = card_color(neck)) andalso all_same_color(neck::tail)

fun sum_cards cs =
   let
      fun tail_rec(cs, acc) =
         case cs of
             [] => acc
           | head::tail => tail_rec(tail, acc + card_value(head))
   in
      tail_rec(cs, 0)
   end

fun score (held_cards, goal) =
   let
      val sum = sum_cards(held_cards)
      val prelimscore = if sum > goal then 3 * (sum - goal) else goal - sum
   in
      if all_same_color(held_cards)
      then prelimscore div 2
      else prelimscore
   end

fun officiate (card_list, move_list, goal) =
   let
      fun dfs (held_cards, card_list, move_list) =
         case move_list of
             [] => score(held_cards, goal)
           | head_move::tail_move => case head_move of
                               			 Draw => (case card_list of
                                           			 [] => score (held_cards, goal)
                                         		   | head_card::tail_card => if sum_cards(head_card::held_cards) > goal
										 						   			 then score(head_card::held_cards, goal)
																   			 else dfs (head_card::held_cards, tail_card, tail_move))
                             		   | Discard c => (dfs(remove_card (held_cards, c, IllegalMove), card_list, tail_move))
   in
      dfs ([], card_list, move_list)
   end

(* Below are solutions for challenge problems *)

(* Challenge Problem A, I very much believe this is overcomplicated *)
fun sum_to_all (lst, n) =
	case lst of
		[] => []
	  | head::tail => (head+n)::sum_to_all(tail, n)
	  					  
fun count cs =
	case cs of
		[] => 0
	  | (suit, rank)::tail => case rank of
	  					  		  Ace => 1 + count(tail)
								| _ => count(tail)

fun sum_exclude_ace cs =
	case cs of
		[] => 0
	  | (suit, rank)::tail => case rank of
	  					  		  Ace => sum_exclude_ace tail
								| _ => sum_exclude_ace(tail) + card_value((suit, rank))

fun sum_challenge cs =
	let
		val count_ace = count(cs)
		val sum_no_ace = sum_exclude_ace(cs)
		fun generate_possible (n, acc) =
			if n = 0
			then acc
			else generate_possible(n-1, sum_to_all(acc, 1) @ sum_to_all(acc, 11))
	in	
		generate_possible(count_ace, [sum_no_ace])
	end

fun compute_score_from_sum (sum, same_color, goal) =
   let
      val prelimscore = if sum > goal then 3 * (sum - goal) else goal - sum
   in
      if same_color
      then prelimscore div 2
      else prelimscore
   end

fun score_challenge (held_cards, goal) =
	let
		val possible_lst = sum_challenge(held_cards)
		val same_color = all_same_color(held_cards)
		fun min_score (sum_list, goal) =
			case sum_list of
				[] => raise List.Empty
		  	  | x::[] => compute_score_from_sum (x, same_color, goal)
		  	  | head::tail => (let 
			  				         val head_sum = compute_score_from_sum(head, same_color, goal)
								      val rec_sum = min_score (tail, goal)
							       in
							   	   if head_sum < rec_sum then head_sum else rec_sum
							       end)
	in
		min_score(possible_lst, goal)
	end

fun test_game_ends_if_sum_exceeds (held_cards, goal) =
	let
		val possible_sums = sum_challenge (held_cards)
		fun one_smaller (possible) =
			case possible of
				[] => true
			  | x::[] => x <= goal
			  | x::xs' => x <= goal orelse one_smaller(xs')
	in
		one_smaller(possible_sums)
	end

fun officiate_challenge (card_list, move_list, goal) =
	let
      fun dfs (held_cards, card_list, move_list) =
         case move_list of
             [] => score_challenge(held_cards, goal)
           | head_move::tail_move => case head_move of
                               			 Draw => (case card_list of
                                           			 [] => score_challenge (held_cards, goal)
                                         		   | head_card::tail_card => if test_game_ends_if_sum_exceeds (head_card::held_cards, goal)
												   							 then dfs (head_card::held_cards, tail_card, tail_move)
										 						   			 else score_challenge (head_card::held_cards, goal))
                             		   | Discard c => dfs(remove_card (held_cards, c, IllegalMove), card_list, tail_move)
   in
      dfs ([], card_list, move_list)
   end

(* Challenge Problem B, and it is incorrect now *)
fun discard_then_draw (card_list, first, goal) =
	case card_list of
		[] => NONE
	  | head::tail => if score(first::remove_card(card_list, head, IllegalMove), goal) = 0
	  				  then SOME head
					  else discard_then_draw(tail, first, goal)

fun careful_player (card_list, goal) =
	let
		fun care (card_list, goal, curr_score, held_cards, sum) =
			case curr_score of
				0 => []
			  | _ => case card_list of
			  			 [] => if sum + 10 < curr_score then [Draw] else []
					   | x::xs' => (case discard_then_draw (held_cards, x, goal) of
					   			  	   	NONE => if sum + 10 < curr_score
												then Draw::care(xs', goal, score(x::held_cards, goal), x::held_cards, sum + card_value(x))
												else (case held_cards of
														  [] => Draw::care(xs', goal, score([x], goal), [x], card_value(x))
														| y::ys' => (Discard y)::care(x::xs', goal, score(ys', goal), ys', sum - card_value(y)))
									  | SOME c => [Discard c, Draw])
    in
		care (card_list, goal, goal, [], 0)
	end