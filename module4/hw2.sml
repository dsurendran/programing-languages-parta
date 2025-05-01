(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(str : string, strs: string list) = 
  let
    fun remove_from_list(elem: string, xs : string list) =
      case xs of
           [] => []
          | hd :: xs' => if same_string(elem, hd) 
            then xs'
            else hd :: remove_from_list(elem, xs')
          in
            case strs of
                 [] => NONE
               | hd :: strs' => if same_string(hd, str) 
                                then SOME(remove_from_list(hd, strs))
                                else all_except_option(str, strs')
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

