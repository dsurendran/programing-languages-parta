(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 1 here *)
(* a, [b,a,c] *)
fun all_except_option(str, []) = NONE
  | all_except_option (str, head::tail) = case same_string(str,head) of
                                          true => SOME tail
                                        | false => case all_except_option(str, tail) of
                                                       NONE => NONE
                                                     | SOME xs => SOME (head :: xs)

fun get_substitutions1(xs : string list list, s : string) =
  case xs of
      [] => []
    | x::xs' => case all_except_option(s, x) of
                    NONE => get_substitutions1(xs',s)
                  | SOME ll => ll @ get_substitutions1(xs',s);

