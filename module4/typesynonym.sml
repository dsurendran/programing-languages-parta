
(* enumerations, including carrying other data *)
datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King
                | Ace | Num of int

type card = suit * rank

fun is_Queen_of_spades (c : card) =
    #1 c = Spade  andalso #2 c = Queen

val c1 : card = (Diamond, Ace)
val c2 : suit * rank = (Heart, Ace)
val c3 = (Spade, Ace)
             
fun is_Queen_of_spades2 c =
    case c of
        (Spade, Queen) => true
      | _ => false

fun same_thing(x, y) =
    if x = y then "yes" else "no"
