(* datatypes *)
datatype mytype = TwoInts of int * int
       | Str of string
       | Pizza

val a = Str "hi"
val b = Str
val c = Pizza
val d = Pizza
val d = TwoInts(1+2,3+4)
val e = a
(* case expresssions *)
(* case e of p1 => e1 | p2 => e2 | p3 => e3 *)
fun f (x: mytype) =
    case x of
        Pizza => 3
      | Str s => String.size s
      | TwoInts(i1, i2) => i1 + i2
(* useful datatypes *)
datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King
       | Ace | Num of int

datatype id = StudentNum of int
       | Name of string
                     * (string option)
                     * string

(* Expression trees * Recursive datatype *)
datatype exp = Constant of int
       | Negate of exp
       | Add of exp * exp
       | Multiply of exp * exp

fun eval e =
    case e of
        Constant i => i
     | Negate e2 => ~ (eval e2)
     | Add (e1, e2) => (eval e1) + (eval e2)
     | Multiply (e1, e2) => (eval e1) * (eval e2)

fun max_constant e =
    case e of
        Constant i => i
     | Negate e1 => max_constant e1
     | Add (e1, e2) => Int.max(max_constant e1, max_constant e2)
     | Multiply(e1, e2) => Int.max(max_constant e1, max_constant e2)

(* Type synonyms *)
type card = suit * rank
(* give a name to record type *)
type name_record = {
    student_num : int option,
    first : string,
    middle : string option,
    last : string
}

fun is_Queen_of_Spades(c: card) =
    #1 c = Spade andalso #2 c = Queen

fun is_Queen_of_Spades_2(c : suit * rank) =
    #1 c = Spade andalso #2 c = Queen

(* different ways of representing types *)
val c1 : card  = (Diamond, Ace)
val c2 : suit * rank  = (Heart, Ace)
val c2 = (Spade, Ace)

(*type of card = type of suit * rank) *)
fun is_Queen_of_Spades_alt c =
    case c of
        (Spade, Queen) => true
     | _ => false

(* Lists *)
datatype my_int_list = Empty
  | Cons of int * my_int_list

val x = Cons(4, Cons(5, Cons(6, Empty)))

fun append_my_list (xs, ys) =
  case xs of
    Empty => ys
    | Cons(x, xs') => Cons(x, append_my_list(xs', ys))

(* Options *)
fun inc_or_zero intoption =
  case intoption of
    NONE => 0
    | SOME i => i + 1

(* Lists *)
(* [] :: are just constructors *)
(* do not use tl hd null*)

fun sum_list xs =
  case xs of
    [] => 0
    | x::xs' => x + sum_list xs'

fun append (xs, ys) =
  case xs of
    [] => ys
    | x::xs' => x :: append(xs', ys);

(*More pattern matching in each of type*)
fun sum_triple triple =
  case triple of
    (x,y,z) => x + y + z

fun full_name r =
  case r of
    {first=x, second=y, last=z} => x ^ " " ^ y ^ " " ^ z
