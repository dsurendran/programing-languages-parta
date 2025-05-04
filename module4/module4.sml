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
(* list and option are not types but type constructors that 
* take type parameters *)
(* [] :: are just constructors *)
fun sum_list xs =
  case xs of
    [] => 0
    | x::xs' => x + sum_list xs'

fun append (xs, ys) =
  case xs of
    [] => ys
    | x::xs' => x :: append(xs', ys);

(*More pattern matching in each of type*)
(* Every function in ML uses pattern matching *)
fun sum_triple triple =
  case triple of
    (x,y,z) => x + y + z

fun sum_triple(x,y,z) = x + y + z

fun full_name r =
  case r of
       {first=x, second=y, last=z} => x ^ " " ^ y ^ " " ^ z

fun full_name {first=x, second=y, last =z} = x ^ " " ^ y ^ " " ^ z
(* Every function in ML takes exactly one argument, that is tuple *)
fun rotate_left(x,y,z) = (y,z,x)
fun rotate_right(x,y,z) = rotate_left(rotate_left(x,y,z))
(* Nested pattern *)

fun zip3 list_triple = 
  case list_triple of
       ([], [], []) => []
     | (hd1::tl1, hd2::tl2, hd3::tl3) => (hd1, hd2, hd3) :: zip3(tl1, tl2, tl3)
(*     | _ => raise ListLengthMismatch *)

fun unzip3 lst = 
  case lst of 
       [] => ([],[],[])
     | (a,b,c)::tl => let val (l1, l2, l3) = unzip3 tl
                      in
                        (a::l1, b::l2, c::l3)
                      end

fun nondecreasing xs =
  case xs of
       [] => true
     | x::[] => true
     | head::(neck::rest) => head <= neck andalso nondecreasing(neck :: rest)
 
datatype sgn = P | N | Z

fun multsign(x1, x2) = 
  let 
    fun sign x = if x = 0 then Z else if x > 0 then P else N
  in
    case (sign x1, sign x2) of
         (P, P) => P
       | (N, N) => P
       | (_, Z) => Z
       | (Z, _) => Z
       | _ => N
  end


fun hd xs =
  case xs of
       [] => raise List.Empty
     | x::_ => x

exception MyUndesirableCondition
exception MyOtherException of int * int

fun mydiv (x, y) =
  if y = 0
  then raise MyUndesirableCondition
  else x div y

fun maxlist(xs, ex) =
  case xs of 
       [] => raise ex
     | x::[] => x
     | x::xs' => Int.max(x, maxlist(xs', ex))

(* Tail recursion *)
fun fact n =
  if n = 0
  then 1
  else n * fact(n-1)

