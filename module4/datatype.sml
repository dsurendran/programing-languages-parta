datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza

(* TwoInts, Str, Pizza are constructors *)
val a = Str "hi"
val b = Str
val c = Pizza
val d = TwoInts(2,3)
val e = a

(* case expressions *)
fun f (x : mytype) =
    case x of
        Pizza => 3
      | Str s => String.size s
      | TwoInts(i1, i2) => i1 + i2

(* enumerations, including carrying other data *)
datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King
                | Ace | Num of int
                                   
(*Alternate ways of identifying real-world things/people *)
datatype id = StudentNum of int
            | Name of string * (string option) * string
                                                     
(*Recursive datatypes*)
datatype Expression =
         Constant of int
         | Negate of Expression
         | Add of Expression * Expression
         | Multiply of Expression * Expression

fun evaluate(e: Expression) = 
    case e of
        Constant c => c
      | Negate e1 => ~ (evaluate e1)
      | Add(e1, e2) => (evaluate e1) + (evaluate e2)
      | Multiply(e1, e2) => (evaluate e1) * (evaluate e2)

val example_exp : Expression = Add(Constant 19, Negate (Constant 10))
val result : int = evaluate example_exp

