datatype Expression =
         Constant of int
         | Negate of Expression
         | Add of Expression * Expression
         | Multiply of Expression * Expression

fun old_evaluate (e: Expression) = 
    case e of
        Constant c => c
      | Negate e1 => ~ (old_evaluate e1)
      | Add(e1, e2) => (old_evaluate e1) + (old_evaluate e2)
      | Multiply(e1, e2) => (old_evaluate e1) * (old_evaluate e2)

fun evaluate (Constant c) = c
  | evaluate (Negate e) = ~ (evaluate e)
  | evaluate (Add(e1,e2)) = evaluate e1 + evaluate e2
  | evaluate (Multiply(e1,e2)) = evaluate e1 * evaluate e2

fun append ([],ys) = ys
  | append (x::xs', ys) = x::append(xs',ys)

(* fun f1 (a::b::c) = 2 + f1 c *)
(*   | f1 [] = 0 *)
exception Empty
fun hd [] = raise Empty
  | hd (x::_) = x

exception MyUndesirableCondition

fun max (x,y) = if x > y then x else y
                                         
fun maxlist(xs, ex) =
    case xs of
        [] => raise ex
      | x::[] => x
      | x::xs' => max(x, maxlist(xs', ex))

val w = maxlist([2,3,4,6], MyUndesirableCondition)
val x = maxlist([3,4,5], MyUndesirableCondition)
        handle MyUndesirableCondition => 42
val z= maxlist([], MyUndesirableCondition)
       handle MyUndesirableCondition => 42
