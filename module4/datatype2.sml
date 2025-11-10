(*Recursive datatypes*)
datatype Expression =
         Constant of int
         | Negate of Expression
         | Add of Expression * Expression
         | Multiply of Expression * Expression

fun max_constant e =
    case e of
        Constant i => i
      | Negate e2 => max_constant e2
      | Add(e1, e2) => Int.max(max_constant e1, max_constant e2)
      | Multiply (e1, e2) => Int.max(max_constant e1, max_constant e2)

val test_exp = Add (Constant 19, Negate (Constant 4))
val nineteen = max_constant test_exp
