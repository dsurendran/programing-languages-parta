fun fact n =
    if n = 0
    then 1
    else n * fact(n-1)

(* uncessarily to keep the stack if there is nothing to evaluate *)    
(* other than returning the results *) 
(* We never build up the stack *)

fun fact2 n =
    let fun aux(n, acc) =
            if n = 0
            then acc
            else aux(n-1, acc * n)
    in
        aux(n, 1)
    end

(* role of accumulator *)      

fun sum xs =
    let fun aux([], acc) = acc
          | aux(x::xs', acc) = aux(xs', acc + x) 
    in  
        aux(xs, 0)
    end

fun reverse xs =
    case xs of
        [] => []
      | x::xs' => (reverse xs') @ [x]

fun reverse_tail xs =
    let
        fun aux ([], acc) = acc
          | aux (x::xs', acc) = aux(xs', x::acc) 
    in
        aux(xs, [])
    end
