fun alternate(xs : int list) =
    let
        fun helper(xs: int list, modifier : int) =
            if null xs
            then 0
            else (hd xs * modifier)
                 + helper(tl xs, ~modifier)
    in
        helper(xs, 1)
    end

(*assumed list is not null and len > 2*)
fun min_max(xs : int list) =
    let
        fun min_max1(xs: int list, min: int, max: int) =
            if null xs
            then (min, max)
            else let
                val minimum = if hd xs < min then hd xs else min
                val maximum = if hd xs > max then hd xs else max
            in
                min_max1(tl xs, minimum, maximum)
            end
    in
        min_max1(xs, hd xs, hd xs)
    end

(* [1,4,20] [1,5,25] *)
fun cumsum(xs : int list) =
    let
        fun helper(xs : int list, sum : int) =
            if null xs
            then []
            else (sum + (hd xs))
                 :: helper(tl xs, (sum + (hd xs)))
    in
        helper(xs, 0)
    end

fun repeat(xs: int list, count : int list) =
    if null xs then []
    else
        let
            fun repeat_element(elem : int, count : int) =
                if count = 0 then []
                else elem :: repeat_element(elem, count - 1)
        in
            repeat_element(hd xs, hd count) @ repeat(tl xs, tl count)
        end

fun zip(xs : int list, ys : int list) =
    if null xs orelse null ys
    then []
    else (hd xs, hd ys) :: zip (tl xs, tl ys)
