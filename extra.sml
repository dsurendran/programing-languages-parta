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
