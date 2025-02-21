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
