fun good_max(xs: int list) = 
    if null xs
    then 0
    else if null (tl xs)
    then hd xs
    else 
        let val ans = good_max(tl xs)
        in 
            if hd xs > ans
            then hd xs
            else ans
        end
