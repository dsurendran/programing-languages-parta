fun countup (from : int, to : int) =
    if from = to
    then to::[]
    else from :: countup(from + 1, to)

fun countdown (from : int, to : int) =
    if from = to
    then to::[]
    else from :: countdown(from - 1, to)
                          
fun bad_max (xs : int list) =
    if null xs
    then 0
    else if null (tl xs)
    then hd xs
    else if hd xs  > bad_max(tl xs)
    then hd xs
    else bad_max(tl xs)

fun old_max (xs: int list) =
    if null xs
    then 0
    else if null (tl xs)
    then hd xs
    else
        let val tl_ans = old_max(tl xs)
        in
            if hd xs > tl_ans
            then hd xs
            else tl_ans
        end
            
            (* fn: int list -> int option *)
fun good_max1 (xs: int list) =
    if null xs
    then NONE
    else
        let
            val tl_ans = good_max1(tl xs)
        in
            if isSome tl_ans andalso valOf tl_ans > hd xs
            then tl_ans
            else SOME(hd xs)
        end

fun good_max2 (xs : int list) =
    if null xs
    then NONE
    else
        let
            fun max_nonempty(xs : int list) =
                if null tl xs
                        
                then hd xs
                else let val tl_ans = good_max2(tl xs)
                     in
                         if hd_xs > tl_ans
                         then SOME hd_xs
                         else tl_ans
                     end
        in
            SOME (max_nonempty xs)
        end
            
