fun sort_pair(pr: int * int) = 
    if #1 pr < #2 pr
    then pr
    else (#2 pr, #1 pr)

fun sort_pair(pr: int * int) = 
    if #1 pr < #2 pr
    then (#1 pr, #2 pr)
    else (#2 pr, #1 pr)

fun append(xs: int list, ys : int list) = 
    if null xs
    then ys
    else hd xs :: append(tl xs, ys)
