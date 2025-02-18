fun is_older(ymd1 : int  * int * int, ymd2 : int * int * int) = 
    if #1 ymd1 < #2 ymd2
    then true
    else if #2 ymd1 < #2 ymd2
    then true
    else if #3 ymd1 < #3 ymd2
    then true
    else false

fun number_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else (if (#2 (hd dates)) = month then 1 else 0) 
        + number_in_month(tl dates, month)

fun number_in_months(dates : (int*int*int) list, month : int list) = 
    