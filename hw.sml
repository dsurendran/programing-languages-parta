(* fun is_older1(ymd1 : int  * int * int, ymd2 : int * int * int) = *)
(*     if #1 ymd1 < #2 ymd2 *)
(*     then true *)
(*     else if #2 ymd1 < #2 ymd2 *)
(*     then true *)
(*     else if #3 ymd1 < #3 ymd2 *)
(*     then true *)
(*     else false *)

fun is_older(date1 : int*int*int, date2 : int*int*int) =
  if (#1 date1 ) <> (#1 date2)
  then (#1 date1) < (#1 date2)
  else if (#2 date1) <> (#2 date2)
  then (#2 date1) < (#2 date2)
  else (#3 date1) < (#3 date2)

fun number_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else (if (#2 (hd dates)) = month then 1 else 0)
        + number_in_month(tl dates, month)

fun number_in_months(dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months)
        + number_in_months(dates, tl months)

fun dates_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else if (#2 (hd dates)) = month
        then (hd dates)::dates_in_month(tl dates, month)
        else  dates_in_month(tl dates, month)

fun dates_in_months(dates: (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @
        dates_in_months(dates, tl months)

fun get_nth(strs : string list, n : int) =
    if n = 1
    then hd strs
    else get_nth(tl strs, n - 1)

fun date_to_string(y : int, m : int, d : int) =
    let
      fun get_month(m) = get_nth(["January", "February", "March", "April", "May",
                "June", "July", "August", "September", "October", "November", "December"], m)
    in
      get_month(m) ^ " " ^ Int.toString(d) ^ ", " ^ Int.toString(y)
    end

(* (10, [1,2,3,4,5]) = 3 *)
fun number_before_reaching_sum(sum : int, numbers : int list) =
  if hd numbers >= sum
  then 0
  else 1 + number_before_reaching_sum(sum - (hd numbers), tl numbers)

fun what_month(day : int) =
  let
    val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  in
     1 + number_before_reaching_sum(day, months)
  end

fun month_range(day1 : int, day2 : int) =
  if day1 = day2
  then [what_month(day1)]
  else what_month(day1) :: month_range(day1+1, day2)

(* oldest([(2012,2,28),(2011,3,31), (2011,4,28)]) *)
fun oldest(dates : (int * int * int) list) =
  if null dates
  then NONE
  else if null (tl dates)
  then SOME(hd dates)
  else if is_older(hd dates, hd(tl dates))
  then SOME(hd dates)
  else oldest(tl dates)
