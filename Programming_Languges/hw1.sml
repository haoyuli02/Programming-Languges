fun is_older (date1 : int * int * int, date2 : int * int * int) =
    if #1 date1 < #1 date2
    then true
    else 
        if #1 date1 = #1 date2
        then 
            if #2 date1 < #2 date2
            then true
            else 
                if #2 date1 = #2 date2
                then 
                    if #3 date1 < #3 date2
                    then true
                    else false
                else false
        else false

fun number_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else
        let
            val rest = number_in_month((tl dates), month)
        in
            if #2 (hd dates) = month
            then 1 + rest
            else rest
        end

fun number_in_months(dates : (int * int * int) list, months : int list) = 
    if null months
    then 0
    else number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))

fun dates_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else 
        let 
            val rest = dates_in_month((tl dates), month)
            val curr = hd dates
        in
            if #2 curr = month
            then curr::rest
            else rest
        end

fun dates_in_months(dates : (int * int * int) list, months : int list) = 
    if null months
    then []
    else (dates_in_month(dates, hd months)) @ dates_in_months(dates, (tl months)) 

fun get_nth(some_list : string list, n : int) =
    if n = 1
    then hd some_list
    else get_nth((tl some_list), n - 1)

fun date_to_string(date : int * int * int) =
    let
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
        val year = Int.toString(#1 date)
        val day = Int.toString(#3 date)
    in
        get_nth(months, #2 date) ^ " " ^ day ^ ", " ^ year 
    end

fun number_before_reaching_sum(sum : int, numbers : int list) =
    if sum <= hd numbers
    then 0
    else 1 + number_before_reaching_sum(sum - hd numbers, tl numbers)

fun what_month(day : int) =
    let
        val day_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day, day_in_month) + 1
    end

fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1, day2)

fun oldest(dates : (int * int * int) list) =
    if null dates
    then NONE
    else
        let
            val rest_old = oldest(tl dates)
        in
            if isSome rest_old
            then 
                if is_older(hd dates, valOf rest_old)
                then SOME (hd dates)
                else rest_old
            else SOME (hd dates)
        end

fun inside (input : int list, num : int) =
    if null input
    then false
    else if hd input = num
        then true
        else inside(tl input, num)

fun strip_rep(input : int list) =
    if null input
    then []
    else
        if inside(tl input, hd input)
        then strip_rep(tl input)
        else (hd input)::strip_rep(tl input)

fun number_in_months_challenge(dates : (int * int * int) list, months : int list) =
    let
        val uniq_months = strip_rep(months)
    in
        number_in_months(dates, uniq_months)
    end

fun dates_in_months_challenge(dates : (int * int * int) list, months : int list) =
    let
        val uniq_months = strip_rep(months)
    in
        dates_in_months(dates, uniq_months)
    end

fun reasonable_date(date : int * int * int) =
    let
        val year = #1 date
        val month = #2 date
        val day = #3 date
        val is_leap = (year mod 400 = 0) orelse ((year mod 4 = 0) andalso (not (year mod 100 = 0)))
        val day_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val leap_day_in_month = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        fun get_nth_int(some_list : int list, n : int) =
            if n = 1
            then hd some_list
            else get_nth_int((tl some_list), n - 1)
    in
        if (year <= 0) orelse ((month <= 0) orelse (month > 12))
        then false
        else 
            if is_leap
            then (day >= 0) andalso (day <= get_nth_int(leap_day_in_month, month))
            else (day >= 0) andalso (day <= get_nth_int(day_in_month, month))
    end