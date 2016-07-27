(**)

let isqrt x = truncate(sqrt(float_of_int x));;

let le_sqrt m n = m * m <= n;;

let is_isqrt m n = (le_sqrt m n) && not(le_sqrt (m+1) n);;


let rec f m n = 
    (*m * m < n でないと停止しない*)
    if le_sqrt m n
        then if not(le_sqrt (m+1) n)
            then m
            else let m = m+1 in f m n
        else -1;;

let isqrt_2 x = 
    (*isqrt x を求める*)
    let m = isqrt x in 
    (*is_isqrtで検証する*)
    (*間違っていたらisqrt xを増分して再検証*)
    f m x;;

let rec bit_num x = 
    if 0 = 0b1 lsl x
        then x-1
        else bit_num (x+1)

(**)
let test1 = isqrt 4;;
let test2 = isqrt 6;;
let test3 = isqrt 10;;
let test4 = le_sqrt (isqrt 5) 6;;
let test5 = is_isqrt (isqrt 5) 6;;
let test6 = f 2 17;;
let test7 = f 11 4;;
let test8 = isqrt_2 17;;
let test9 = bit_num 0;;

(*
* max_int
* 0b011111111111111111111111111111111111111111111111111111111111111;;
* min_int
* 0b100000000000000000000000000000000000000000000000000000000000000;;
* ZERO
* 0b000000000000000000000000000000000000000000000000000000000000000;;
* val test1 : int = 2
* val test2 : int = 2
* val test3 : int = 3
* val test4 : bool = true
* val test5 : bool = true
* val test6 : int = 4
* val test7 : int = -1
* val test8 : int = 4
* val test9 : int = 62
* )
