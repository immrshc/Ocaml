(*mの累乗を返す*)
let f m = m * m

(*m**2 <= n < (m+1)**2となるmを返す*)
let rec g m n = 
    if f m <= n && n < f(m+1) 
        then m
        else g (m+1) n

(*問題1: nの整数上の平方根を返す*)
let isqrt n = g 0 n 

(*問題2: nのk乗を返す*)
let rec power n k = 
    if k = 1
        then n
        else (power n (k-1)) * n

(*問題3: nがkで割り切れる回数を返す*)
let rec factor n k = 
    if n mod k = 0
        then 1 + factor (n/k) k
        else 0

(*n が k 以下の全ての整数で割れるか確認する*)
let rec is_prime_like n k = 
    if k < 2
        then true
        else (n mod k <> 0) && (is_prime_like n (k-1))

(*素数判定をする*)
let is_prime n = 
    is_prime_like n (n-1)

(*m 以上の n の素因数の個数を返す*)
let rec check_prime_factor  m n =
    if m <= n 
        then if is_prime m && n mod m = 0
            then 1 + check_prime_factor (m+1) n
            else check_prime_factor (m+1) n
        else 0

(*問題4: 素因数の個数を求める*)
let prime_factor n = 
    check_prime_factor 2 n

(*問題5: 二進数で表した際のbit数*)
let rec bit_num n = 
    if n > 1  
        then if n mod 2 = 0
            then 1 + bit_num (n/2)
            else 1 + bit_num ((n-1)/2)
        else 1

(*n の階乗を求める*)
let rec factorial n = 
    if n = 0
        then 1
        else factorial(n-1) * n

(*マクローリン展開の第n項を求める*)
let maclaurin_section n =
    if n > 0
        then 1. /. float(factorial(n-1))
        else 0.

(*第m項から第n項までのマクローリン展開を求める*)
let rec maclaurin m n = 
    if m <= n 
        then maclaurin_section m +. maclaurin (m+1) n
        else 0.

(*発展問題1: 自然対数の底 e のマクローリン展開*)
let maclaurin_e n = maclaurin 1 n

(*arc tanの第n項を求める*)
let arc_tan_section x n = 
    let s = float(n - 1) in
    let t = float(2 * n - 1) in
    if n > 0
        then ((-1.)**s) *. (x**t) /. t
        else 0.

(*第m項から第n項までのarc tanを求める*)
let rec arc_tan x m n = 
    if m <= n
        then arc_tan_section x m +. arc_tan x (m+1) n
        else 0.

(*発展問題2: 第n項までの arc tan を使って円周率を求める*)
let arc_tan_pai n = 4. *. arc_tan (1. /. 5.) 1 n -. arc_tan (1. /. 239.) 1 n

(*テスト*)
let test1 = isqrt 16;;
let test2 = isqrt 30;;

let test3 = power 2 3;;
let test4 = power 3 4;;

let test5 = factor 80 2;;
let test6 = factor 80 3;;
let test7 = factor 80 5;;
let test8 = is_prime 3;;
let test9 = is_prime 6;;
let test10 = prime_factor 80;;
let test11 = prime_factor 1;;

(*8 = 1000*)
let test12 = bit_num 8;;
(*160 = 10100000*)
let test13 = bit_num 160;;
(*22 = 10110*)
let test14 = bit_num 22;;

let test15 = maclaurin_e 1;;
let test16 = maclaurin_e 4;;
let test17 = 1. +. 1. +. 0.5 +. 1. /. 6.;;
let test18 = maclaurin_e 10;;
let test19 = maclaurin_e 20;;
let test20 = exp 1.;;

let test21 = arc_tan 1. 1 5;;
let test22 = arc_tan 2. 1 10;;
let test23 = arc_tan_pai 3;;
let test24 = arc_tan_pai 15;;
let test25 = 3.14159265359 /. 4.;; 

