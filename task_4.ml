(*指定した値より大きい、最大のリストの要素を返す*)
let rec compare_list x list = 
    match list with
    | [] -> x
    | h::t -> if h > x
        then compare_list h t
        else compare_list x t

(*問題①: リストの最大値の要素を返す*)
let max_list list =
    match list with
    | [] -> 0
    | h::t -> compare_list h t

(*問題②: 0以外の要素の個数を返す*)
let rec num_nonzero list = 
    match list with
    | [] -> 0
    | h::t -> if h = 0
        then num_nonzero t
        else 1 + num_nonzero t

(*リストから要素を削除したリストを返す*)
let rec remove x list =
    match list with
    | [] -> []
    | h::t -> if h = x
        then t
        else h::(remove x t)

(*問題③: 相異なる整数のリストの中で2番目に大きな値を返す*)
(*最大値を削除したリストの最大値を返す*)
let second_largest list = 
    max_list(remove (max_list list) list)

(*問題④: リストの最後の要素を返す*)
let rec last list =
    match list with
    | [] -> failwith "空リストは引数に出来ない"
    | h::[] -> h
    | h::t -> last t

(*リストの要素の総和を返す*)
let rec sumlist list = 
    match list with
    | [] -> 0
    | h::t -> h + sumlist t;;

(*問題⑤: リストのリストの各リストの総和のリストを返す*)
let rec sum_list list = 
    match list with
    | [] -> []
    | h::t -> sumlist h :: sum_list t

(*発展問題1: 昇順に並んでいる 2 つの整数リストを昇順にマージする*)
let rec merge list1 list2 =
    match (list1, list2) with
    | ([], []) -> []
    | ([], h2::t2) -> list2
    | (h1::t1, []) -> list1
    | (h1::t1, h2::t2) -> if h1 < h2
            then h1::merge t1 list2
            else h2::merge list1 t2

(*最大値の要素を取り除いたリストを返す*)
let remove_max list  = remove (max_list list) list

(*発展問題2: 上位n番目の要素を返す*)
let rec nth_top n list =
    if n > 1
        then match list with
            | [] -> failwith "指定番号がリストの要素を上回っている"
            | h::t -> nth_top (n-1) (remove_max list) 
        else match list with
            | [] -> failwith "指定番号がリストの要素を上回っている"
            | h::t -> max_list list

(*テスト*)
let test1 = max_list [1; 5; 0; 4; 1; 0];;
let test2 = num_nonzero [1; 5; 0; 4; 1; 0];;
let test3 = remove 6 [1; 6; 3; 2; 4; 0];;
let test4 = second_largest [1; 6; 3; 2; 4; 0];;
let test5 = last [1; 6; 3; 2; 4; 0];;
let test6 = sum_list [[1;2;3]; [4;5]; [6;7;8;9;10]];;
let test7 = merge [1;3;5][2;3;10;15];;
let test8 = nth_top 3 [1;7;5;3;4];;
let test9 = nth_top 1 [4; 5; 2];;
let test10 = nth_top 4 [1; 2; 3];;

