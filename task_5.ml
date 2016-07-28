(*整数型に「正の無限大」と「負の無限大」を追加したもの*)
type intplus_1 =
    | Fin of int
    | Inf of bool

(*問題 1-1：intplus_1型の要素に対する「足し算」をあらわす関数 *)
let rec addplus_1 (n:intplus_1 * intplus_1):intplus_1 =
    match n with
    | (Fin(i), Fin(j)) -> Fin(i + j)
    | (Fin(i), Inf(j)) -> Inf(j)
    | (Inf(i), Fin(j)) -> Inf(i)
    | (Inf(i), Inf(j)) -> if i = j
                            then Inf(i)
                            else failwith "不定"

(*問題 1-2：intplus_1型の要素に対する「掛け算」をあらわす関数 *)
let rec timeplus_1 (n:intplus_1 * intplus_1):intplus_1 =
    match n with
    | (Fin(i), Fin(j)) -> Fin(i * j)
    | (Fin(i), Inf(j)) -> Inf(j)
    | (Inf(i), Fin(j)) -> Inf(i)
    | (Inf(i), Inf(j)) -> if i = j
                            then Inf(i)
                            else failwith "不定"

(*「不定」を含む整数を拡張した型*)
type intplus_2 =
  | Fin of int
  | Inf of bool
  | Indefinite

(*発展問題 1-1：「不定」を含む整数を拡張した型の足し算*)
let rec addplus_2 (n:intplus_2 * intplus_2):intplus_2 =
    match n with
    | (Fin(i), Fin(j)) -> Fin(i + j)
    | (Fin(i), Inf(j)) -> Inf(j)
    | (Inf(i), Fin(j)) -> Inf(i)
    | (Inf(i), Inf(j)) -> if i = j
                            then Inf(i)
                            else Indefinite

(*発展問題 1-2：「不定」を含む整数を拡張した型の掛け算*)
let rec timeplus_2 (n: intplus_2 * intplus_2): intplus_2 =
    match n with
    | (Fin(i), Fin(j)) -> Fin(i * j)
    | (Fin(i), Inf(j)) -> Inf(j)
    | (Inf(i), Fin(j)) -> Inf(i)
    | (Inf(i), Inf(j)) -> if i = j
                            then Inf(i)
                            else Indefinite

(*二分木のデータ型の定義*)
type binary_tree =
  | Leaf
  | Node of int * binary_tree * binary_tree

(*問題 2-1：2分木(binary tree 型のデータ)におけるノードの個数(Nodeというタグの個数)を計算する*)
let rec size (bt: binary_tree):int =
    match bt with
    | Leaf -> 0
    | Node(n, bt1, bt2) -> 1 + size bt1 + size bt2

(*二つのリストを一つのリストにする*)
let rec merge list1 list2 =
    match (list1, list2) with
    | ([], []) -> []
    | ([], _) -> list2
    | (_, []) -> list1
    | (h1::t1, h2::t2) -> h1 :: merge t1 list2

(*問題 2-2：2分木(binary tree 型のデータ)を、inorder (中間順、通りがけ順) で走査して得られる整数値たちを、リストにして返す*)
let rec flatten (bt: binary_tree): int list =
    match bt with
    | Leaf -> []
    | Node(n, bt1, bt2) -> match (bt1, bt2) with
        | (Node(_, _, _), Node(_, _, _)) -> merge (flatten bt1) (n :: flatten bt2)
        | (Leaf, Node(_, _, _)) -> n :: flatten bt2
        | (Node(_, _, _), Leaf) -> merge (flatten bt1) (n :: [])
        | (Leaf, Leaf) -> n :: []

(*二分木の高さ*)
let rec height (bt: binary_tree): int =
  match bt with
    | Leaf -> 0
    | Node(_,bt1,bt2) ->
      let h1 = height bt1 in
      let h2 = height bt2 in
        if h1 > h2 then h1 + 1
        else h2 + 1

(*問題 2-3：2分木(binary tree 型のデータ)が、バランス木であるかどうかを判定する*)
let rec is_balanced (bt: binary_tree): bool =
  match bt with
  | Leaf -> true
  | Node(_, bt1, bt2) ->
    if is_balanced bt1 && is_balanced bt2
      then let h1 = height bt1 in let h2 = height bt2 in
        abs(h1 - h2) <= 1
      else false

(*発展課題 2：2分木(binary tree 型のデータ)が、整列された木であるかどうかを判定する*)
let rec is_sorted (bt: binary_tree): bool =
  match bt with
  | Leaf -> true
  | Node(y, bt1, bt2) ->
    if is_sorted bt1 && is_sorted bt2 then match (bt1, bt2) with
      | (Node(x, _, _), Node(z, _, _)) -> x <= y && y <= z
      | (Node(x, _, _), Leaf) -> x <= y
      | (Leaf, Node(z, _, _)) -> y <= z
      | (Leaf, Leaf) -> true
    else false

(*整数と四則演算からなる「式」*)
type expr =
  | Const of int
  | Add of expr * expr
  | Sub of expr * expr
  | Times of expr * expr
  | Div of expr * expr

(*発展課題 3-1：式(expr 型のデータ)が、何個の乗算を含むかを返す*)
let rec count_times(e: expr): int =
  match e with
  | Const(i) -> 0
  | Add(e1, e2) -> count_times e1 + count_times e2
  | Sub(e1, e2) -> count_times e1 + count_times e2
  | Times(e1, e2) -> 1 + count_times e1 + count_times e2
  | Div(e1, e2) -> count_times e1 + count_times e2

(*発展課題 3-2：e·0 = 0 という法則に従って、式(expr 型のデータ)を簡略化する変換をする*)
let rec opt(e: expr): int =
match e with
| Const(i) -> i
| Add(e1, e2) -> opt e1 + opt e2
| Sub(e1, e2) -> opt e1 - opt e2
| Times(e1, e2) ->
  if opt e1 = 0 || opt e2 = 0 then 0
  else opt e1 * opt e2
| Div(e1, e2) -> opt e1 / opt e2

(*テスト*)
let test1 = addplus_1(Fin(4), Fin(6));;
let test2 = addplus_1(Inf(true), Inf(true));;
let test3 = addplus_1(Inf(true), Fin(1));;
(*
let test4 = addplus_1(Inf(false), Inf(true));;
*)

let test5 = timeplus_1(Fin(4), Fin(6));;
let test6 = timeplus_1(Inf(true), Inf(true));;
let test7 = timeplus_1(Inf(true), Fin(1));;
(*
let test8 = timeplus_1(Inf(false), Inf(true));;
*)

let test9 = addplus_2(Inf(false), Inf(true));;

let test10 = timeplus_2(Inf(false), Inf(true));;

let test11 = size (Node(12, Leaf, Leaf));;
let test12 = size (Node(2, Node(3, Leaf, Leaf), Node(0, Leaf, Leaf)));;

let test13 = flatten (Node(12, Leaf, Leaf));;
let test14 = flatten (Node(2, Node(3, Leaf, Leaf), Node(0, Leaf, Leaf)));;
let test15 = flatten (Node(1,Node(2, Node(3, Leaf, Leaf), Node(4, Leaf, Leaf)), Node(5, Leaf, Leaf)));;

let test16 = is_balanced (Node(1,Node(2,Node(3,Leaf,Leaf),Leaf),Leaf));;
let test17 = is_balanced (Node(1,Node(2,Node(3,Leaf,Leaf),Leaf),Node(4,Leaf,Leaf)));;

let test18 = is_sorted(Node(4,Node(2,Node(5,Leaf,Leaf),Node(3,Leaf,Leaf)),Node(5,Leaf,Leaf)));;
let test19 = is_sorted(Node(4,Node(2,Node(1,Leaf,Leaf),Node(3,Leaf,Leaf)),Node(5,Leaf,Leaf)));;

let test20 = count_times(Add(Const(10),Times(Const(20),Times(Const(30),Const(40)))));;

let test21 = opt(Add(Const(10),Times(Const(20),Const(0)))) = opt(Add(Const(10),Const(0)));;
