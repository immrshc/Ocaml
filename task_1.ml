(*演習問題1*)

(*目的：a - b - c の実行順序を確認する*)
let subtract a b c = 
    (print_string "a\n"; a) - (print_string "b\n"; b) - (print_string "c\n";c);;

(*目的：a + b + c の実行順序を確認する*)
(*verb: int -> int -> int -> int*)
let verb a b c = 
    (print_string "a\n"; a) + (print_string "b\n"; b) + (print_string "c\n";
    c);;

(*目的：関数適用と四則演算の優先度を確認する*)
(*f: int -> int*)
let f x = x * 2;;

(*g: int -> int -> string*)
let g x y = 
    let z = f x + y in
    if z = f(x) + y 
        then print_string"f(x) + y: 関数適用が四則演算より優先される\n"
        else print_string"f(x + y): 関数適用より四則演算が優先される\n";;

(*目的：入れ子型のif文の適応範囲を調べる*)
(*左結合なら、x < y の時"if(else): 左結合"が出力される*)
(*右結合なら、x > y の時"if(if(else)): 右結合"が出力される*)
(*check_scope: int -> bool*)
let check_scope x y = 
    if x > y
        then if x > infinity
            then () 
            else (if x > y 
                then print_string"if(if(else)): 右結合\n"
                else print_string"if(else): 左結合\n");;

(*目的：関数適用を確かめるために、引数が整数一つの関数を定義する*)
(*h: int -> int*)
let h x = x * 6;;

(*テスト*)
(*問題①*)
print_string"a-b-cの計算の実行順に変数を表示する\n";;
let test1 = subtract 1 2 3;;
print_newline();;
(* 実行結果①
a-b-cの計算の実行順に変数を表示する
- : unit = ()
c
b
a
val test1 : int = -4

以上より, 右から順に参照されているので、(a-(b-c))のように括弧付けられている
*)

print_string"verbの計算の実行順に変数を表示する\n";;
let test2 = verb 1 2 3;;
print_newline();;
(*　実行結果②
verbの計算の実行順に変数を表示する
- : unit = ()
c
b
a
val test2 : int = 6

以上のようにprint_stringで評価されている変数の順番に出力することで、括弧付けの順番を確かめる
*)

(*問題②*)
print_string"f 10 + 3 を実行する\n";;
let test3 = f 10 + 3;;
print_newline();;
(* 実行結果③
f 10 + 3 を実行する
- : unit = ()
val test3 : int = 23
*)

print_string"関数適用と四則演算の優先度の確認\n";;
let test4 = g 10 3;;
print_newline();;
(* 実行結果④
関数適用と四則演算の優先度の確認
- : unit = ()
f(x) + y: 関数適用が四則演算より優先される
val test4 : unit = ()

以上のようにf x y = f(x) + y より関数適用が四則演算より優先される
*)

(*問題③*)
print_string"if文のelseの結合順を調べる\n";;
let test5 = check_scope 10.0 3.0;;
print_newline();;
(* 実行結果⑤
if文のelseの結合順を調べる
- : unit = ()
if(if(else)): 右結合
val test5 : unit = ()

*)

print_string"if文のelseの結合順位を調べる\n";;
let test6 = check_scope 4.0 7.0;;
print_newline();;
(* 実行結果⑥
if文のelseの結合順位を調べる
- : unit = ()
val test6 : unit = ()

以上のように、右結合なら、x > y の時"if(if(else)): 右結合"が出力されている。
一方で、左結合なら、x < y の時"if(else): 左結合"が出力されるが、それは出力されていない。
よって、if文のelseは左隣のif文に優先されることがわかる。
*)

(*問題④*)
print_string"関数適用が右結合かどうか確かめる\n";;
let test7 = h f 3 = h(f 3);;
print_newline();;
(* 実行結果⑦
関数適用が右結合かどうか確かめる
- : unit = ()
File "task_1.ml", line 65, characters 12-13:
Error: This function has type int -> int
       It is applied to too many arguments; maybe you forgot a `;'.

以上より、h f 3 = (h(f) 3) となっており、関数同士の優先順位は、左結合であることがわかる。
*)
