(* ベンチマーク用の関数. 実行時間の計測にはこれをコピーして利用してください *)
let bench f =
  let t = Sys.time () in
  ignore (f ()); Printf.printf "Time Elapsed: %f ms\n" (Sys.time () -. t)

(* ベンチマーク対象の関数例; たらい回し関数 (竹内関数) *)
let rec tarai x y z =
  if x <= y then y
  else tarai (tarai (x - 1) y z) (tarai (y - 1) z x) (tarai (z -1) x y)

(* 利用例 *)
(* let _ = bench (fun () -> tarai 12 6 0);;
let _ = bench (fun () -> tarai 13 7 0);; *)

(* 非末尾再帰で x の n 乗を計算する*)
let rec power_1 x n =
  if n = 0 then 1
  else x * power_1 x (n - 1)

(* 末尾再帰で x の n 乗を計算する*)
let power_2 x n =
  let rec power_aux x t n =
    if n = 0 then t
    else power_aux x (x * t) (n - 1)
  in
    power_aux x 1 n

(* 第一引数のリストを逆順にして、第二引数のリストと一つにする *)
let rec rev_append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h::t -> rev_append t (h::lst2)

(* 末尾再帰ではない *)
let rec foo1 n1 n2 =
  if n1 = n2 then 0.0
  else let r1 = float n1 in
    (sqrt r1) +. (sin r1) +. (cos r1) +. (foo1 (n1 + 1) n2)

(* 末尾再帰 *)
let foo2 n1 n2 =
  let rec foo2_aux n1 n2 m =
    if n1 = n2 then m
    else let r1 = float n1 in
      foo2_aux (n1 + 1) n2 (m +. (sqrt r1) +. (sin r1) +. (cos r1))
  in
    foo2_aux n1 n2 0.0

(* テスト *)
let test1 = power_1 2 3;;
let test2 = power_2 2 3;;

let test3 = rev_append [1; 2; 3] [4; 5; 6];;
let test4 = rev_append [7; 8; 9] [10; 11; 12];;

let test3 = bench(fun () -> power_1 2 15);;
let test4 = bench(fun () -> power_2 2 15);;
let test5 = bench(fun () -> foo1 5 8);;
let test6 = bench(fun () -> foo2 5 8);;
(*
Time Elapsed: 0.000007 ms
val test3 : unit = ()
Time Elapsed: 0.000006 ms
val test4 : unit = ()
Time Elapsed: 0.000011 ms
val test5 : unit = ()
Time Elapsed: 0.000003 ms
val test6 : unit = ()
*)
