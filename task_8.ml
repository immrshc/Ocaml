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

let rec sum_aux x s =
  if x = 0 then s
  else sum_aux (x - 1) (s + x);;

let sum2 x = sum_aux x 0 in sum2 10;;

(* append は末尾再帰でないが、これは準備である *)
let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h::t -> h::(append lst1 lst2) ;;

(* 末尾再帰でない reverse の定義 *)
let rec reverse lst =
  match lst with
  |[] -> []
  | h::t -> append (reverse t) [h] ;;
(* 末尾再帰による reverse の定義 *)
let rec reverse_tailrec lst acc =
  match lst with
  | [] -> acc
  | h::t -> reverse_tailrec t (h :: acc) ;;

let reverse2 lst = reverse_tailrec lst [] ;;
