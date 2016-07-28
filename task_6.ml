let add3 x = x + 3;;
let mul2 x = x * 2;;

let rec my_map f lst =
  match lst with
  | [] -> []
  | h::t -> (f h) :: (my_map f t)

let _ = my_map add3 [1; 3; 5; 3; 2];;

(* 演習問題 1 *)
(* (int -> bool) -> (int list -> bool list) *)
(* 'a = int および 'b = bool *)
let _ = my_map (fun x -> (x > 3)) [1; 5; 8; 2; 4];;
(* (int -> string) -> (int list -> string list) *)
(* 'a = int および 'b = string *)
let _ = my_map string_of_int [1; 5; 8; 2; 4];;

let hoo1 x y = x + y;;
let hoo2 x y = x * y;;
let hoo3 x y = if x > y then x else y;;
let hoo5 x y = if x > 4 then x else y;;
let hoo6 x y = x * 2 + y ;;
let hoo7 x y = x ^ (string_of_int y) ;;
let hoo8 x y = x ^ (string_of_int y) ^ (string_of_int y) ;;
(* 左畳み込み : リストの要素を全部まとめて一つにする *)
(* 左畳み込みは、左のリストの要素から引数にされていく *)
(* 畳み込みは第一引数の型と、返り値の型が一致している必要がある *)

(* 演習問題 2 *)
(* ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)

(* (int -> int -> int) -> int -> int list -> int *)
(* 'a = int および 'b = int *)
let _ = List.fold_left hoo1 0 [1; 5; 8; 2; 4]
  = hoo1 (hoo1 (hoo1 (hoo1 (hoo1 0 1) 5) 8) 2) 4;;

(* (int -> int -> int) -> int -> int list -> int *)
(* 'a = int および 'b = int *)
let _ = List.fold_left hoo2 1 [1; 5; 8; 2; 4]
  = 1 * 1 * 5 * 8 * 2 * 4;;

(* (int -> int -> int) -> int -> int list -> int *)
(* 'a = int および 'b = int *)
let _ = List.fold_left hoo3 min_int [1; 5; 8; 2; 4] = 8;;

(* (int -> int -> int) -> int -> int list -> int *)
(* 'a = int および 'b = int *)
let _ = List.fold_left hoo5 0 [1; 5; 8; 2; 4] = 5;;

(* リストを二進数とおもって、十進数に変換する。
たとえば [1; 0; 1; 1] を 2^3 + 2 + 1 = 11 に変換する *)
(* (int -> int -> int) -> int -> int list -> int *)
(* 'a = int および 'b = int *)
let _ = List.fold_left hoo6 0 [1; 0; 1; 1] ;;

(* fold で壊した分を、文字列でくっつける *)
(* (string -> int -> string) -> string -> int list -> string *)
(* 'a = string および 'b = int *)
let _ = List.fold_left hoo7 "" [1; 5; 8; 2; 4] ;;

(* もらってきたものを つの要素にして連結する 2 *)
(* (string -> int -> string) -> string -> int list -> string *)
(* 'a = string および 'b = int *)
let _ = List.fold_left hoo8 "" [1; 5; 8; 2; 4] ;;
