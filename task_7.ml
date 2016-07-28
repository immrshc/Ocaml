(* 課題7 *)
(* (1) List.fold_left または List.fold_right を使って、与えられた整数リストの最小値を求める関数 min を書きなさい。
ただし、max_int が最大の整数であることを使ってよい。 *)
let min (lst: int list): int =
  match lst with
  | [] -> max_int
  | h::[] -> h
  | h::t -> List.fold_left (fun x y -> if x > y then y else x) h t

(* (2) List.fold_left または List.fold_right を使って、与えられた文字列のリストに対して、
辞書式順序で最後に来る文字列を求める関数 dic_last を書きなさい。
ただし、空文字列 (””) が、辞書式順序で最初にくることを使ってよい。
また、OCaml では文字列 s1,s2 に対して、 s1 < s2 とすると辞書式順序での比較ができることを使ってよい。 *)
let dic_last (lst: string list): string =
 match lst with
 | [] -> ""
 | h::[] -> h
 | h::t -> List.fold_left (fun x y -> if x > y then x else y) h t

(* (3) リストに対する高階関数 List.iter (iterator の意味) と同等の動きをする関数 my_iter を定義せよ。 *)
let rec my_iter f lst =
  match lst with
  | [] -> ()
  | h::t -> begin
      f h;
      my_iter f t
    end

(* (4) (発展課題) リストに対する高階関数 List.fold_right と同等の動きをする関数 my_fold_right を定義せよ。 *)
let rec my_fold_right f lst x =
  match lst with
  | [] -> x
  | h::t -> f h (my_fold_right f t x)

(* (5) (発展課題) 関数 f : float -> float と、float 型の値 v を与えられたとき、
x における f の微分 f’(v) の近似値を求める関数 deriv を定義せよ。 *)
let deriv f v =
  let rec delta d =
    let x = (f (v +. d) -. f v) /. d in
    let y = (f v -. f (v -. d)) /. d in
      if (x -. y < 0.000001 && x -. y > 0.0) then x
      else if (y -. x < 0.000001 && y -. x > 0.0) then y
      else delta (d /. 2.0)
  in
    delta 1.0

(* (6) (発展課題) OCaml のリストモジュールには、List.sort 関数が用意されており、
これは、任意の順序で、ソート (整列) をお こなうものである。
この関数について調べて、「整数リストを小さい順から並べる」「文字列リストを辞書式順に並べる」など、
いろいろなソートが実現できることを示しなさい。 *)

(* テスト *)
let test1 = min [] = max_int;;
let test2 = min [1; 5; 2; 3; 4] = 1;;

let test3 = dic_last [] = "";;
let test4 = dic_last ["aloha"; "mahalo"; "komo"] = "mahalo";;

let test6 = my_iter print_int [111; 222; 333] = ();;

let test7 =  my_fold_right (fun x y -> (float x) +. y) [1; 2; 3] 0.0 = 6.0;;

let test8 = deriv (fun x -> x *. x +. x +. 1.0) 3.5;;
let test9 = deriv (fun x -> x *. x +. x +. 1.0) 11.5;;
