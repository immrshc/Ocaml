(* 必須課題の三つは必ず提出する *)

(* 1 逆ポーランド記法 *)
type stack = int list

let push (n: int) (s: stack): stack =
  n::s

let pop (s: stack): int * stack =
  match s with
  | [] -> failwith "Empty stack error in pop"
  | h::s2 -> (h, s2)

let is_empty (s: stack): bool = (s = [])

(* 引数：現在のスタック、入力の文字列、処理している文字列の番号
返り値：その文字の処理が終わった後のスタック *)
let rec run (stk: stack) (str: string) (i: int): stack =
  if i < String.length str then
    match str.[i] with
    (* 出来るだけ末尾再帰で記述する *)
    | '0' -> let newskt = push 0 stk in run newskt str (i+1)
    | '1' -> let newskt = push 1 stk in run newskt str (i+1)
    | '2' -> let newskt = push 2 stk in run newskt str (i+1)
    | '3' -> let newskt = push 3 stk in run newskt str (i+1)
    | '4' -> let newskt = push 4 stk in run newskt str (i+1)
    | '5' -> let newskt = push 5 stk in run newskt str (i+1)
    | '6' -> let newskt = push 6 stk in run newskt str (i+1)
    | '7' -> let newskt = push 7 stk in run newskt str (i+1)
    | '8' -> let newskt = push 8 stk in run newskt str (i+1)
    | '9' -> let newskt = push 9 stk in run newskt str (i+1)
    | '+' -> let (top1, newstack1) = pop stk in
             let (top2, newstack2) = pop newstack1 in
             let newskt = push (top1 + top2) newstack2 in
             run newskt str (i+1)
    | '-' -> let (top1, newstack1) = pop stk in
             let (top2, newstack2) = pop newstack1 in
             let newskt = push (top1 - top2) newstack2 in
             run newskt str (i+1)
    | '*' -> let (top1, newstack1) = pop stk in
             let (top2, newstack2) = pop newstack1 in
             let newskt = push (top1 * top2) newstack2 in
             run newskt str (i+1)
    | '/' -> let (top1, newstack1) = pop stk in
             let (top2, newstack2) = pop newstack1 in
             let newskt = push (top1 / top2) newstack2 in
             run newskt str (i+1)
    | _ -> failwith ("illegal input:")
  else stk

(* 処理の最初にスタックに初期値を与え、処理の最後にスタックから答えを取り出す *)
let eval (str: string): int =
  let stk = run [] str 0 in
  match pop stk with
  (* _が空になるかどうかの確認を追加する *)
  | (top, newstk) -> if is_empty newstk then top
                     else failwith "Remaining number error in run"

(* テスト *)
let test1 = eval "123+*4+" = 9;;
(* スタックが途中で不足してしまうのでエラー
"Empty stack error in pop" *)
(* let test2 = eval "123+*4++";; *)
(* スタックに要素が 2つ以上残ってしまうのでエラー
"Remaining number error in run" *)
(* let test3 = eval "123+*4";; *)

(* 2. 論理式の処理 *)
