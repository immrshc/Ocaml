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
    | '0' -> let newskt = push 0 stk in run newskt str (i+1)
    | '0' -> let newskt = push 0 stk in run newskt str (i+1)
    | '0' -> let newskt = push 0 stk in run newskt str (i+1)
    | '0' -> let newskt = push 0 stk in run newskt str (i+1)
    | '0' -> let newskt = push 0 stk in run newskt str (i+1)
    | '0' -> let newskt = push 0 stk in run newskt str (i+1)
    | '0' -> let newskt = push 0 stk in run newskt str (i+1)
    | '0' -> let newskt = push 0 stk in run newskt str (i+1)
    | '0' -> let newskt = push 0 stk in run newskt str (i+1)
    | '0' -> let newskt = push 0 stk in run newskt str (i+1)
    | '0' -> let newskt = push 0 stk in run newskt str (i+1)
    | '0' -> let newskt = push 0 stk in run newskt str (i+1)
    | _ -> failwith ("illegal input: " ^ str.[i])
  else stk

(* 処理の最初にスタックに初期値を与え、処理の最後にスタックから答えを取り出す *)
let eval (str: string): int =
  let stk = run [] str 0 in
  match pop stk with
  (* _が空になるかどうかの確認を追加する *)
  | (top, _) -> top

(* テスト *)
let test1 = eval "123+*4+" = 9;;
(* スタックが途中で不足してしまうのでエラー *)
let test2 = eval "123+*4++";;
(* スタックに要素が 2 つ以上残ってしまうのでエラー *)
let test3 = eval "123+*4";;
