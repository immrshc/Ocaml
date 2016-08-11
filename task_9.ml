(* 必須課題の三つは必ず提出する *)

(* 1.1 逆ポーランド記法 *)
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
let eval_1 (str: string): int =
  let stk = run [] str 0 in
  match pop stk with
  (* _が空になるかどうかの確認を追加する *)
  | (top, newstk) -> if is_empty newstk then top
                     else failwith "Remaining number error in run"

(* テスト *)
let test1_1 = eval_1 "123+*4+" = 9;;
(* スタックが途中で不足してしまうのでエラー
"Empty stack error in pop" *)
(* let test1_2 = eval_1 "123+*4++";; *)
(* スタックに要素が 2つ以上残ってしまうのでエラー
"Remaining number error in run" *)
(* let test1_3 = eval_1 "123+*4";; *)

(* 2.1 論理式の処理 *)
type formula =
 | Atom of string
 | Not of formula
 | And of formula * formula
 | Or of formula * formula

(* 2.1 論理式に含まれる原子命題の名前の種類のリストを返す *)
let get_atom (fml: formula): string list =
  let rec set_atom (fml: formula) (lst: string list): string list =
    match fml with
    | Atom(s) -> if List.mem s lst then lst else s::lst
    | Not(f1) -> set_atom f1 lst
    | And(f1, f2) -> set_atom f1 (set_atom f2 lst)
    | Or(f1, f2) -> set_atom f1 (set_atom f2 lst)
  in set_atom fml []

(* テスト *)
let test2_1 = get_atom (And(Not(Atom("p")), Or(Atom("q"),Atom("p")))) = ["q"; "p"];;


(* 2.2 論理式の真偽値 *)
let rec eval_2 (fml: formula) (asn: (string * bool) list): bool =
  match fml with
  | Atom(s) -> List.assoc s asn
  | Not(f1) -> not (eval_2 f1 asn)
  | And(f1, f2) -> (eval_2 f1 asn) && (eval_2 f2 asn)
  | Or(f1, f2) -> (eval_2 f1 asn) || (eval_2 f2 asn)

(* テスト *)
let test2_2_1 = eval_2 (And(Not(Atom("p")),Or(Atom("q"),Atom("p")))) [("p",true);("q",false)] = false;;
let test2_2_2 = eval_2 (And(Not(Atom("p")),Or(Atom("q"),Atom("p")))) [("p",false);("q",true)] = true;;
(* let test2_2_3 = eval_2 (And(Not(Atom("p")),Or(Atom("q"),Atom("p")))) [("p", true)];;(* エラー *) *)
(* let test2_2_4 = eval_2 (And(Not(Atom("p")),Or(Atom("q"),Atom("p")))) [("p", false)];;(* エラー *) *)
let test2_2_5 = eval_2 (And(Not(Atom("p")),Or(Atom("q"),Atom("p")))) [("p", false); ("q", true); ("r", true)] = true;;


(* 原子命題のリストatomsをもらって、その原子命題リストに対する「すべての割当てを並べたリスト」を返す *)
let make_aenv (atoms : string list) =
 let rec walk atoms aenv =
  match atoms with
  | [] -> [aenv]
  | h::t -> (walk t ((h, true)::aenv)) @ (walk t ((h, false)::aenv))
 in walk atoms []

(* 原子命題のリストatomsをもらって、そのリストの要素の文字列を結合させる *)
let make_index (atoms : string list) =
  let rec make_index_aux (atoms : string list) (str: string): string =
    match atoms with
    | [] -> str ^ ":"
    | h::t -> make_index_aux t (str ^ h ^ " ")
  in make_index_aux atoms ""

(* 2.3 上記の関数eval_2を利用して、与えられた論理式の真理値表を作成する *)
let formula_table (fml: formula) =
  let str_list: string list = get_atom fml in
  let formula_row (s: (string * bool) list) = print_string((make_index str_list) ^ string_of_bool(eval_2 fml s) ^ "\n") in
begin
  print_string "p q target test\n";
  List.iter formula_row (make_aenv str_list)
end

(* テスト *)
let _ = make_aenv ["p"; "q"];;
let _ = make_index ["p"; "q"];;
let _ = make_aenv (get_atom (And(Not(Atom("p")), Or(Atom("q"), Atom("p")))));;
(* let _ = List.iter (fun (s: ((string * bool) list)) -> (print_string(string_of_bool(List.assoc "p" s) ^ "\n") ))
  (make_aenv (get_atom (And(Not(Atom("p")), Or(Atom("q"), Atom("p"))))));; *)
let _ = formula_table (And(Not(Atom("p")), Or(Atom("q"), Atom("p"))));;
