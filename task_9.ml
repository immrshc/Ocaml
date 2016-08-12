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

(* 原子命題のリストatomsをもらって、そのリストの要素の文字列を列とする *)
let make_column (atoms : string list) =
  let rec make_column_aux (atoms : string list) (str: string): string =
    match atoms with
    | [] -> str ^ ": target\n"
    | h::t -> make_column_aux t (str ^ h ^ " ")
  in make_column_aux atoms ""

(* タプルのキーの文字列に対応した値の真偽値の組み合わせを返す *)
let make_index (asn: (string * bool) list) (atoms: string list): string =
  let rec make_index_aux (asn: (string * bool) list) (atoms: string list) (str: string): string =
    match atoms with
    | [] -> str ^ ": "
    | h::t -> make_index_aux asn t (str ^ string_of_bool(List.assoc h asn) ^ " ")
  in make_index_aux asn atoms ""

(* 2.3 上記の関数eval_2を利用して、与えられた論理式の真理値表を作成する *)
let formula_table (fml: formula) =
  let atoms: string list = get_atom fml in
  let formula_row (asn: (string * bool) list) =
    (* 割り当てのキーに対応した値の真偽値の組み合わせの文字列を返す：make_index *)
    (* その真偽値の組み合わせで解いた論理式の真偽値の文字列を返す：eval_2 を使う *)
    print_string((make_index asn atoms) ^ string_of_bool(eval_2 fml asn) ^ "\n") in
begin
  print_string(make_column atoms);
  List.iter formula_row (make_aenv atoms)
end

(* テスト *)
let test2_3_1 = make_aenv ["p"; "q"];;
let test2_3_2 = make_index [("p",true);("q",false)] ["p"; "q"];;
let test2_3_3 = make_index [("p",true);("q",false)] ["q"; "p"];;
let test2_3_4 = make_aenv (get_atom (And(Not(Atom("p")), Or(Atom("q"), Atom("p")))));;
let test2_3_5 = formula_table (And(Not(Atom("p")), Or(Atom("q"), Atom("p"))));;


(* 演習課題2-4 (発展課題) 論理式And(Not(Atom("p")),Or(Atom("q"),Atom("p"))) を(~ p) & (q \/ p) などのように出力する *)
let print_formula (fml: formula) =
  let rec string_of_formula (fml: formula): string =
    match fml with
    | Atom(s) -> s
    | Not(f1) -> "(~ " ^ (string_of_formula f1) ^ ")"
    | And(f1, f2) -> "(" ^ (string_of_formula f1) ^ " & " ^ (string_of_formula f2) ^ ")"
    | Or(f1, f2) -> "(" ^ (string_of_formula f1) ^ " \\/ " ^ (string_of_formula f2) ^ ")"
  in print_string(string_of_formula fml)

(* テスト *)
let test2_4 = print_formula (And(Not(Atom("p")),Or(Atom("q"),Atom("p"))))


(* 否定の記号を、一番内側に移動する。このためには、Not(Not(f))->f, Not(And(f1,f2))->Or(Not(f1),Not(f2))、
Not(Or(f1,f2))->And(Not(f1),Not(f2)) といった変形を繰返し適用し、これ以上変形できなくなったら終了すれば
よい。 *)
(* And をOr の外に出す。このためには、Or(And(f1,f2),f3)-> And(Or(f1,f3),Or(f2,f3))、
Or(f3,And(f1,f2))-> And(Or(f3,f1),Or(f3,f2)) といった変形を繰返し適用し、これ以上変形できなくなったら終了
すればよい。 *)

(* 演習課題3-1 (選択必修課題; これと2-3 のどちらかが必修) 上記の2つの変換のうちNot を内側にいれる変換を実装せよ。関数
名はto_nnf とする。(NNF というのはNegation-Normal Form、つまり否定に関する標準形という意味である)。 *)
let to_nnf (fml: formula): formula =
  let rec to_nnf_aux (pre: string) (fml: formula): formula =
    if pre = "Not" then
      match fml with
      | Atom(s) -> Not(Atom(s))
      | Not(f1) -> to_nnf_aux "" f1
      | And(f1, f2) -> Or((to_nnf_aux "Not" f1), (to_nnf_aux "Not" f2))
      | Or(f1, f2) -> And((to_nnf_aux "Not" f1), (to_nnf_aux "Not" f2))
    else match fml with
      | Atom(s) -> Atom(s)
      | Not(f1) -> to_nnf_aux "Not" f1
      | And(f1, f2) -> And((to_nnf_aux "" f1), (to_nnf_aux "" f2))
      | Or(f1, f2) -> Or((to_nnf_aux "" f1), (to_nnf_aux "" f2))
  in to_nnf_aux "" fml

(* テスト *)
let test3_1_1 = to_nnf (Not(Not(Atom "p")));;
let test3_1_2 = to_nnf (Not(And(Atom "p", Atom "q")));;
let test3_1_3 = to_nnf (Not(And(Not(Atom "p"), Atom "q")));;
let test3_1_4 = to_nnf (Not(And(Not(Atom "p"), Or(Atom "q", Atom "p")))) = Or(Atom "p", And(Not (Atom "q"), Not(Atom "p")));;

(* 演習課題3-2 (発展) 上記の2 つの変換のうち後半(否定に関する処理がおわった論理式に対して、それをCNF に変換) を実装せよ。関数名は to_cnf とする。 *)
let to_cnf (fml: formula): formula =
  let rec to_cnf_aux (pre: string) (fml: formula): formula =
    if pre = "Or" then match fml with
    | Or(And(f1, f2), f3) -> And((to_cnf_aux "" (Or(f1, f3))), (to_cnf_aux "" (Or(f2, f3))))
    | Or(f3, And(f1, f2)) -> And((to_cnf_aux "" (Or(f1, f3))), (to_cnf_aux "" (Or(f2, f3))))
    | Or(Or(f1, f2), f3) -> Or((to_cnf_aux "" (Or(f1, f2))), (to_cnf_aux "" f3))
    | Or(_, _) -> fml
    else match fml with
    | Atom(s) -> fml
    | Not(f1) -> fml (* to_nnfが既に実行されているから *)
    | Or(f1, f2) -> to_cnf_aux "Or" fml
    | And(f1, f2) -> And((to_cnf_aux "" f1), (to_cnf_aux "" f2))
  in to_cnf_aux "" fml

(* テスト *)
let test3_2_1 = to_cnf (Atom "p");;
let test3_2_2 = to_cnf (Or(Atom "p", Atom "q"));;
let test3_2_3 = to_cnf (Or(And((Atom "p"), (Atom "q")), Atom "r"));;
let test3_2_4 = to_cnf (Or(Or(Atom "p", Atom "q"), Atom "r"));;
