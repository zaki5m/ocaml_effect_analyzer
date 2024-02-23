open Pparse
open Pprintast
open Ast_helper
open Ast_mapper
open Parsetree
open Longident

open Effect_analyzer_core
open Efname_formatter

(* パターンマッチがエフェクトか例外かその他かを判定するための型 *)
type pattern_type = 
  | Effect
  | Exception
  | Other

(* 文字列からどの型にマッチするかを判定する関数 *)
let pattern_type_of_string str =
  match str with
  | "effc" -> Effect
  | "exnc" -> Exception
  | _ -> Other

(* パターンマッチのケースを解析する *)

(* 式内で 'perform' を探す *)

let extract_ident_from_construct (name:t with_loc) =
  match name.txt with
  | Lident s -> s
  | Ldot (lident, s) -> String.concat "." (Longident.flatten lident @ [s])
  | Lapply (l1, l2) -> 
      String.concat "" [Longident.last l1; "("; Longident.last l2; ")"]

(* applyする関数がhandlerに関係するものかどうかを判断 *)
let function_name_is_handler (function_name: expression) = match function_name with
  | { pexp_desc = Pexp_ident { txt = Lident "match_with"; _ }; _ } -> true
  | { pexp_desc = Pexp_ident { txt = Lident "continu_with"; _ }; _ } -> true
  | _ -> false

(* match_withの関数適用を変換する *)
(* match_with 関数　{ レコード　}の式を(関数，レコード)に変換するもの *)
let convert_match_with args =
  let rec loop (lst: (Asttypes.arg_label * expression) list) = match lst with
    | [] -> []
    | [(_, _)] -> []
    | (_, expr) :: rest -> 
        expr :: (loop rest)
  in
  let tmp_args = loop args in
  let len_args = List.length args in
  let handler_record = List.nth args (len_args - 1) |> snd in
  (tmp_args, handler_record)

(* handlerの中身を解析して，efNameOfHandler list型に変換する　*)


(* perform_lst: efName list list *)
(* perform_lstの中身をefName listに変換する *)


(* before_lst: 既存のperform_lst(前) *)
(* tmp_lst: appendする対象のperform_lst(新) *)
(* efName list listを返す *)
let perform_lst_append tmp_lst before_lst = match before_lst with
  | [] -> tmp_lst
  | _ -> List.map (fun lst -> List.map (fun lst2 -> lst2 @ lst) tmp_lst) before_lst |> List.flatten

(* 関数名のexpressionから関数名を抽出する *)
let extract_function_name expr =
  match expr.pexp_desc with
  | Pexp_ident { txt = Lident function_name; _ } -> function_name
  | _ -> "unknown"

(* case文の重複しているものを除きつつ，パターン名を削除するもの *)
let rec remove_duplicate_case lst tmp_lst = match lst with
  | [] -> []
  | (_, efName_lst) :: rest -> 
      let tmp_lst = 
        if List.exists (fun tmp_efName_lst -> tmp_efName_lst == efName_lst ) tmp_lst then
          tmp_lst
        else
          efName_lst :: tmp_lst
      in
      remove_duplicate_case rest tmp_lst

(* FunctionNameの第一要素が任意の文字列ならtrueそうでなければfalseを返す関数 *)
let is_any_function_name (function_name: string) efName = match efName with
  | FunctionName (name, _, _) -> name = function_name
  | _ -> false

(* efNameからefNameOfHandlerのlistを返す関数 *)
let extract_efNameOfHandler_from_efName (efName: efName) = match efName with
  | FunctionName (_, handler, _) -> handler
  | _ -> []

(* efNameのlistからefNameofHandlerを返す関数 *)
let efNameOfHandler_list_from_efName_list (efName_tree: efNameTree) pattern = 
  let handler = 
    let rec loop efName_tree = 
      match efName_tree with
      | Leaf -> None
      | Node (efName, efName_lst) -> 
        if is_any_function_name " " efName then
          Some (extract_efNameOfHandler_from_efName efName)
        else
          if efName_lst = [] then
            None
          else
            efName_tree_lst_loop efName_lst
    and efName_tree_lst_loop lst = match lst with
      | [] -> None
      | efName_tree :: rest -> 
          match loop efName_tree with
          | Some efName -> Some efName 
          | None -> efName_tree_lst_loop rest
    in
    match loop efName_tree with 
    | Some efName -> efName
    | None -> 
      match pattern with
      | Effect -> [Effc [("_", efName_tree)]]
      | Exception -> [Exnc [("_", efName_tree)]]
      | Other -> [Retc efName_tree]
  in
  handler


(* treeのLeafまで走査して，新たなNodeを追加する関数 *)
let rec add_efName_tree (tree: efNameTree) (append_tree: efNameTree) = match tree with
  | Leaf -> append_tree
  | Node (efName, []) -> Node (efName, [append_tree])
  | Node (efName, lst) -> Node (efName, List.map (fun new_tree -> add_efName_tree new_tree append_tree) lst)

(* treeのLeafまで走査して，新たなNodeを追加する関数 (appendするのがlist version) *)
let rec add_efName_tree_list (tree: efNameTree) (append_tree: efNameTree list) = match tree with
  | Leaf -> Node (FunctionName ("unit", [], []), append_tree)
  | Node (efName, []) -> Node (efName, append_tree)
  | Node (efName, lst) -> Node (efName, List.map (fun new_tree -> add_efName_tree_list new_tree append_tree) lst)

(* treeのLeafにのみ新たなNodeを追加する関数 *)
let rec add_efName_tree_to_leaf (tree: efNameTree) (append_tree: efNameTree) = match tree with
  | Leaf -> append_tree
  | Node (efName, lst) -> Node (efName, List.map (fun new_tree -> add_efName_tree_to_leaf new_tree append_tree) lst)

(* treeのLeafにのみ新たなNodeを追加する関数 (appendするのがlist version) *)
let rec add_efName_tree_list_to_leaf (tree: efNameTree) (append_tree: efNameTree list) = match tree with
  | Leaf -> Node (FunctionName ("unit", [], []), append_tree)
  | Node (efName, lst) -> Node (efName, List.map (fun new_tree -> add_efName_tree_list_to_leaf new_tree append_tree) lst)

(* efNameTreeのリストを結合する関数 *)
let append_efNameTree (tree_lst: efNameTree list) (append_tree: efNameTree) = match tree_lst with
  | [] -> [append_tree]
  | _ -> List.map (fun tree -> add_efName_tree tree append_tree) tree_lst

(* efNameTreeのリストを結合する関数 (list version) *)
let append_efNameTree_list (tree_lst: efNameTree list) (append_tree: efNameTree list) = match tree_lst with
  | [] -> append_tree
  | _ -> List.map (fun tree -> add_efName_tree_list tree append_tree) tree_lst

(* value binding listから関数名と引数の個数を取得する *)
let extract_function_name_and_arg_num_from_vb_list vb_lst = match vb_lst with
  | [] -> ("unknown", 0)
  | head :: _ -> 
    let function_name = match head.pvb_pat.ppat_desc with 
    | Ppat_any -> "_"
    | Ppat_var { txt = function_name; _ } -> function_name
    | _ -> "unknown"
    in
    let args_count = match head.pvb_expr.pexp_desc with
    | Pexp_fun (_, _, _, _) ->
      let rec count_args expr acc = match expr.pexp_desc with
        | Pexp_fun (_, _, _, body) -> count_args body (acc + 1)
        | _ -> acc
      in
      count_args head.pvb_expr 0
    | _ -> 0
    in
    (function_name, args_count)

(* 関数名とlocal_var_lstからlocalVarを作成する関数 *)
let create_localVar (function_name: string) (tmp_local_var_lst: localVar list) (pre_local_var_lst: localVar list) (tree: efNameTree list) = 
  let length_diff = List.length tmp_local_var_lst - List.length pre_local_var_lst in
  let rec loop i lst args_lst = 
    if i = 0 then 
      args_lst
    else
      match lst with
      | [] -> args_lst
      | head :: rest -> ( match head with 
        | ArgsVar (_, _) -> 
          let args_lst = head :: args_lst in
          loop (i-1) rest args_lst
        | _ -> args_lst
      ) 
  in
  (* 引数のリストを取り出す *)
  let args_lst = loop length_diff tmp_local_var_lst [] in
  if args_lst = [] then
    None
  else
    let local_var = LocalVar (function_name, args_lst, (Node (Empty, tree))) in
    Some local_var


(* パターンマッチのケースを解析する *)