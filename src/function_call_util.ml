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
  | FunctionName (name, _) -> name = function_name
  | _ -> false

(* efNameからefNameOfHandlerのlistを返す関数 *)
let extract_efNameOfHandler_from_efName (efName: efName) = match efName with
  | FunctionName (_, handler) -> handler
  | _ -> []

(* efNameのlistからefNameofHandlerを返す関数 *)
let efNameOfHandler_list_from_efName_list (efName_lst: efName list) pattern = 
  let handler = 
    try 
      List.find (fun efName -> is_any_function_name " " efName) efName_lst |> extract_efNameOfHandler_from_efName
    with
      Not_found -> 
        match pattern with
        | Effect -> [Effc [("_", [efName_lst])]]
        | Exception -> [Exnc [("_", [efName_lst])]]
        | Other -> [Retc [efName_lst]]
  in
  handler


(* efNameOfHandlerのlistからefNameのlistを返す関数 *)