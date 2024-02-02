open Pparse
open Pprintast
open Ast_helper
open Ast_mapper
open Parsetree
open Longident

open Effect_analyzer_core
open Efname_formatter
open Function_call_util

(* 式内で 'perform' を探す *)
(* is_patern_searchはハンドラのパターンをトラバースするときに使う *)
let rec find_perform_in_expr ?(is_perform_name = false) is_patern_search expr (perform_lst: efName list list) local_var_lst =
  Printf.printf "--------len: %d--------\n"  (List.length perform_lst);
  (* Printf.printf "find_perform_in_expr is_patern_search: %s\n" (Pprintast.string_of_expression expr); *)
  match expr.pexp_desc with
  | Pexp_ident { txt = Lident "perform"; _ } ->
    (* 'perform' を使う式を見つけた場合 *)
    Printf.printf "perform foundB: %s\n" (Pprintast.string_of_expression expr);
    perform_lst
  | Pexp_let (_, vb_list, expr1) ->
    (* 'let' 式の場合は本体をトラバースする *)
    Printf.printf "let found: %s\n" (Pprintast.string_of_expression expr);
    (* vbを走査 *)
    let tmp_perform_lst = List.fold_left (fun lst vb -> find_perform_in_value_binding is_patern_search vb lst local_var_lst) [] vb_list in
    Printf.printf "--------let len: %d--------\n"  (List.length tmp_perform_lst);
    let new_perform_lst = perform_lst_append tmp_perform_lst perform_lst in
    Printf.printf "--------let len: %d--------\n"  (List.length new_perform_lst);
    let new_perform_lst = find_perform_in_expr is_patern_search expr1 new_perform_lst local_var_lst in
    Printf.printf "--------let len: %d--------\n"  (List.length new_perform_lst);
    new_perform_lst
  | Pexp_letmodule (_, _, expr1) ->
    (* 'let module' 式の場合は本体をトラバースする *)
    Printf.printf "letmodule found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr is_patern_search expr1 perform_lst local_var_lst in
    new_perform_lst
  | Pexp_fun (_, _, _, expr1) ->
    (* 'fun' 式の場合は本体をトラバースする *)
    Printf.printf "fun found: %s\n" (Pprintast.string_of_expression expr);
    let new_perfor_lst = find_perform_in_expr is_patern_search expr1 perform_lst local_var_lst in
    new_perfor_lst
  | Pexp_match (expr1, cases) ->
    (* 'match' 式の場合は対象式とケースをトラバースする *)
    Printf.printf "match found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr is_patern_search expr1 perform_lst local_var_lst in
    let tmp_perform_lst = find_perform_in_cases is_patern_search cases [] local_var_lst in (*ToDo caseeは未完成*)
    let new_perform_lst = perform_lst_append tmp_perform_lst new_perform_lst in
    new_perform_lst
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "perform"; _ }; _ }, _args) ->
    (* 'perform' を使う式を見つけた場合 未完成*)
    Printf.printf "perform foundC: %s\n" (Pprintast.string_of_expression expr);
    Printf.printf "args: %s\n" (Pprintast.string_of_expression (List.nth _args 0 |> snd));
    let new_perform_lst = List.map (fun (_, expr) -> find_perform_in_expr is_patern_search expr [] ~is_perform_name:true local_var_lst) _args |> List.flatten in
    (* Printf.printf "perform_name: %s\n" perform_name; *)
    new_perform_lst
  | Pexp_apply (expr1, args) ->
    (* 関数適用の場合は関数と引数をトラバースする *)
    Printf.printf "apply found: %s\n" (Pprintast.string_of_expression expr);
    Printf.printf "is handler?: %b\n" (function_name_is_handler expr1);
    Printf.printf "apply expr: %s\n" (Pprintast.string_of_expression expr1);
    let func_name = extract_function_name expr1 in
    (* この位置以降の call flowを取得 *)
    Printf.printf "args len: %d\n" (List.length args);
    Printf.printf "args0: %s\n" (Pprintast.string_of_expression (List.nth args 0 |> snd));
    if (function_name_is_handler expr1) then 
      let (tmp_args, handler_record) = convert_match_with args in (* ToDo: 高階関数の場合はこの辺りでバグる可能性あり *) (* 現在はhandlerの部分を未処理 *)
      let tmp_perform_lst = List.fold_left (fun lst expr -> (find_perform_in_expr is_patern_search expr [] local_var_lst) @ lst ) [] tmp_args in 
      let tmp_func_name = extract_function_name (List.hd tmp_args) in
      let handler = analyze_handler handler_record local_var_lst in
      let tmp_perform_lst = perform_lst_append [[(FunctionName (tmp_func_name, handler))]] tmp_perform_lst in
      let new_perform_lst = perform_lst_append tmp_perform_lst perform_lst in
      new_perform_lst
    else
      let tmp_perform_lst = List.fold_left (fun lst (_, expr) -> (find_perform_in_expr is_patern_search expr lst local_var_lst) ) [] args in
      let tmp_perform_lst = perform_lst_append [[(FunctionName (func_name, []))]] tmp_perform_lst in
      let new_perform_lst = perform_lst_append tmp_perform_lst perform_lst in
      Printf.printf "--------perform len: %d--------\n"  (List.length new_perform_lst);
      new_perform_lst
  | Pexp_array exprs ->
    (* 配列の場合は要素をトラバースする *)
    Printf.printf "array found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = List.fold_left (fun lst expr -> (find_perform_in_expr is_patern_search expr [] local_var_lst) @ lst ) perform_lst exprs in
    new_perform_lst
  | Pexp_tuple exprs ->
    (* タプルの場合は要素をトラバースする *)
    Printf.printf "tuple found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = List.fold_left (fun lst expr -> (find_perform_in_expr is_patern_search expr [] local_var_lst) @ lst ) perform_lst exprs in
    new_perform_lst
  | Pexp_construct (name, Some expr1) ->
    (* コンストラクタの場合は引数をトラバースする *)
    Printf.printf "construct found: %s\n" (Pprintast.string_of_expression expr);
    Printf.printf "is_perform_name: %b\n" is_perform_name;
    Printf.printf "name: %s\n" (extract_ident_from_construct name );
    let new_perform_lst = find_perform_in_expr is_patern_search expr1 perform_lst local_var_lst in
    if is_perform_name then 
      let new_perform_lst =  perform_lst_append [[(EffectName (extract_ident_from_construct name))]] new_perform_lst in
      new_perform_lst
    else
      new_perform_lst
  | Pexp_variant (_, Some expr1) ->
    (* バリアントの場合は引数をトラバースする *)
    Printf.printf "variant found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr is_patern_search expr1 perform_lst local_var_lst in
    new_perform_lst
  | Pexp_record (fields, _) ->
    (* レコードの場合はフィールドをトラバースする *)
    Printf.printf "record found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = List.fold_left (fun lst (_, expr) -> (find_perform_in_expr is_patern_search expr [] local_var_lst ) @ lst ) perform_lst fields in
    new_perform_lst
  | Pexp_field (expr1, _) ->
    (* フィールド参照の場合はレコードをトラバースする *)
    Printf.printf "field found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst =  find_perform_in_expr is_patern_search expr1 perform_lst local_var_lst in
    new_perform_lst
  | Pexp_setfield (expr1, _, expr2) ->
    (* フィールド代入の場合はレコードと値をトラバースする *)
    Printf.printf "setfield found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr is_patern_search expr1 perform_lst local_var_lst in
    let new_perform_lst =  find_perform_in_expr is_patern_search expr2 new_perform_lst local_var_lst in
    new_perform_lst
  | Pexp_ifthenelse (expr1, expr2, Some expr3) ->
    (* if式の場合は条件式とthen節とelse節をトラバースする *)
    Printf.printf "ifthenelse found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr is_patern_search expr1 perform_lst local_var_lst in
    let new_perform_lst =  find_perform_in_expr is_patern_search expr2 new_perform_lst local_var_lst in
    let new_perform_lst =  find_perform_in_expr is_patern_search expr3 new_perform_lst local_var_lst in
    new_perform_lst
  | Pexp_sequence (expr1, expr2) ->
    (* シーケンスの場合は式1と式2をトラバースする *)
    Printf.printf "sequence found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr is_patern_search expr1 perform_lst local_var_lst in
    let new_perform_lst =  find_perform_in_expr is_patern_search expr2 new_perform_lst local_var_lst in
    new_perform_lst
  | Pexp_while (expr1, expr2) ->
    (* while式の場合は条件式と本体をトラバースする *)
    Printf.printf "while found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr is_patern_search expr1 perform_lst local_var_lst in
    let new_perform_lst =  find_perform_in_expr is_patern_search expr2 new_perform_lst local_var_lst in
    new_perform_lst
  | Pexp_for (pat, expr1, expr2, _, expr3) ->
    (* for式の場合はパターンと初期値と終了値と本体をトラバースする *)
    Printf.printf "for found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr is_patern_search expr1 perform_lst local_var_lst in
    let new_perform_lst =  find_perform_in_expr is_patern_search expr2 new_perform_lst local_var_lst in
    let new_perform_lst =  find_perform_in_expr is_patern_search expr3 new_perform_lst local_var_lst in
    new_perform_lst
  | Pexp_constraint (expr1, _) ->
    (* 型制約の場合は式をトラバースする *)
    Printf.printf "constraint found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst =  find_perform_in_expr is_patern_search expr1 perform_lst local_var_lst in
    new_perform_lst
  | Pexp_coerce (expr1, _, _) ->
    (* 型変換の場合は式をトラバースする *)
    Printf.printf "coerce found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst =  find_perform_in_expr is_patern_search expr1 perform_lst local_var_lst in
    new_perform_lst
  | Pexp_send (expr1, _) ->
    (* メッセージ送信の場合は式をトラバースする *)
    Printf.printf "send found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst =  find_perform_in_expr is_patern_search expr1 perform_lst local_var_lst in
    new_perform_lst
  | Pexp_newtype (_, expr1) ->
    (* newtypeの場合は式をトラバースする *)
    Printf.printf "newtype found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst =  find_perform_in_expr is_patern_search expr1 perform_lst local_var_lst in
    new_perform_lst
  | _ -> 
    Printf.printf "other found: %s\n" (Pprintast.string_of_expression expr);
    perform_lst
and find_perform_in_cases is_patern_search cases perform_lst local_var_lst = match is_patern_search with
  (* ガードは今は未対応 *)
  (* handlerは即座にmatchする場合に限る *)
  | Effect -> 
    let tmp_perform_lst = Effc (List.fold_left (fun lst case -> ((find_perform_in_case is_patern_search case [] local_var_lst)) @ lst) [] cases) in
    (* let new_perform_lst = perform_lst_append FunctionName ("effect", [tmp_perform_lst])] perform_lst in
    new_perform_lst *)
    [[FunctionName (" ",[tmp_perform_lst])]]
  | Exception -> perform_lst 
  | Other -> 
    let tmp_perform_lst = List.fold_left (fun lst case -> ((find_perform_in_case is_patern_search case [] local_var_lst)) @ lst) [] cases in
    let tmp_perform_lst = remove_duplicate_case tmp_perform_lst [] in
    let new_perform_lst = perform_lst_append tmp_perform_lst perform_lst in
    new_perform_lst
and find_perform_in_case is_patern_search case perform_lst local_var_lst :(string * efName list list) list =
  (* ガードは未対応 *)
  let pattern_lst: string list = find_perform_in_pattern case.pc_lhs [] in
  let tmp_perform_lst = List.map (fun name -> (name, find_perform_in_expr Other case.pc_rhs [] local_var_lst)) pattern_lst in
  tmp_perform_lst
and find_perform_in_value_binding is_patern_search vb perform_lst local_var_lst =
  (* パターンは今は未対応 *)
  let new_perfor_lst = find_perform_in_expr is_patern_search vb.pvb_expr perform_lst local_var_lst in
  new_perfor_lst
and find_perform_in_pattern pat pattern_lst =
  match pat.ppat_desc with
  | Ppat_any -> "_"::pattern_lst
  | Ppat_var { txt = name; _ } -> name::pattern_lst
  | Ppat_alias (pat1, { txt = name; _ }) -> name::(find_perform_in_pattern pat1 pattern_lst)
  | Ppat_constant _ -> pattern_lst
  | Ppat_interval (_, _) -> pattern_lst
  | Ppat_tuple pats -> List.fold_left (fun lst pat -> (find_perform_in_pattern pat lst)) pattern_lst pats
  | Ppat_construct (name, _) -> (extract_ident_from_construct name)::pattern_lst (* ToDo: この部分は修正必須 *)
  | Ppat_variant (_, Some pat1) -> find_perform_in_pattern pat1 pattern_lst
  | Ppat_record (fields, _) -> List.fold_left (fun lst (_, pat) -> (find_perform_in_pattern pat lst)) pattern_lst fields
  | Ppat_array pats -> List.fold_left (fun lst pat -> (find_perform_in_pattern pat lst)) pattern_lst pats
  | Ppat_or (pat1, pat2) -> (find_perform_in_pattern pat1 pattern_lst) @ (find_perform_in_pattern pat2 pattern_lst)
  | Ppat_constraint (pat1, _) -> find_perform_in_pattern pat1 pattern_lst
  | Ppat_type _ -> pattern_lst
  | Ppat_lazy pat1 -> find_perform_in_pattern pat1 pattern_lst
  | Ppat_unpack _ -> pattern_lst
  | Ppat_exception pat1 -> find_perform_in_pattern pat1 pattern_lst
  | Ppat_extension _ -> pattern_lst
  | Ppat_open (_, pat1) -> find_perform_in_pattern pat1 pattern_lst
  | _ -> pattern_lst
(* handlerの中身を解析して，efNameOfHandler list型に変換する　*)
and analyze_handler handler_record local_var_lst = match handler_record.pexp_desc with
| Pexp_record (fields, _) ->
  (* レコードの場合はフィールドをトラバースする *)
  Printf.printf "record found: %s\n" (Pprintast.string_of_expression handler_record);
  (* 今回はfieldに全ての要素がある場合のみ対応 *)
  let (name1, field1) = List.nth fields 0 in
  let (name2, field2) = List.nth fields 1 in
  let (name3, field3) = List.nth fields 2 in 
  let name1 = extract_ident_from_construct name1 in
  (* name1がeffectで決めうちかつ，effectの発生のパターンマッチが必ず起こるパターンに限定 *)
  Printf.printf "name1: %s\n" name1;
  let lst1 = find_perform_in_expr (pattern_type_of_string name1) field1 [] local_var_lst in
  let effect_handler = efNameOfHandler_list_from_efName_list (List.hd lst1) Effect in
  (* List.iter (fun lst -> efName_list_to_string lst |> print_endline) lst1; *)
  let name2 = extract_ident_from_construct name2 in
  Printf.printf "name2: %s\n" name2;
  let lst2 = find_perform_in_expr (pattern_type_of_string name2) field2 [] local_var_lst in
  let exception_handler = efNameOfHandler_list_from_efName_list (List.hd lst2) Exception in
  List.iter (fun lst -> efName_list_to_string lst |> print_endline) lst2;
  let name3 = extract_ident_from_construct name3 in
  Printf.printf "name3: %s\n" name3;
  let lst3 = find_perform_in_expr (pattern_type_of_string name3) field3 [] local_var_lst in
  let other_handler = efNameOfHandler_list_from_efName_list (List.hd lst3) Other in
  List.iter (fun lst -> efName_list_to_string lst |> print_endline) lst3;
  let handler = effect_handler @ exception_handler @ other_handler in
  Printf.printf "handler len: %d\n" (List.length handler);
  handler
  | _ -> []

(* 構文要素をトラバースしてエフェクトを使用する部分を探す *)



let print_structure_item item =
  let format = Format.str_formatter in
  structure_item format item;
  Format.flush_str_formatter () |> print_endline

(* 関数名，発生するperformの組のlistにする *)
let rec find_perform_expr_in_structure_item item =
  print_structure_item item;
  Printf.printf "----------------------\n";
  match item.pstr_desc with
  | Pstr_value (_, value_bindings) ->
    Printf.printf "Pstr_value found\n";
    (* 'let' 式をトラバース *)
    let function_name = (match value_bindings with
        | { pvb_pat = { ppat_desc = Ppat_var { txt = function_name; _ }; _ }; _ } :: _ -> function_name
        | _ -> "unknown") in
    let perform_lst = List.fold_left (fun lst vb -> (find_perform_in_expr Other vb.pvb_expr [] []) @ lst) [] value_bindings in 
    Some (function_name, perform_lst)
  | _ -> None

let print_perform_expressions structure =
  let result = List.map (fun item -> find_perform_expr_in_structure_item item) structure in
  List.iter (fun item -> match item with
      | Some (function_name, perform_lst) ->
        Printf.printf "function_name: %s\n" function_name;
        (* Printf.printf "perform_lst len: %d\n" (List.length perform_lst); *)
        List.iter (fun lst -> efName_list_to_string lst |> print_endline) perform_lst;
      | None -> ()) result;
  ()


(* let custom_mapper _ =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = "perform"; _ }, _); _ } ->
        (* ここでeffect関連の情報を処理 *)
        Printf.printf "effect拡張が見つかりました: %s\n" (Pprintast.string_of_expression expr);
        expr
      | x -> default_mapper.expr mapper x;
  }

let _ = register "custom_mapper" custom_mapper *)


let print_ast ast =
  string_of_structure ast |> print_endline

let parse_ocaml_file filename =
  let ast = parse_implementation ~tool_name:"my_ocaml_parser" filename in
  print_perform_expressions ast

let parse_test_ocaml_file filename =
  let ast = parse_implementation ~tool_name:"my_ocaml_parser" filename in
  let result = List.map (fun item -> find_perform_expr_in_structure_item item) ast in
  let result = List.filter_map (fun item -> item) result in
  result
  

