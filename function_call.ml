open Pparse
open Pprintast
open Ast_helper
open Ast_mapper
open Parsetree
open Longident

let extract_ident_from_construct (name:t with_loc) =
  match name.txt with
  | Lident s -> s
  | Ldot (lident, s) -> String.concat "." (Longident.flatten lident @ [s])
  | Lapply (l1, l2) -> 
      String.concat "" [Longident.last l1; "("; Longident.last l2; ")"]

(* 式をトラバースしてエフェクトを使用する部分を探す *)
let rec find_perform_expr expr =
  Printf.printf "find_perform_expr: %s\n" (Pprintast.string_of_expression expr);
  match expr.pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "perform"; _ }; _ }, args) ->
    (* エフェクトを使用する式を見つけた場合の処理 *)
    Some expr
  | _ ->
    (* 他の式の場合は子ノードをトラバースする *)
    Some (Ast_mapper.default_mapper.expr Ast_mapper.default_mapper expr)

(* 式内で 'perform' を探す *)
let rec find_perform_in_expr ?(is_perform_name = false) expr perform_lst =
  print_endline "----------------";
  (* Printf.printf "find_perform_in_expr: %s\n" (Pprintast.string_of_expression expr); *)
  match expr.pexp_desc with
  | Pexp_extension ({ txt = "perform"; _ }, _) ->
    (* 'perform' 拡張を使う式を見つけた場合 *)
    Printf.printf "perform foundA: %s\n" (Pprintast.string_of_expression expr);
    perform_lst
  | Pexp_ident { txt = Lident "perform"; _ } ->
    (* 'perform' を使う式を見つけた場合 *)
    Printf.printf "perform foundB: %s\n" (Pprintast.string_of_expression expr);
    perform_lst
  | Pexp_let (_, vb_list, expr1) ->
    (* 'let' 式の場合は本体をトラバースする *)
    Printf.printf "let found: %s\n" (Pprintast.string_of_expression expr);
    let tmp_perform_lst = List.fold_left (fun a vb -> (find_perform_in_value_binding vb []) @ a) perform_lst vb_list in
    let new_perform_lst = find_perform_in_expr expr1 tmp_perform_lst in
    new_perform_lst
  | Pexp_letmodule (_, _, expr1) ->
    (* 'let module' 式の場合は本体をトラバースする *)
    Printf.printf "letmodule found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr expr1 perform_lst in
    new_perform_lst
  | Pexp_fun (_, _, _, expr1) ->
    (* 'fun' 式の場合は本体をトラバースする *)
    Printf.printf "fun found: %s\n" (Pprintast.string_of_expression expr);
    let new_perfor_lst = find_perform_in_expr expr1 perform_lst in
    new_perfor_lst
  | Pexp_match (expr1, cases) ->
    (* 'match' 式の場合は対象式とケースをトラバースする *)
    Printf.printf "match found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr expr1 perform_lst in
    let perform_lst_lst = List.map (fun case -> find_perform_in_case case []) cases in (*ToDo caseeは未完成*)
    new_perform_lst
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "perform"; _ }; _ }, _args) ->
    (* 'perform' を使う式を見つけた場合 未完成*)
    Printf.printf "perform foundC: %s\n" (Pprintast.string_of_expression expr);
    Printf.printf "args: %s\n" (Pprintast.string_of_expression (List.nth _args 0 |> snd));
    let new_perform_lst = List.map (fun (_, expr) -> find_perform_in_expr expr [] ~is_perform_name:true) _args |> List.flatten in
    (* Printf.printf "perform_name: %s\n" perform_name; *)
    new_perform_lst
  | Pexp_array exprs ->
    (* 配列の場合は要素をトラバースする *)
    Printf.printf "array found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = List.fold_left (fun lst expr -> (find_perform_in_expr expr []) @ lst ) perform_lst exprs in
    new_perform_lst
  | Pexp_tuple exprs ->
    (* タプルの場合は要素をトラバースする *)
    Printf.printf "tuple found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = List.fold_left (fun lst expr -> (find_perform_in_expr expr []) @ lst ) perform_lst exprs in
    new_perform_lst
  | Pexp_construct (name, Some expr1) ->
    (* コンストラクタの場合は引数をトラバースする *)
    Printf.printf "construct found: %s\n" (Pprintast.string_of_expression expr);
    Printf.printf "is_perform_name: %b\n" is_perform_name;
    Printf.printf "name: %s\n" (extract_ident_from_construct name );
    let new_perform_lst = find_perform_in_expr expr1 perform_lst in
    if is_perform_name then 
      let new_perform_lst =  List.map (fun lst -> (extract_ident_from_construct name )::lst)new_perform_lst in
      new_perform_lst
    else
      new_perform_lst
  | Pexp_variant (_, Some expr1) ->
    (* バリアントの場合は引数をトラバースする *)
    Printf.printf "variant found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr expr1 perform_lst in
    new_perform_lst
  | Pexp_record (fields, _) ->
    (* レコードの場合はフィールドをトラバースする *)
    Printf.printf "record found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = List.fold_left (fun lst (_, expr) -> (find_perform_in_expr expr []) @ lst ) perform_lst fields in
    new_perform_lst
  | Pexp_field (expr1, _) ->
    (* フィールド参照の場合はレコードをトラバースする *)
    Printf.printf "field found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst =  find_perform_in_expr expr1 perform_lst in
    new_perform_lst
  | Pexp_setfield (expr1, _, expr2) ->
    (* フィールド代入の場合はレコードと値をトラバースする *)
    Printf.printf "setfield found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr expr1 perform_lst in
    let new_perform_lst =  find_perform_in_expr expr2 new_perform_lst in
    new_perform_lst
  | Pexp_ifthenelse (expr1, expr2, Some expr3) ->
    (* if式の場合は条件式とthen節とelse節をトラバースする *)
    Printf.printf "ifthenelse found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr expr1 perform_lst in
    let new_perform_lst =  find_perform_in_expr expr2 new_perform_lst in
    let new_perform_lst =  find_perform_in_expr expr3 new_perform_lst in
    new_perform_lst
  | Pexp_sequence (expr1, expr2) ->
    (* シーケンスの場合は式1と式2をトラバースする *)
    Printf.printf "sequence found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr expr1 perform_lst in
    let new_perform_lst =  find_perform_in_expr expr2 new_perform_lst in
    new_perform_lst
  | Pexp_while (expr1, expr2) ->
    (* while式の場合は条件式と本体をトラバースする *)
    Printf.printf "while found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr expr1 perform_lst in
    let new_perform_lst =  find_perform_in_expr expr2 new_perform_lst in
    new_perform_lst
  | Pexp_for (pat, expr1, expr2, _, expr3) ->
    (* for式の場合はパターンと初期値と終了値と本体をトラバースする *)
    Printf.printf "for found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr expr1 perform_lst in
    let new_perform_lst =  find_perform_in_expr expr2 new_perform_lst in
    let new_perform_lst =  find_perform_in_expr expr3 new_perform_lst in
    new_perform_lst
  | Pexp_constraint (expr1, _) ->
    (* 型制約の場合は式をトラバースする *)
    Printf.printf "constraint found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst =  find_perform_in_expr expr1 perform_lst in
    new_perform_lst
  | Pexp_coerce (expr1, _, _) ->
    (* 型変換の場合は式をトラバースする *)
    Printf.printf "coerce found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst =  find_perform_in_expr expr1 perform_lst in
    new_perform_lst
  | Pexp_send (expr1, _) ->
    (* メッセージ送信の場合は式をトラバースする *)
    Printf.printf "send found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst =  find_perform_in_expr expr1 perform_lst in
    new_perform_lst
  | _ -> perform_lst
and find_perform_in_case case perform_lst =
  (* パターンは今は未対応 *)
  let new_perform_lst = find_perform_in_expr case.pc_rhs perform_lst in
  match case.pc_guard with
  | Some expr -> find_perform_in_expr expr new_perform_lst
  | None -> new_perform_lst
and find_perform_in_value_binding vb perform_lst =
  (* パターンは今は未対応 *)
  let new_perfor_lst = find_perform_in_expr vb.pvb_expr perform_lst in
  new_perfor_lst

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
    let perform_lst = List.fold_left (fun lst vb -> (find_perform_in_expr vb.pvb_expr []) @ lst) [] value_bindings in 
    Some (function_name, perform_lst)
  | _ -> None

let print_perform_expressions structure =
  Printf.printf "print_perform_expressions\n";
  let result = List.map (fun item -> find_perform_expr_in_structure_item item) structure in
  List.iter (fun item -> match item with
      | Some (function_name, perform_lst) ->
        Printf.printf "function_name: %s\n" function_name;
        (* List.iter (fun perform_name -> Printf.printf "perform_name: %s\n" perform_name) perform_lst *)
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

let _ = parse_ocaml_file Sys.argv.(1)
