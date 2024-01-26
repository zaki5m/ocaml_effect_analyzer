open Pparse
open Pprintast
open Ast_helper
open Ast_mapper
open Parsetree
open Longident

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
let rec find_perform_in_expr expr =
  Printf.printf "len: %d\n" (List.length expr.pexp_attributes);
  print_endline "----------------";
  (* Printf.printf "find_perform_in_expr: %s\n" (Pprintast.string_of_expression expr); *)
  match expr.pexp_desc with
  | Pexp_extension ({ txt = "perform"; _ }, _) ->
    (* 'perform' 拡張を使う式を見つけた場合 *)
    Printf.printf "perform found: %s\n" (Pprintast.string_of_expression expr);
    expr
  | Pexp_ident { txt = Lident "perform"; _ } ->
    (* 'perform' を使う式を見つけた場合 *)
    Printf.printf "perform found: %s\n" (Pprintast.string_of_expression expr);
    expr
  | Pexp_let (_, vb_list, expr1) ->
    (* 'let' 式の場合は本体をトラバースする *)
    Printf.printf "let found: %s\n" (Pprintast.string_of_expression expr);
    List.iter (fun vb -> ignore (find_perform_in_value_binding vb)) vb_list;
    ignore (find_perform_in_expr expr1);
    expr1
  | Pexp_letmodule (_, _, expr1) ->
    (* 'let module' 式の場合は本体をトラバースする *)
    Printf.printf "letmodule found: %s\n" (Pprintast.string_of_expression expr);
    ignore (find_perform_in_expr expr1);
    expr1
  | Pexp_fun (_, _, _, expr1) ->
    (* 'fun' 式の場合は本体をトラバースする *)
    Printf.printf "fun found: %s\n" (Pprintast.string_of_expression expr);
    ignore (find_perform_in_expr expr1);
    expr1
  | Pexp_match (expr1, cases) ->
    (* 'match' 式の場合は対象式とケースをトラバースする *)
    Printf.printf "match found: %s\n" (Pprintast.string_of_expression expr);
    ignore (find_perform_in_expr expr1);
    List.iter (fun case -> ignore (find_perform_in_case case)) cases;
    expr1
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "perform"; _ }; _ }, _args) ->
    (* 'perform' を使う式を見つけた場合 *)
    Printf.printf "perform found: %s\n" (Pprintast.string_of_expression expr);
    expr
  | Pexp_array exprs ->
    (* 配列の場合は要素をトラバースする *)
    Printf.printf "array found: %s\n" (Pprintast.string_of_expression expr);
    List.iter (fun expr -> ignore (find_perform_in_expr expr)) exprs;
    expr
  | Pexp_tuple exprs ->
    (* タプルの場合は要素をトラバースする *)
    Printf.printf "tuple found: %s\n" (Pprintast.string_of_expression expr);
    List.iter (fun expr -> ignore (find_perform_in_expr expr)) exprs;
    expr
  | Pexp_construct (_, Some expr1) ->
    (* コンストラクタの場合は引数をトラバースする *)
    Printf.printf "construct found: %s\n" (Pprintast.string_of_expression expr);
    ignore (find_perform_in_expr expr1);
    expr1
  | Pexp_variant (_, Some expr1) ->
    (* バリアントの場合は引数をトラバースする *)
    Printf.printf "variant found: %s\n" (Pprintast.string_of_expression expr);
    ignore (find_perform_in_expr expr1);
    expr1
  | Pexp_record (fields, _) ->
    (* レコードの場合はフィールドをトラバースする *)
    Printf.printf "record found: %s\n" (Pprintast.string_of_expression expr);
    List.iter (fun (_, expr) -> ignore (find_perform_in_expr expr)) fields;
    expr
  | Pexp_field (expr1, _) ->
    (* フィールド参照の場合はレコードをトラバースする *)
    Printf.printf "field found: %s\n" (Pprintast.string_of_expression expr);
    ignore (find_perform_in_expr expr1);
    expr1
  | Pexp_setfield (expr1, _, expr2) ->
    (* フィールド代入の場合はレコードと値をトラバースする *)
    Printf.printf "setfield found: %s\n" (Pprintast.string_of_expression expr);
    ignore (find_perform_in_expr expr1);
    ignore (find_perform_in_expr expr2);
    expr
  | Pexp_ifthenelse (expr1, expr2, Some expr3) ->
    (* if式の場合は条件式とthen節とelse節をトラバースする *)
    Printf.printf "ifthenelse found: %s\n" (Pprintast.string_of_expression expr);
    ignore (find_perform_in_expr expr1);
    ignore (find_perform_in_expr expr2);
    ignore (find_perform_in_expr expr3);
    expr
  | Pexp_sequence (expr1, expr2) ->
    (* シーケンスの場合は式1と式2をトラバースする *)
    Printf.printf "sequence found: %s\n" (Pprintast.string_of_expression expr);
    ignore (find_perform_in_expr expr1);
    ignore (find_perform_in_expr expr2);
    expr
  | Pexp_while (expr1, expr2) ->
    (* while式の場合は条件式と本体をトラバースする *)
    Printf.printf "while found: %s\n" (Pprintast.string_of_expression expr);
    ignore (find_perform_in_expr expr1);
    ignore (find_perform_in_expr expr2);
    expr
  | Pexp_for (pat, expr1, expr2, _, expr3) ->
    (* for式の場合はパターンと初期値と終了値と本体をトラバースする *)
    Printf.printf "for found: %s\n" (Pprintast.string_of_expression expr);
    ignore (find_perform_in_expr expr1);
    ignore (find_perform_in_expr expr2);
    ignore (find_perform_in_expr expr3);
    expr
  | Pexp_constraint (expr1, _) ->
    (* 型制約の場合は式をトラバースする *)
    Printf.printf "constraint found: %s\n" (Pprintast.string_of_expression expr);
    ignore (find_perform_in_expr expr1);
    expr1
  | Pexp_coerce (expr1, _, _) ->
    (* 型変換の場合は式をトラバースする *)
    Printf.printf "coerce found: %s\n" (Pprintast.string_of_expression expr);
    ignore (find_perform_in_expr expr1);
    expr1
  | Pexp_send (expr1, _) ->
    (* メッセージ送信の場合は式をトラバースする *)
    Printf.printf "send found: %s\n" (Pprintast.string_of_expression expr);
    ignore (find_perform_in_expr expr1);
    expr1
  | Pexp_new _ ->
    (* クラス生成の場合は何もしない *)
    Printf.printf "new found: %s\n" (Pprintast.string_of_expression expr);
    expr
  | _ ->
    (* 他の式の場合は子ノードをトラバースする *)
    Printf.printf "other found: %s\n" (Pprintast.string_of_expression expr);
    Ast_mapper.default_mapper.expr Ast_mapper.default_mapper expr
and find_perform_in_case case =
  (* パターンは今は未対応 *)
  ignore (find_perform_in_expr case.pc_rhs);
  match case.pc_guard with
  | Some expr -> ignore (find_perform_in_expr expr)
  | None -> ()
and find_perform_in_value_binding vb =
  (* パターンは今は未対応 *)
  ignore (find_perform_in_expr vb.pvb_expr)

(* 構文要素をトラバースしてエフェクトを使用する部分を探す *)



let print_structure_item item =
  let format = Format.str_formatter in
  structure_item format item;
  Format.flush_str_formatter () |> print_endline

let string_of_patern_name pat =
  match pat.ppat_desc with
  | Ppat_var { txt; _ } -> txt
  | _ -> ""

let string_of_value_binding vb =
  let format = Format.str_formatter in
  (* vb.pvb_loc |> Location.print_loc format; *)
  Pprintast.binding format vb;
  Format.flush_str_formatter ()

let print_perform_expressions structure =
  List.iter (fun item ->
    print_structure_item item;
    print_endline "================";
    match item.pstr_desc with
    | Pstr_eval (expr, _) ->
      (match find_perform_expr expr with
       | Some perform_expr -> Printf.printf "Found perform: %s\n" (Pprintast.string_of_expression perform_expr)
       | None -> ())
    | Pstr_value (_, value_bindings) ->
      (* 'let' 式をトラバース *)
      List.iter (fun vb -> Printf.printf "vb: %s\n" (string_of_patern_name vb.pvb_pat); ignore (find_perform_in_expr vb.pvb_expr) ) value_bindings
    | _ -> ()
  ) structure


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
