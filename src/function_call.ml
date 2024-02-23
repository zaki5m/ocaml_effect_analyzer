open Pparse
open Pprintast
open Ast_helper
open Ast_mapper
open Parsetree
open Longident
open Compile_common

open Effect_analyzer_core
open Efname_formatter
open Function_call_util
open EfName_to_effect_row
open Typedtree_formatter

(* 式内で 'perform' を探す *)
(* is_patern_searchはハンドラのパターンをトラバースするときに使う *)
let rec find_perform_in_expr ?(is_perform_name = false) is_patern_search expr (local_var_lst: localVar list) :(efNameTree list * localVar list) =
  (* Printf.printf "find_perform_in_expr is_patern_search: %s\n" (Pprintast.string_of_expression expr); *)
  match expr.pexp_desc with
  | Pexp_ident { txt = Lident "perform"; _ } ->
    (* 'perform' を使う式を見つけた場合 *)
    Printf.printf "perform foundB: %s\n" (Pprintast.string_of_expression expr);
    ([], local_var_lst)
  | Pexp_let (_, vb_list, expr1) ->
    (* 'let' 式の場合は本体をトラバースする *)
    Printf.printf "let found: %s\n" (Pprintast.string_of_expression expr);
    (* vbを走査 *)
    Printf.printf "vb_list len: %d\n" (List.length vb_list);
    (* 以下の処理は要改善 *)
    let (tmp_tree_lst, new_local_var_lst) = List.fold_left (fun (tree_lst, var_lst) vb -> let (lst1, lst2) = find_perform_in_value_binding is_patern_search vb local_var_lst in (lst1@tree_lst, lst2@var_lst)) ([], []) vb_list in
    let (new_tree_lst, _) = find_perform_in_expr is_patern_search expr1 new_local_var_lst in
    let new_tree_lst = append_efNameTree_list tmp_tree_lst new_tree_lst in
    (new_tree_lst, local_var_lst)
  | Pexp_letmodule (_, _, expr1) ->
    (* 'let module' 式の場合は本体をトラバースする *)
    Printf.printf "letmodule found: %s\n" (Pprintast.string_of_expression expr);
    let new_tree_lst = find_perform_in_expr is_patern_search expr1 local_var_lst in
    new_tree_lst
  | Pexp_fun (_, _, pattern, expr1) ->
    (* 'fun' 式の場合は本体をトラバースする *)
    Printf.printf "fun found: %s\n" (Pprintast.string_of_expression expr);
    let pattern_lst = find_perform_in_pattern pattern [] in
    List.iter (fun name -> Printf.printf "pattern: %s\n" name) pattern_lst;
    let new_pattern_lst = List.map (fun name -> ArgsVar (name, Leaf)) pattern_lst in
    let new_tree = find_perform_in_expr is_patern_search expr1 (new_pattern_lst@local_var_lst) in
    new_tree
  | Pexp_match (expr1, cases) ->
    (* 'match' 式の場合は対象式とケースをトラバースする *)
    Printf.printf "match found: %s\n" (Pprintast.string_of_expression expr);
    let (tmp_tree_lst, _) = find_perform_in_expr is_patern_search expr1 local_var_lst in
    let new_tree_lst = find_perform_in_cases is_patern_search cases local_var_lst in (*ToDo caseeは未完成*)
    let new_tree_lst = List.map (fun tree -> add_efName_tree_list tree tmp_tree_lst) new_tree_lst in
    (new_tree_lst, local_var_lst)
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "perform"; _ }; _ }, _args) ->
    (* 'perform' を使う式を見つけた場合 未完成*)
    Printf.printf "perform foundC: %s\n" (Pprintast.string_of_expression expr);
    Printf.printf "args: %s\n" (Pprintast.string_of_expression (List.nth _args 0 |> snd));
    let new_tree_lst = List.map (fun (_, expr) -> find_perform_in_expr is_patern_search expr ~is_perform_name:true local_var_lst) (List.rev _args) in
    let new_tree_lst = List.fold_left (fun lst (tree, _) -> tree @ lst) [] new_tree_lst in
    (* Printf.printf "perform_name: %s\n" perform_name; *)
    (new_tree_lst, local_var_lst)
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
      let tmp_tree_lst = List.fold_left (fun lst expr -> (fst (find_perform_in_expr is_patern_search expr local_var_lst)) @ lst ) [] tmp_args in (* この部分は要改善 *)
      let tmp_func_name = extract_function_name (List.hd tmp_args) in (* argsの中で関数適用があった場合を未処理 *)
      let handler = analyze_handler handler_record local_var_lst in
      (* 一引数目は関数名のためskip *)
      let tmp_args = List.map (fun expr -> args_analys_expr expr) (List.tl tmp_args) in (* ToDo: Argsの中身について調べる *)
      let new_tree_lst = append_efNameTree tmp_tree_lst (Node (FunctionName (tmp_func_name, handler, local_var_lst, tmp_args), [])) in
      (new_tree_lst, local_var_lst)
    else
      let (tmp_tree_lst, arg_lst) = 
        List.fold_left (fun (tree_lst, arg_lst) (_, expr) -> 
          let (tmp_efName_tree, _) = find_perform_in_expr is_patern_search expr local_var_lst in
          let arg = args_analys_expr expr in
          match tree_lst with
          | [] -> (tmp_efName_tree, arg::arg_lst)
          | _ -> (List.map (fun tree -> add_efName_tree_list tree tmp_efName_tree) tree_lst, arg::arg_lst)
        ) ([], []) (List.rev args) 
      in
      let tmp_tree_lst = append_efNameTree tmp_tree_lst (Node (FunctionName (func_name, [], local_var_lst, arg_lst), [])) in
      Printf.printf "tmp_tree_lst len: %d\n" (List.length tmp_tree_lst);
      (tmp_tree_lst, local_var_lst)
  | Pexp_array exprs ->
    (* 配列の場合は要素をトラバースする *)
    Printf.printf "array found: %s\n" (Pprintast.string_of_expression expr);
    let new_tree_lst = List.fold_left (fun lst expr -> (fst (find_perform_in_expr is_patern_search expr local_var_lst)) @ lst ) [] exprs in
    (new_tree_lst, local_var_lst)
  | Pexp_tuple exprs ->
    (* タプルの場合は要素をトラバースする *)
    Printf.printf "tuple found: %s\n" (Pprintast.string_of_expression expr);
    let new_tree_lst = List.fold_left (fun lst expr -> (fst (find_perform_in_expr is_patern_search expr local_var_lst)) @ lst ) [] exprs in
    (new_tree_lst, local_var_lst)
  | Pexp_construct (name, Some expr1) ->
    (* コンストラクタの場合は引数をトラバースする *)
    Printf.printf "construct found: %s\n" (Pprintast.string_of_expression expr);
    Printf.printf "is_perform_name: %b\n" is_perform_name;
    Printf.printf "name: %s\n" (extract_ident_from_construct name );
    let (new_tree_lst, _) = find_perform_in_expr is_patern_search expr1 local_var_lst in
    if is_perform_name then 
      let new_tree_lst = append_efNameTree new_tree_lst (Node (EffectName (extract_ident_from_construct name), [])) in 
      (new_tree_lst, local_var_lst)
    else
      (new_tree_lst, local_var_lst)
  | Pexp_variant (_, Some expr1) ->
    (* バリアントの場合は引数をトラバースする *)
    Printf.printf "variant found: %s\n" (Pprintast.string_of_expression expr);
    let new_tree_lst = find_perform_in_expr is_patern_search expr1 local_var_lst in
    new_tree_lst
  | Pexp_record (fields, _) ->
    (* レコードの場合はフィールドをトラバースする *)
    Printf.printf "record found: %s\n" (Pprintast.string_of_expression expr);
    let new_tree_lst = List.fold_left (fun lst (_, expr) -> (fst (find_perform_in_expr is_patern_search expr local_var_lst )) @ lst ) [] fields in
    (new_tree_lst, local_var_lst)
  | Pexp_field (expr1, _) ->
    (* フィールド参照の場合はレコードをトラバースする *)
    Printf.printf "field found: %s\n" (Pprintast.string_of_expression expr);
    let new_tree_lst =  find_perform_in_expr is_patern_search expr1 local_var_lst in
    new_tree_lst
  | Pexp_setfield (expr1, _, expr2) ->
    (* フィールド代入の場合はレコードと値をトラバースする *)
    (* Printf.printf "setfield found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr is_patern_search expr1 perform_lst local_var_lst in
    let new_perform_lst =  find_perform_in_expr is_patern_search expr2 new_perform_lst local_var_lst in
    new_perform_lst *)
    ([], local_var_lst)
  | Pexp_ifthenelse (expr1, expr2, op_expr3) ->
    (* if式の場合は条件式とthen節とelse節をトラバースする *)
    Printf.printf "ifthenelse found: %s\n" (Pprintast.string_of_expression expr);
    let (route_tree, _) = find_perform_in_expr is_patern_search expr1 local_var_lst in
    let (if_tree, _) =  find_perform_in_expr is_patern_search expr2 local_var_lst in
    (match op_expr3 with
    | None -> 
      let new_tree_lst = append_efNameTree_list route_tree if_tree in
      (new_tree_lst, local_var_lst)
    | Some expr3 ->
      let (else_tree, _) =  find_perform_in_expr is_patern_search expr3 local_var_lst in
      let if_else_tree = if_tree @ else_tree in
      let new_tree_lst = append_efNameTree_list route_tree if_else_tree in
      (new_tree_lst, local_var_lst))
  | Pexp_sequence (expr1, expr2) ->
    (* シーケンスの場合は式1と式2をトラバースする *)
    Printf.printf "sequence found: %s\n" (Pprintast.string_of_expression expr);
    let (tmp_tree_lst, _) = find_perform_in_expr is_patern_search expr1 local_var_lst in
    let (new_tree_lst, _) =  find_perform_in_expr is_patern_search expr2 local_var_lst in
    let new_tree_lst = List.map (fun tree -> add_efName_tree_list tree new_tree_lst ) tmp_tree_lst in
    (new_tree_lst, local_var_lst)
  | Pexp_while (expr1, expr2) ->
    (* while式の場合は条件式と本体をトラバースする *)
    (* Printf.printf "while found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr is_patern_search expr1 perform_lst local_var_lst in
    let new_perform_lst =  find_perform_in_expr is_patern_search expr2 new_perform_lst local_var_lst in
    new_perform_lst *)
    ([], local_var_lst)
  | Pexp_for (pat, expr1, expr2, _, expr3) ->
    (* for式の場合はパターンと初期値と終了値と本体をトラバースする *)
    (* Printf.printf "for found: %s\n" (Pprintast.string_of_expression expr);
    let new_perform_lst = find_perform_in_expr is_patern_search expr1 perform_lst local_var_lst in
    let new_perform_lst =  find_perform_in_expr is_patern_search expr2 new_perform_lst local_var_lst in
    let new_perform_lst =  find_perform_in_expr is_patern_search expr3 new_perform_lst local_var_lst in
    new_perform_lst *)
    ([], local_var_lst)
  | Pexp_constraint (expr1, _) ->
    (* 型制約の場合は式をトラバースする *)
    Printf.printf "constraint found: %s\n" (Pprintast.string_of_expression expr);
    let new_tree_lst =  find_perform_in_expr is_patern_search expr1 local_var_lst in
    new_tree_lst
  | Pexp_coerce (expr1, _, _) ->
    (* 型変換の場合は式をトラバースする *)
    Printf.printf "coerce found: %s\n" (Pprintast.string_of_expression expr);
    let new_tree_lst =  find_perform_in_expr is_patern_search expr1 local_var_lst in
    new_tree_lst
  | Pexp_send (expr1, _) ->
    (* メッセージ送信の場合は式をトラバースする *)
    Printf.printf "send found: %s\n" (Pprintast.string_of_expression expr);
    let new_tree_lst =  find_perform_in_expr is_patern_search expr1 local_var_lst in
    new_tree_lst
  | Pexp_newtype (_, expr1) ->
    (* newtypeの場合は式をトラバースする *)
    Printf.printf "newtype found: %s\n" (Pprintast.string_of_expression expr);
    let new_tree_lst =  find_perform_in_expr is_patern_search expr1 local_var_lst in
    new_tree_lst
  | _ -> 
    Printf.printf "other found: %s\n" (Pprintast.string_of_expression expr);
    ([], local_var_lst)
and find_perform_in_cases is_patern_search cases local_var_lst :efNameTree list = match is_patern_search with
  (* ガードは今は未対応 *)
  (* handlerは即座にmatchする場合に限る *)
  | Effect -> 
    let tmp_perform_lst = Effc (List.fold_left (fun lst case -> ((find_perform_in_case case local_var_lst)) @ lst) [] cases) in
    (* let new_perform_lst = perform_lst_append FunctionName ("effect", [tmp_perform_lst])] perform_lst in
    new_perform_lst *)
    [Node (FunctionName (" ",[tmp_perform_lst], local_var_lst, []), [])]
  | Exception -> []
  | Other -> 
    let tmp_perform_lst = List.fold_left (fun lst case -> ((find_perform_in_case case local_var_lst)) @ lst) [] cases in
    let tmp_perform_lst = remove_duplicate_case tmp_perform_lst [] in
    let tree_lst = List.map (fun (_, tree) -> tree) tmp_perform_lst in
    tree_lst
and find_perform_in_case case local_var_lst :(string * efNameTree) list =
  (* ガードは未対応 *)
  let pattern_lst: string list = find_perform_in_pattern case.pc_lhs [] in
  let tmp_perform_lst = List.map (fun name -> (name, Node (Empty , (fst (find_perform_in_expr Other case.pc_rhs local_var_lst))))) pattern_lst in
  tmp_perform_lst
and find_perform_in_value_binding is_patern_search vb local_var_lst: (efNameTree list * localVar list) =
  Printf.printf "find_perform_in_value_binding is_patern_search: %s\n" (Pprintast.string_of_expression vb.pvb_expr);
  let pattern_lst: string list = find_perform_in_pattern vb.pvb_pat [] in (* 関数名を取得 *)
  List.iter (fun name -> Printf.printf "pattern: %s\n" name) pattern_lst;
  let (tmp_tree_lst, tmp_local_var) = find_perform_in_expr is_patern_search vb.pvb_expr local_var_lst in
  if pattern_lst = [] || (List.hd pattern_lst = "_") then (* ワイルドカードの場合は記憶しておく必要がないので，local_var_lstには追加しない *)
    (tmp_tree_lst, local_var_lst)
  else
    let new_local_var = create_localVar (List.hd pattern_lst) tmp_local_var local_var_lst tmp_tree_lst in
    (match new_local_var with
      | None -> (tmp_tree_lst, (LocalVar ((List.hd pattern_lst), [], Leaf))::local_var_lst)
      | Some new_local_var -> ([], new_local_var::local_var_lst)
    )
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
  let (lst1,_) = find_perform_in_expr (pattern_type_of_string name1) field1 local_var_lst in
  Printf.printf "lst1 len: %d\n" (List.length lst1);
  let effect_handler = efNameOfHandler_list_from_efName_list (List.hd lst1) Effect in
  let name2 = extract_ident_from_construct name2 in
  Printf.printf "name2: %s\n" name2;
  let (lst2, _) = find_perform_in_expr (pattern_type_of_string name2) field2 local_var_lst in
  let lst2 = if (List.length lst2 = 0) then Leaf else (List.hd lst2) in
  let exception_handler = efNameOfHandler_list_from_efName_list lst2 Exception in
  let name3 = extract_ident_from_construct name3 in
  Printf.printf "name3: %s\n" name3;
  let (lst3, _) = find_perform_in_expr (pattern_type_of_string name3) field3 local_var_lst in
  let lst3 = if (List.length lst3 = 0) then Leaf else (List.hd lst3) in
  let other_handler = efNameOfHandler_list_from_efName_list lst3 Other in
  let handler = effect_handler @ exception_handler @ other_handler in
  Printf.printf "handler len: %d\n" (List.length handler);
  handler
  | _ -> []
(* 引数のexprを解析して適切な形に変換する *)
and args_analys_expr expr = match expr.pexp_desc with
  | Pexp_constant _ -> ArgValue
  | Pexp_ident name -> ArgVar (extract_ident_from_construct name)
  | _ -> ArgValue (* あとで変更する *)


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
    let function_info = extract_function_name_and_arg_num_from_vb_list value_bindings in
    (* 相互再帰には未対応 *)
    (* let tree_lst = List.fold_left (fun lst vb -> (fst (find_perform_in_expr Other vb.pvb_expr [])) @ lst) [] value_bindings in (* 今はここでlocalVar listは捨てている *)
    Printf.printf "tree_lst len: %d\n" (List.length tree_lst); *)
    let (tree_lst, local_var_lst) = find_perform_in_expr Other (List.hd value_bindings).pvb_expr [] in
    Printf.printf "local_var_lst len: %d\n" (List.length local_var_lst);
    let tree = Node (Empty, tree_lst) in
    (* 引数の順番を正しいものに変更 *)
    Some (function_info, tree, (List.rev local_var_lst))
  | _ -> None

let print_perform_expressions structure =
  let result = List.map (fun item -> find_perform_expr_in_structure_item item) structure in
  let result = List.filter_map (fun item -> item) result in
  List.iter (fun ((function_name, args), tree, _) ->
    Printf.printf "function name: %s\n" function_name;
    Printf.printf "args len: %d\n" args; 
    print_endline (efNameTree_to_string tree)) result;
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

let effect_row_test filename = 
  let parsed_file = parse_test_ocaml_file filename in 
  let result = analyze_function_call [] parsed_file in
  result 

(* let main () =
  let parsed_file = parse_test_ocaml_file Sys.argv.(1) in 
  let result = analyze_function_call [] (parsed_file) in
  List.iter (fun (function_info, perform_lst) ->
        Printf.printf "function_name: %s\n" (fst function_info);
        print_endline (efNameTree_to_string perform_lst);) result;
  () *)

let main () =
  parse_ocaml_file Sys.argv.(1);
  ()

(* 型チェックの実行 *)
(* let type_check ast =
  Compmisc.init_path (); (* これがないとエラーになる *)
  let typed_tree = Typemod.type_structure (Compmisc.initial_env ()) ast in
  typed_tree

let main () = 
  let ast = parse_implementation ~tool_name:"my_ocaml_parser" Sys.argv.(1) in
  let typed_tree = type_check ast in
  let (structure, _ , _, _, _) = typed_tree in
  print_structure structure;
  () *)