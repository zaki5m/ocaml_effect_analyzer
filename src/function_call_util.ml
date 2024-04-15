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
  | FunctionName (name, _, _, _) -> name = function_name
  | _ -> false

(* efNameからefNameOfHandlerのlistを返す関数 *)
let extract_efNameOfHandler_from_efName (efName: efName) = match efName with
  | FunctionName (_, handler, _, _) -> handler
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
      | Effect -> [Effc [("_", efName_tree, [])]]
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
  | Leaf -> Node (Empty, append_tree)
  | Node (efName, []) -> Node (efName, append_tree)
  | Node (efName, lst) -> Node (efName, List.map (fun new_tree -> add_efName_tree_list new_tree append_tree) lst)

(* treeのLeafにのみ新たなNodeを追加する関数 *)
let rec add_efName_tree_to_leaf (tree: efNameTree) (append_tree: efNameTree) = match tree with
  | Leaf -> append_tree
  | Node (efName, lst) -> Node (efName, List.map (fun new_tree -> add_efName_tree_to_leaf new_tree append_tree) lst)

(* treeのLeafにのみ新たなNodeを追加する関数 (appendするのがlist version) *)
let rec add_efName_tree_list_to_leaf (tree: efNameTree) (append_tree: efNameTree list) = match tree with
  | Leaf -> Node (Empty, append_tree)
  | Node (efName, lst) -> Node (efName, List.map (fun new_tree -> add_efName_tree_list_to_leaf new_tree append_tree) lst)

(* efNameTreeのリストを結合する関数 *)
let append_efNameTree (tree_lst: efNameTree list) (append_tree: efNameTree) = match tree_lst with
  | [] -> [append_tree]
  | _ -> List.map (fun tree -> add_efName_tree tree append_tree) tree_lst

(* efNameTreeのリストを結合する関数 (list version) *)
let append_efNameTree_list (tree_lst: efNameTree list) (append_tree: efNameTree list) = match tree_lst with
  | [] -> append_tree
  | _ -> List.map (fun tree -> add_efName_tree_list tree append_tree) tree_lst

(*  --------efNameTreeWithId版--------- *)
(* treeのLeafまで走査して，新たなNodeを追加する関数 *)
let rec add_efName_tree_with_id (tree: efNameTreeWithId) (append_tree: efNameTreeWithId) = match tree with
  | LeafWithId _ -> append_tree
  | NodeWithId (efName, [], id) -> NodeWithId (efName, [append_tree], id)
  | NodeWithId (efName, lst, id) -> NodeWithId (efName, List.map (fun new_tree -> add_efName_tree_with_id new_tree append_tree) lst, id)
  | RecNodeWithId id -> RecNodeWithId id
  | ConditionWithId (condition_lst, [], id) -> ConditionWithId (condition_lst, [append_tree], id)
  | ConditionWithId (condition_lst, lst, id) -> ConditionWithId (condition_lst, List.map (fun new_tree -> add_efName_tree_with_id new_tree append_tree) lst, id)

(* treeのLeafまで走査して，新たなNodeを追加する関数 (appendするのがlist version) *)
let rec add_efName_tree_with_id_list (tree: efNameTreeWithId) (append_tree: efNameTreeWithId list) = match tree with
  | LeafWithId id -> NodeWithId (Empty, append_tree, id)
  | NodeWithId (efName, [], id) -> NodeWithId (efName, append_tree, id)
  | NodeWithId (efName, lst, id) -> NodeWithId (efName, List.map (fun new_tree -> add_efName_tree_with_id_list new_tree append_tree) lst, id)
  | RecNodeWithId id -> RecNodeWithId id
  | ConditionWithId (condition_lst, [], id) -> ConditionWithId (condition_lst, append_tree, id)
  | ConditionWithId (condition_lst, lst, id) -> ConditionWithId (condition_lst, List.map (fun new_tree -> add_efName_tree_with_id_list new_tree append_tree) lst, id)

(* treeのLeafにのみ新たなNodeを追加する関数 *)
let rec add_efName_tree_with_id_to_leaf (tree: efNameTreeWithId) (append_tree: efNameTreeWithId) = match tree with
  | LeafWithId _ -> append_tree
  | NodeWithId (efName, lst, id) -> NodeWithId (efName, List.map (fun new_tree -> add_efName_tree_with_id_to_leaf new_tree append_tree) lst, id)
  | RecNodeWithId id -> RecNodeWithId id
  | ConditionWithId (condition_lst, lst, id) -> ConditionWithId (condition_lst, List.map (fun new_tree -> add_efName_tree_with_id_to_leaf new_tree append_tree) lst, id)

(* treeのLeafにのみ新たなNodeを追加する関数 (appendするのがlist version) *)
let rec add_efName_tree_with_id_list_to_leaf (tree: efNameTreeWithId) (append_tree: efNameTreeWithId list) = match tree with
  | LeafWithId id -> NodeWithId (Empty, append_tree, id)
  | NodeWithId (efName, lst, id) -> NodeWithId (efName, List.map (fun new_tree -> add_efName_tree_with_id_list_to_leaf new_tree append_tree) lst, id)
  | RecNodeWithId id -> RecNodeWithId id
  | ConditionWithId (condition_lst, lst, id) -> ConditionWithId (condition_lst, List.map (fun new_tree -> add_efName_tree_with_id_list_to_leaf new_tree append_tree) lst, id)

(* efNameTreeのリストを結合する関数 *)
let append_efNameTree_with_id (tree_lst: efNameTreeWithId list) (append_tree: efNameTreeWithId) = match tree_lst with
  | [] -> [append_tree]
  | _ -> List.map (fun tree -> add_efName_tree_with_id tree append_tree) tree_lst

(* efNameTreeのリストを結合する関数 (list version) *)
let append_efNameTree_with_id_list (tree_lst: efNameTreeWithId list) (append_tree: efNameTreeWithId list) = match tree_lst with
  | [] -> append_tree
  | _ -> List.map (fun tree -> add_efName_tree_with_id_list tree append_tree) tree_lst
(*  --------efNameTreeWithId版--------- *)


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

(* effect名とlocal_var_lstからlocalVarを作成する関数 *)
let create_localVar_from_effect (effect_name: string) (tmp_local_var_lst: localVar list) (pre_local_var_lst: localVar list) (tree: efNameTree) = 
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
    (effect_name, tree, [])
  else
    (effect_name, tree, args_lst)

(* パターンマッチのケースを解析する *)

(* localVar listから変数名が一致するものを探して取り出す関数 *)
let rec find_local_var (name: string) (current_eval: localVar list) = match current_eval with
  | [] -> None
  | hd :: tl -> match hd with
    | LocalVar (n, _, _) -> if n = name then Some hd else find_local_var name tl
    | ArgsVar (n, _) -> if n = name then Some hd else find_local_var name tl
    | _ -> find_local_var name tl

let rec handler_lst_analyze lst effect_lst exception_lst ret_lst = match lst with
  | [] -> (effect_lst, exception_lst, ret_lst)
  | hd :: tl -> (match hd with
    | Effc effect_lst -> handler_lst_analyze tl effect_lst exception_lst ret_lst
    | Exnc exception_lst -> handler_lst_analyze tl effect_lst exception_lst ret_lst
    | Retc ret_lst -> handler_lst_analyze tl effect_lst exception_lst ret_lst)

let any_exist_wildcard effect_lst = 
  let result = List.find_opt (fun (name, _, _) -> name = "_") effect_lst in
  match result with
  | Some (_, ef_lst, _) -> ef_lst
  | None -> Leaf

(* continueがある場合に継続を後ろに付け加える *)
let rec analyze_handler_serch_continue (tree: efNameTreeWithId) continuation_tree_lst = match tree with
  | LeafWithId id -> LeafWithId id
  | NodeWithId (efName, lst, id) -> (match efName with 
    | FunctionName (name, tmp_handler,  local_var_lst, arg_lst) ->
      Printf.printf "arg_lst: %s\n" (List.fold_left (fun acc arg -> acc ^  arg_to_string arg) "" arg_lst);
      Printf.printf "local_var_lst: %s\n" (List.fold_left (fun acc local_var -> acc ^  local_var_to_string local_var) "" local_var_lst);
      if name = "continue" then 
        NodeWithId (Empty, continuation_tree_lst, id)
      else 
        NodeWithId (efName, (List.map (fun tree -> analyze_handler_serch_continue tree continuation_tree_lst) lst), id)
    (* | Conditions condition_lst -> Some (NodeWithId (Conditions (List.filter_map (fun tree -> analyze_handler_serch_continue tree continuation_tree) condition_lst), (List.filter_map (fun tree -> analyze_handler_serch_continue tree) lst), id)) *)
    | _ -> NodeWithId (efName, (List.map (fun tree -> analyze_handler_serch_continue tree continuation_tree_lst) lst), id)
  )
  | RecNodeWithId id -> RecNodeWithId id
  | ConditionWithId (condition_lst, lst, id) -> ConditionWithId (List.map (fun tree -> analyze_handler_serch_continue tree continuation_tree_lst) condition_lst, (List.map (fun tree -> analyze_handler_serch_continue tree continuation_tree_lst) lst), id)

let rec add_continue_efNameTree handler tree = match tree with 
  | Leaf -> Leaf
  | Node (efName, lst) -> (match efName with 
    | FunctionName (name, _,  local_var_lst, arg_lst) ->
      if name = "continue" then 
        let new_efName = FunctionName (name, handler, local_var_lst, arg_lst) in
        Node (new_efName, (List.map (fun tree -> add_continue_efNameTree handler tree) lst))
      else 
        Node (efName, (List.map (fun tree -> add_continue_efNameTree handler tree) lst))
    | _ -> Node (efName, (List.map (fun tree -> add_continue_efNameTree handler tree) lst))
  )

let rec add_continue_effect_handler handler effect_handler = match effect_handler with
  | [] -> []
  | (name, tree, local_var_lst) :: tl -> 
    (name, add_continue_efNameTree handler tree, local_var_lst):: add_continue_effect_handler handler tl


(* Deep handler内のcontinueに対して同一のhandlerを付与する *)
let add_continue_handler (handler: efNameOfHandler list) = 
  let rec loop lst = match lst with
    | [] -> []
    | hd :: tl -> (match hd with
      | Effc lst -> Effc (add_continue_effect_handler handler lst) :: loop tl
      | _ -> hd :: loop tl
    )
  in
  loop handler 

(* treeにid(-1)を付与する *)
let rec add_id_to_tree tree  = match tree with
  | Leaf -> LeafWithId (-1)
  | Node (efName, lst) -> NodeWithId (efName, List.map (fun tree -> add_id_to_tree tree) lst, -1)

(* tree_with_idをtreeに戻す *)
(* 多分この部分後で変更しないとダメな気がする *)
let rec remove_id_from_tree tree_with_id = match tree_with_id with
  | LeafWithId _ -> Leaf
  | NodeWithId (efName, lst, _) -> Node (efName, List.map (fun tree -> remove_id_from_tree tree) lst)
  | RecNodeWithId _ -> Node (Empty, [])
  | ConditionWithId (condition_lst, lst, _) -> Node (Conditions (List.map (fun tree -> remove_id_from_tree tree) condition_lst), List.map (fun tree -> remove_id_from_tree tree) lst)

(* treeにidを任意の数だけ追加する，また最大値の次の値を返す *)
let add_id_to_tree_with_id tree add_id = 
  let ref_max_id = ref 0 in
  let rec loop tree = match tree with
    | LeafWithId id -> 
      if id > !ref_max_id then
        ref_max_id := id;
      LeafWithId (add_id + id)
    | NodeWithId (efName, lst, id) -> 
      if id > !ref_max_id then
        ref_max_id := id;
      NodeWithId (efName, List.map (fun tree -> loop tree) lst, add_id + id)
    | RecNodeWithId id -> 
      if id > !ref_max_id then
        ref_max_id := id;
      RecNodeWithId (add_id + id)
    | ConditionWithId (condition_lst, lst, id) -> 
      if id > !ref_max_id then
        ref_max_id := id;
      ConditionWithId (List.map (fun tree -> loop tree) condition_lst, List.map (fun tree -> loop tree) lst, add_id + id)
  in
  let result = loop tree in
  if !ref_max_id <= 0 then
    (result, add_id)
  else
    (result, !ref_max_id + add_id + 1)

(* treeの中の最大値の次の値(next_id)の検索 *)
let search_next_id tree =
  let ref_max_id = ref 0 in
  let rec loop tree = match tree with
    | LeafWithId id -> 
      if id > !ref_max_id then
        ref_max_id := id
    | NodeWithId (efName, lst, id) -> 
      if id > !ref_max_id then
        ref_max_id := id
      else
        List.iter (fun tree -> loop tree) lst
    | RecNodeWithId id -> 
      if id > !ref_max_id then
        ref_max_id := id
    | ConditionWithId (condition_lst, lst, id) ->
      if id > !ref_max_id then
        ref_max_id := id
      else
        List.iter (fun tree -> loop tree) lst
  in
  loop tree;
  !ref_max_id + 1

(* Emptyを削除したTreeWithIdを再構築 *)
let remove_empty_from_tree_with_id tree = 
  let rec loop (tree: efNameTreeWithId) = match tree with 
    | LeafWithId _ -> [LeafWithId (-1)]
    | NodeWithId (efName, lst, id) -> (match efName with 
      | Empty -> loop2 lst
      | _ -> [NodeWithId (efName, loop2 lst, id)]
    )
    | RecNodeWithId id -> [RecNodeWithId id]
    | ConditionWithId (condition_lst, lst, id) -> 
      let new_condition_lst = loop2 condition_lst in
      let new_lst = loop2 lst in
      [ConditionWithId (new_condition_lst, new_lst, id)]
    and loop2 (lst: efNameTreeWithId list) = match lst with
      | [] -> []
      | hd :: tl -> (match loop hd with
        | [] -> loop2 tl
        | lst -> lst @ (loop2 tl)
      )
  in
  (loop tree) |> List.hd

(* Emptyを削除したTreeWithIdを再構築 *)
(* let rec remove_empty_from_tree_with_id tree = match tree with
  | LeafWithId _ -> LeafWithId (-1)
  | NodeWithId (efName, lst, id) -> 
    let not_leaf_lst = List.filter (fun tree -> 
      match tree with
      | LeafWithId _ -> false
      | _ -> true
       ) lst in 
    let (empty_lst, not_empty_lst) = List.partition (fun tree -> 
      match tree with
      | NodeWithId (efName, _, _) -> 
        (match efName with
          | Empty -> true
          | _ -> false
        )
      | _ -> false
       ) not_leaf_lst in
    let new_not_empty_lst = List.map (fun tree -> remove_empty_from_tree_with_id tree) not_empty_lst in
    let new_empty_lst = List.fold_left (fun tmp_lst tree -> (
      match tree with
      | NodeWithId (_, lst, _) -> lst @ tmp_lst
      | _ -> tmp_lst
    )) [] empty_lst in
    let new_empty_lst = List.map (fun tree -> remove_empty_from_tree_with_id tree) new_empty_lst in
    NodeWithId (efName, new_empty_lst @ new_not_empty_lst, id)
  | RecNodeWithId id -> RecNodeWithId id
  | ConditionWithId (condition_lst, lst, id) -> 
    let not_leaf_lst = List.filter (fun tree -> 
      match tree with
      | LeafWithId _ -> false
      | _ -> true
       ) lst in 
    let (empty_lst, not_empty_lst) = List.partition (fun tree -> 
      match tree with
      | NodeWithId (efName, _, _) -> 
        (match efName with
          | Empty -> true
          | _ -> false
        )
      | _ -> false
       ) not_leaf_lst in
    let new_not_empty_lst = List.map (fun tree -> remove_empty_from_tree_with_id tree) not_empty_lst in
    let new_empty_lst = List.fold_left (fun tmp_lst tree -> (
      match tree with
      | NodeWithId (_, lst, _) -> lst @ tmp_lst
      | _ -> tmp_lst
    )) [] empty_lst in
    let new_empty_lst = List.map (fun tree -> remove_empty_from_tree_with_id tree) new_empty_lst in
    ConditionWithId (condition_lst, new_empty_lst @ new_not_empty_lst, id) *)

(* TrreWithIdから後続のlstを取得する関数 *)
let get_lst_from_tree_with_id tree = match tree with
  | NodeWithId (_, lst, _) -> lst
  | _ -> []


(* RecNodeが指すidのリストを返す関数 *)  
let rec search_rec_id tree = match tree with
  | LeafWithId _ -> []
  | NodeWithId (_, lst, _) -> 
    let rec_id_lst = List.fold_left (fun tmp_lst tree -> (
      search_rec_id tree @ tmp_lst
    )) [] lst in
    rec_id_lst
  | RecNodeWithId id -> [id]
  | ConditionWithId (condition_lst, lst, _) -> 
    let rec_id_lst = List.fold_left (fun tmp_lst tree -> (
      search_rec_id tree @ tmp_lst
    )) [] condition_lst in
    let rec_id_lst2 = List.fold_left (fun tmp_lst tree -> (
      search_rec_id tree @ tmp_lst
    )) [] lst in
    rec_id_lst @ rec_id_lst2

(* RecNodeがないRootを全て削除する関数 *)
let remove_rec_node_from_tree tree = 
  let id_lst = search_rec_id tree in
  let initial = ref true in 
  let rec loop tree = 
    match tree with
    | NodeWithId (efName, lst, id) -> 
      if List.exists (fun tmp_id -> tmp_id = id) id_lst || !initial then
        (initial := false;
        Some (NodeWithId (efName, loop2 lst, id)))
      else
        (match efName with
          | Empty -> None
          | Root -> None
          | _ -> Some (NodeWithId (efName, loop2 lst, id))
        )
    | LeafWithId id -> Some (LeafWithId id)
    | RecNodeWithId id -> Some (RecNodeWithId id)
    | ConditionWithId (condition_lst, lst, id) -> 
      if List.exists (fun tmp_id -> tmp_id = id) id_lst || !initial then
        (initial := false;
        Some (ConditionWithId (loop2 condition_lst, loop2 lst, id)))
      else
        (match condition_lst with
          | [] -> None
          | _ -> Some (ConditionWithId (loop2 condition_lst, loop2 lst, id))
        )
  and loop2 tree_lst = match tree_lst with
    | [] -> []
    | hd :: tl -> (match loop hd with
      | Some tree -> tree :: loop2 tl
      | None -> 
        let lst = get_lst_from_tree_with_id hd in
        loop2 lst @ loop2 tl
    )
  in
  match loop tree with 
  | Some tree -> tree
  | None -> LeafWithId (-1)