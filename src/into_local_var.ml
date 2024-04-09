open Effect_analyzer_core
open Function_call_util
open Efname_formatter
open Effect_row_to_graph


let change_handler (tree: efNameTreeWithId) (handler: efNameOfHandler list) local_var_lst next_id = 
  let (effect_lst, exception_lst, ret_tree) = handler_lst_analyze handler [] [] Leaf in
  let ref_next_id = ref next_id in
  let rec loop (tree: efNameTreeWithId) = match tree with
    | LeafWithId id -> LeafWithId id
    | NodeWithId (efName, [], id) -> 
      let (ret_tree_with_id, next_id) = add_id_to_efNameTree ret_tree !ref_next_id in
      ref_next_id := next_id;
      NodeWithId (efName, [ret_tree_with_id], id)
    | NodeWithId (efName, tmp_tree_lst, id) -> (match efName with
      | EffectName (name, _, _) -> 
        (* tmp_tree_lstが継続 *)
        (* 実際には継続は関数が返ってbindすることもあるが，その場合は現在未考慮 *)
        let result = List.find_opt (fun (tmp_name, _, _) -> name = tmp_name) effect_lst in
        (match result with
        | Some (_, tmp_tree, local_var_lst) ->
          (* let continue_tree = Node (FunctionName (" ", handler , tmp_tree_lst, []), []) in *)
          Printf.printf "name: %s, tmp_tree: %s\n" name (efNameTree_to_string tmp_tree);
          let tmp_tree = analyze_handler_serch_continue tmp_tree in
          (match tmp_tree with
          | Some tree ->
            let (next_tree, next_id) = add_id_to_efNameTree tree !ref_next_id in
            Printf.printf "next_tree: %s\n" (efNameTreeWithId_to_string next_tree);
            ref_next_id := next_id;
            let new_tree = (NodeWithId (efName, [loop next_tree], id)) in
            add_efName_tree_with_id_list_to_leaf new_tree (List.map (fun tree ->  loop tree) tmp_tree_lst)
          | None -> 
            let new_tree = (NodeWithId (efName, [], id)) in
            add_efName_tree_with_id_list_to_leaf new_tree (List.map (fun tree ->  loop tree) tmp_tree_lst)
          )
        | None -> 
          let tmp_tree = any_exist_wildcard effect_lst in
          let tmp_tree = analyze_handler_serch_continue tmp_tree in
          (match tmp_tree with
          | Some tree ->
            let (next_tree, next_id) = add_id_to_efNameTree tree !ref_next_id in
            ref_next_id := next_id;
            let new_tree = (NodeWithId (efName, [loop next_tree], id)) in
            add_efName_tree_with_id_list_to_leaf new_tree (List.map (fun tree ->  loop tree) tmp_tree_lst)
          | None -> 
            let new_tree = (NodeWithId (efName, [], id)) in
            add_efName_tree_with_id_list_to_leaf new_tree (List.map (fun tree ->  loop tree) tmp_tree_lst)
          )
        )
      | _ -> NodeWithId (efName, (List.map (fun tree ->  loop tree) tmp_tree_lst), id)
    )
    | RecNodeWithId id -> RecNodeWithId id
    | ConditionWithId (lst1, lst2, id) -> 
      let new_lst1 = List.map (fun tree -> loop tree) lst1 in
      let new_lst2 = List.map (fun tree -> loop tree) lst2 in
      ConditionWithId (new_lst1, new_lst2, id)
  in
  (loop tree, !ref_next_id) 

(* 関数の引数を具体的なものに変更し，その結果を返す関数 *)
(* (引数，mappingする引数もしくは値) list, まだ適用されていないリスト *)
let change_arg_to_concrete exp_lst (arg_lst: arg list) (local_var_lst: localVar list) (current_eval: localVar list) local_arg_lst = 
  let rec loop arg_lst rest_local_var_lst mapped_lst = match arg_lst, rest_local_var_lst with
    | [], _ -> (mapped_lst, rest_local_var_lst)
    | _, [] -> (mapped_lst, rest_local_var_lst)
    | arg_hd :: arg_tl, local_var_hd :: local_var_tl -> 
      Printf.printf "arg_hd: %s\n" (arg_to_string arg_hd);
      (match arg_hd with
      | ArgVar name -> 
        let new_local_var = find_local_var name current_eval in
        (match new_local_var with
          | Some new_local_var -> 
              loop arg_tl local_var_tl ((local_var_hd, new_local_var) :: mapped_lst)
          | None -> 
            let new_local_var = find_local_var name local_arg_lst in
            (match new_local_var with
            | Some new_local_var -> 
              loop arg_tl local_var_tl ((local_var_hd, new_local_var) :: mapped_lst)
            | None -> 
              let result = List.find_opt (fun (((n, _), _, _), _) -> n = name) exp_lst in
              (match result with
                | Some ((_, tree, local_var_lst), _) -> 
                  (* ここでefNameTreeWithIdからefNameTreeに変換 *)
                  let tree = remove_id_from_tree tree in
                  loop arg_tl local_var_tl ((local_var_hd, LocalVar (name, local_var_lst, tree)) :: mapped_lst)
                | None ->
                  loop arg_tl local_var_tl ((local_var_hd, EmptyVar) :: mapped_lst)
              )
            )
        )
      | _ -> loop arg_tl local_var_tl ((local_var_hd, EmptyVar) :: mapped_lst)
      )
  in
  loop arg_lst local_var_lst []

(* 現在の環境の中身を適切なものに置き換える関数 *)
let change_env_to_concrete (env: localVar list) (arg_lst: (localVar * localVar) list) = 
  let rec loop env arg_lst tmp = match env with
    | [] -> tmp
    | hd :: tl -> 
      (match hd with
      | ArgsVar (name, _) -> 
        let (result, rest_arg_lst) = List.partition (fun (local_var, _) -> local_var = hd) arg_lst in
        (match result with
         | [] -> loop tl arg_lst (hd::tmp)
          | (_, new_local_var) :: _ -> 
            let new_local_var = 
              (match new_local_var with
              | LocalVar (_, local_var_lst, tree) -> LocalVar (name, local_var_lst, tree)
              | ArgsVar (_, tree) -> ArgsVar (name, tree)
              | EmptyVar -> EmptyVar
              )
            in
            loop tl rest_arg_lst (new_local_var :: tmp)
        )
      | _ -> loop tl arg_lst (hd::tmp)
      )
  in
  loop (List.rev env) arg_lst []
  
    


(* treeをlocal_varが正しく入った状態のtreeに変換する関数 *)
(* handlerの中身に関しては未処理 *)
let rec change_tree_to_concrete (tree: efNameTreeWithId) (arg_lst: (localVar * localVar) list) = match tree with
  | LeafWithId id -> LeafWithId id
  | NodeWithId (efName, lst, id) -> (match efName with
    | FunctionName (name, handler, current_eval, tmp_arg_lst) -> 
      let new_current_eval = change_env_to_concrete current_eval arg_lst in
      NodeWithId (FunctionName (name, handler, new_current_eval, tmp_arg_lst), List.map (fun tree -> change_tree_to_concrete tree arg_lst) lst, id)
    | EffectName (name, local_var_lst, _) -> NodeWithId (EffectName (name, local_var_lst, []), List.map (fun tree -> change_tree_to_concrete tree arg_lst) lst, id)
    | Empty -> NodeWithId (Empty, List.map (fun tree -> change_tree_to_concrete tree arg_lst) lst, id)
    | Root -> NodeWithId (Root, List.map (fun tree -> change_tree_to_concrete tree arg_lst) lst, id)
    | _ -> LeafWithId id (* Conditionは後で要改善 *)
  )
  | RecNodeWithId id -> RecNodeWithId id
  | ConditionWithId (lst1, lst2, id) -> ConditionWithId (lst1, lst2, id)
    

(* treeをlocal_varが正しく入った状態のtgreeに変換する関数 *)


(* (tree, handler, この関数の引数の実際に入ってくる値に置き換えたもの), 現在のscope内の環境 *)
(* current_idは今のid，next_idはtreeを増やす際の次のidを表している *)
let rec change_efName (exp_lst: (((string * int) * efNameTreeWithId * localVar list) * bool) list) efName (local_var_lst: localVar list) current_id next_id (now_function: (string * int)) : ((efNameTreeWithId * efNameOfHandler list * localVar list)  * bool * int) = match efName with
  | FunctionName (name, lst, current_eval, arg_lst) -> 
    if name = "continue" then (* continueの場合は残しておく *)
      ((NodeWithId (efName, [], current_id), lst, []), false, next_id) (* この部分は要改善 *)
    else if name = (fst now_function) then
      ((RecNodeWithId (snd now_function), [], []), false, next_id)
    else
      let result = List.find_opt (fun (((n,_), _, _), _) -> n = name) exp_lst in
      (match result with
      | Some ((_, tmp_lst, tmp_local_var_lst), used_args) -> 
        (* idの書き換えが必要 *)
        if used_args then
          let (tmp_arg_lst, rest_local_var) = change_arg_to_concrete exp_lst arg_lst tmp_local_var_lst current_eval local_var_lst in (* 部分適用を考える際にはこの部分は必須 *)
          (* ここでtreeの環境の変換を行う *)
          Printf.printf "tmp_arg_lst: %d\n" (List.length tmp_arg_lst);
          Printf.printf "efname: %s\n" (efNameTreeWithId_to_string tmp_lst);
          List.iter (fun (a, b) -> Printf.printf "a: %s, b: %s\n" (local_var_to_string a) (local_var_to_string b)) tmp_arg_lst;
          let tmp_lst = change_tree_to_concrete tmp_lst tmp_arg_lst in
          Printf.printf "new_efname: %s\n" (efNameTreeWithId_to_string tmp_lst);
          let (tmp_lst, next_id) = add_id_to_tree_with_id tmp_lst next_id in
          ((tmp_lst, lst, rest_local_var), false, next_id)
        else
          let (tmp_lst, next_id) = add_id_to_tree_with_id tmp_lst next_id in
          let (_, rest_local_var) = change_arg_to_concrete exp_lst arg_lst tmp_local_var_lst current_eval local_var_lst in 
          ((tmp_lst, lst, rest_local_var), false, next_id)
      | None -> 
        let result = find_local_var name local_var_lst in
        (match result with
        | Some (LocalVar (_, tmp_local_var_lst, tmp_tree)) -> (* 要改善 *)
          let (tmp_arg_lst, rest_local_var) = change_arg_to_concrete exp_lst arg_lst tmp_local_var_lst current_eval local_var_lst in (* 部分適用を考える際にはこの部分は必須 *)
          (* ここでtreeの環境の変換を行う *)
          Printf.printf "tmp_arg_lst: %d\n" (List.length tmp_arg_lst);
          Printf.printf "efname: %s\n" (efNameTree_to_string tmp_tree);
          List.iter (fun (a, b) -> Printf.printf "a: %s, b: %s\n" (local_var_to_string a) (local_var_to_string b)) tmp_arg_lst;
          let (tmp_tree, next_id) = add_id_to_efNameTree tmp_tree next_id in
          let tmp_lst = change_tree_to_concrete tmp_tree tmp_arg_lst in
          ((tmp_lst, lst, rest_local_var), false, next_id)
        | Some (ArgsVar (_, _)) ->
          Printf.printf "🥶args_name: %s\n" name;
          ((NodeWithId (efName, [],current_id), lst, []), true, next_id)
        | _ -> 
          let result = find_local_var name current_eval in
          (match result with
          | Some (LocalVar (_, tmp_local_var_lst, tmp_tree)) -> (* 要改善 *)
            let (tmp_arg_lst, rest_local_var) = change_arg_to_concrete exp_lst arg_lst tmp_local_var_lst current_eval local_var_lst in (* 部分適用を考える際にはこの部分は必須 *)
            (* ここでtreeの環境の変換を行う *)
            Printf.printf "tmp_arg_lst: %d\n" (List.length tmp_arg_lst);
            Printf.printf "efname: %s\n" (efNameTree_to_string tmp_tree);
            List.iter (fun (a, b) -> Printf.printf "a: %s, b: %s\n" (local_var_to_string a) (local_var_to_string b)) tmp_arg_lst;
            let (tmp_tree, next_id) = add_id_to_efNameTree tmp_tree next_id in
            let tmp_lst = change_tree_to_concrete tmp_tree tmp_arg_lst in
            ((tmp_lst, lst, rest_local_var), false, next_id)
          | _ ->
            (LeafWithId current_id, [], []), false, next_id) (* ユーザ定義でない関数からはeffectのperformはないものとして考える *)
        )
      )
  | EffectName (name, local_var_lst, _) -> ((NodeWithId (EffectName (name, local_var_lst, []), [], current_id), [], []),  false , next_id)
  | Empty -> ((LeafWithId current_id, [], []),  false, next_id)
  | Root -> ((NodeWithId (Root, [], current_id), [], []),  false, next_id)
  | Conditions tree_lst -> 
    let (tree_lst, next_id) = add_id_to_efNameTreeList tree_lst next_id in 
    let ref_next_id = ref next_id in
    let ref_used_args = ref false in
    let rec loop2 tree_lst next_id = match tree_lst with
      | [] -> []
      | tree :: tl ->
        let (new_lst , used_args, next_id) = change_efNameTree exp_lst tree local_var_lst next_id now_function in
        ref_next_id := next_id;
        ref_used_args := used_args || !ref_used_args;
        new_lst :: loop2 tl next_id
    in
    let next_id = !ref_next_id in
    let used_args = !ref_used_args in
    let new_tree_lst = loop2 tree_lst next_id in
    let new_tree_lst = List.filter(fun tree -> tree != None) new_tree_lst in
    if new_tree_lst = [] then 
      ((LeafWithId current_id, [], []),  used_args, next_id)
    else
      let new_tree_lst = List.map (fun tree -> Option.get tree) new_tree_lst in
      ((ConditionWithId (new_tree_lst, [], current_id), [], []), used_args, next_id)


(* handler内の解析を行う *)
and change_handler_inside exp_lst handler local_var_lst next_id (now_function: (string * int)) = 
  let used_args = ref false in
  let ref_next_id = ref next_id in 
  let (effect_tree, exception_tree, ret_tree) = handler_lst_analyze handler [] [] Leaf in
  let rec loop tree next_id = match tree with
    | Leaf -> (None, false, next_id)
    | Node (name, lst) -> 
      let ((efName, handler, rest_local_var), used_args, next_id) = change_efName exp_lst name local_var_lst (-1) next_id now_function in
      let (efName, new_used_args, next_id) = 
        if List.length handler = 0 then (efName, false, next_id)
        else 
          let (new_handler, tmp_used_args, next_id) = change_handler_inside exp_lst handler local_var_lst next_id now_function in
          let (efName, next_id) = change_handler efName new_handler local_var_lst next_id in
          (efName, tmp_used_args, next_id)
      in
      let new_tree_lst = List.map(fun tree -> loop tree next_id) lst in
      let new_tree_lst = List.filter(fun (tree, _, _) -> tree != None) new_tree_lst in
      if new_tree_lst = [] && efName = LeafWithId (-1) then 
        (None,false, next_id)
      else
        let used_args = new_used_args || used_args || (List.exists (fun (_, used_args, _) -> used_args) new_tree_lst) in
        let new_tree_lst = List.map (fun (tree, _, _) -> Option.get tree) new_tree_lst in
        (match efName with
          | LeafWithId _ ->
            (Some (Node (Empty, new_tree_lst)), used_args, next_id)
          | _ ->
            let efName = remove_id_from_tree efName in
            (Some (add_efName_tree_list efName new_tree_lst), used_args, next_id)  
        )
  in
  let effect_tree = 
    List.filter_map (fun (name, tree, local_var_lst) -> 
      let result = loop tree next_id in 
      match result with 
        | Some tree, tmp_used_args, next_id -> 
          ref_next_id := next_id;
          used_args := tmp_used_args || !used_args;
          Some (name, tree, local_var_lst)
        | None, _, _ -> None
    ) 
    effect_tree in
  let exception_tree = List.filter_map (fun (name, tree) -> 
    let result = loop tree !ref_next_id in 
    match result with 
      | Some tree, tmp_used_args, next_id -> 
        ref_next_id := next_id;
        used_args := tmp_used_args || !used_args;
        Some (name, tree)
      | None, _, _ -> None
    )  exception_tree in
  let ret_tree = 
    match loop ret_tree !ref_next_id with
    | Some tree, tmp_used_args, next_id -> 
      ref_next_id := next_id;
      used_args := tmp_used_args || !used_args;
      tree
    | None, _, _ -> Leaf
  in
  ([Effc effect_tree; Exnc exception_tree; Retc ret_tree], !used_args, !ref_next_id)

(* 現在はnow_functionにlocalvar listを持たせていないため，部分適用には未対応 *)
and change_efNameTree (exp_lst: (((string * int)* efNameTreeWithId * localVar list) * bool ) list) tree local_var_lst (next_id: int) (now_function: (string * int)) : (efNameTreeWithId option * bool * int)  = 
  let ref_next_id = ref next_id in
  let rec loop tree = match tree with
    | LeafWithId id -> (Some (LeafWithId id), false, !ref_next_id)
    | NodeWithId (name, lst, id) -> 
      Printf.printf "name: %s\n" (efName_to_string name);
      let ((efName, handler, rest_local_var), used_args, next_id) = change_efName exp_lst name local_var_lst id !ref_next_id now_function in
      Printf.printf "efName_base: %s\n" (efNameTreeWithId_to_string efName);
      Printf.printf "handler_base: %s\n" (handlers_to_string handler);
      (* handlerの解析はあとで行う *)
      let (efName, new_used_args, next_id) = 
        if List.length handler = 0 then (efName, false, next_id)
        else 
          let (new_handler, tmp_used_args, next_id) = change_handler_inside exp_lst handler local_var_lst next_id now_function in
          Printf.printf "efName: %s\n" (efNameTreeWithId_to_string efName);
          Printf.printf "pre_handler: %s\n" (handlers_to_string handler);
          Printf.printf "new_handler: %s\n" (handlers_to_string new_handler);
          let (efName, next_id) = change_handler efName new_handler local_var_lst next_id in
          (efName, tmp_used_args, next_id)
      in
      ref_next_id := next_id;
      let new_tree_lst = List.map(fun tree -> loop tree) lst in
      let new_tree_lst = List.filter(fun (tree, _, _) -> tree != None) new_tree_lst in
      if new_tree_lst = [] then 
        (match efName with
          | LeafWithId _ -> (None, false, !ref_next_id)
          | NodeWithId (Root, [], _) -> (None, false, !ref_next_id)
          | _ -> (Some efName, false, !ref_next_id)
        )
      else
        let used_args = new_used_args || used_args || (List.exists (fun (_, used_args, _) -> used_args) new_tree_lst) in
        let new_tree_lst = List.map (fun (tree, _, _) -> Option.get tree) new_tree_lst in
        (match efName with
          | LeafWithId _ -> 
            (Some (NodeWithId (Empty, new_tree_lst, -1)), used_args, !ref_next_id)
          | _ -> 
            (Some (add_efName_tree_with_id_list efName new_tree_lst), used_args, !ref_next_id)

        )
    | RecNodeWithId id -> (Some (RecNodeWithId id), false, !ref_next_id)
    | ConditionWithId (condition_lst, lst, id) -> 
      let new_condition_lst = List.map (fun tree -> loop tree) condition_lst in
      let new_condition_lst = List.filter(fun (tree, _, _) -> tree != None) new_condition_lst in
      let used_args = List.exists (fun (_, used_args, _) -> used_args) new_condition_lst in
      let new_condition_lst = List.map (fun (tree, _, _) -> Option.get tree) new_condition_lst in
      let new_tree_lst = List.map (fun tree -> loop tree) lst in
      let new_tree_lst = List.filter(fun (tree, _, _) -> tree != None) new_tree_lst in
      let used_args = used_args || List.exists (fun (_, used_args, _) -> used_args) new_tree_lst in
      let new_tree_lst = List.map (fun (tree, _, _) -> Option.get tree) new_tree_lst in
      (Some (ConditionWithId (new_condition_lst, new_tree_lst, id)), used_args, !ref_next_id)
  in
  loop tree


let rec function_call_to_mid_call_flow (exp_lst: (((string * int)* efNameTreeWithId * localVar list) * bool) list) (lst: ((string * int) * (efNameTreeWithId * int) * localVar list)list) = match lst with
  | [] -> []
  | (name,(tree , next_id), local_var_lst) :: tl -> 
    (* 再帰する際に戻る位置と *)
    let now_function = (fst name, next_id -1) in
    let new_exp_lst = (name, change_efNameTree exp_lst tree local_var_lst next_id now_function) in
    match new_exp_lst with
    | (_, (None,_, _)) -> 
      function_call_to_mid_call_flow exp_lst tl
    | (name, (Some tree, used_args, _)) ->
        (name, tree, local_var_lst) ::function_call_to_mid_call_flow (((name, tree, local_var_lst), used_args)::exp_lst) tl