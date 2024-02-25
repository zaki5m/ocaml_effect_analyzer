open Effect_analyzer_core
open Function_call_util
open Efname_formatter


let change_handler (tree: efNameTree) (handler: efNameOfHandler list) local_var_lst = 
  let (effect_lst, exception_lst, ret_tree) = handler_lst_analyze handler [] [] Leaf in
  let rec loop (tree: efNameTree) = match tree with
    | Leaf -> Leaf
    | Node (efName, []) -> Node (efName, [ret_tree])
    | Node (efName, tmp_tree_lst) -> (match efName with
      | EffectName name -> 
        let result = List.find_opt (fun (tmp_name, _) -> name = tmp_name) effect_lst in
        (match result with
        | Some (_, tmp_tree) -> 
          let tmp_tree = analyze_handler_serch_continue tmp_tree in
          (match tmp_tree with
          | Some tree ->
            let new_tree = (Node (efName, [tree])) in
            add_efName_tree_list_to_leaf new_tree (List.map (fun tree ->  loop tree) tmp_tree_lst)
          | None -> 
            let new_tree = (Node (efName, [])) in
            add_efName_tree_list_to_leaf new_tree (List.map (fun tree ->  loop tree) tmp_tree_lst)
          )
        | None -> 
          let tmp_tree = any_exist_wildcard effect_lst in
          let tmp_tree = analyze_handler_serch_continue tmp_tree in
          (match tmp_tree with
          | Some tree ->
            let new_tree = (Node (efName, [tree])) in
            add_efName_tree_list_to_leaf new_tree (List.map (fun tree ->  loop tree) tmp_tree_lst)
          | None -> 
            let new_tree = (Node (efName, [])) in
            add_efName_tree_list_to_leaf new_tree (List.map (fun tree ->  loop tree) tmp_tree_lst)
          )
        )
      | _ -> Node (efName, (List.map (fun tree ->  loop tree) tmp_tree_lst))
    )
  in
  loop tree  

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
let rec change_tree_to_concrete (tree: efNameTree) (arg_lst: (localVar * localVar) list) = match tree with
  | Leaf -> Leaf
  | Node (efName, lst) -> match efName with
    | FunctionName (name, handler, current_eval, tmp_arg_lst) -> 
      let new_current_eval = change_env_to_concrete current_eval arg_lst in
      Node (FunctionName (name, handler, new_current_eval, tmp_arg_lst), List.map (fun tree -> change_tree_to_concrete tree arg_lst) lst)
    | EffectName name -> Node (EffectName name, List.map (fun tree -> change_tree_to_concrete tree arg_lst) lst)
    | Empty -> Node (Empty, List.map (fun tree -> change_tree_to_concrete tree arg_lst) lst)
    

(* treeをlocal_varが正しく入った状態のtgreeに変換する関数 *)


(* (tree, handler, この関数の引数の実際に入ってくる値に置き換えたもの), 現在のscope内の環境 *)
let change_efName (exp_lst: (((string * int) * efNameTree * localVar list) * bool) list) efName (local_var_lst: localVar list)  : ((efNameTree * efNameOfHandler list * localVar list)  * bool) = match efName with
  | FunctionName (name, lst, current_eval, arg_lst) -> 
    if name = "continue" then (* continueの場合は残しておく *)
      ((Node (efName, []), lst, []), false) (* この部分は要改善 *)
    else
      let result = List.find_opt (fun (((n,_), _, _), _) -> n = name) exp_lst in
      (match result with
      | Some ((_, tmp_lst, tmp_local_var_lst), used_args) -> 
        if used_args then
          let (tmp_arg_lst, rest_local_var) = change_arg_to_concrete exp_lst arg_lst tmp_local_var_lst current_eval local_var_lst in (* 部分適用を考える際にはこの部分は必須 *)
          (* ここでtreeの環境の変換を行う *)
          Printf.printf "tmp_arg_lst: %d\n" (List.length tmp_arg_lst);
          Printf.printf "efname: %s\n" (efNameTree_to_string tmp_lst);
          List.iter (fun (a, b) -> Printf.printf "a: %s, b: %s\n" (local_var_to_string a) (local_var_to_string b)) tmp_arg_lst;
          let tmp_lst = change_tree_to_concrete tmp_lst tmp_arg_lst in
          Printf.printf "new_efname: %s\n" (efNameTree_to_string tmp_lst);
          ((tmp_lst, lst, rest_local_var), false)
        else
          let (_, rest_local_var) = change_arg_to_concrete exp_lst arg_lst tmp_local_var_lst current_eval local_var_lst in 
          ((tmp_lst, lst, rest_local_var), false)
      | None -> 
        let result = find_local_var name local_var_lst in
        (match result with
        | Some (LocalVar (_, _, _)) -> (* 要改善 *)
          ((Node (efName, []), lst, local_var_lst), false)
        | Some (ArgsVar (_, _)) ->
          ((Node (efName, []), lst, []), true)
        | _ -> 
          (Leaf, [], []), false) (* ユーザ定義でない関数からはeffectのperformはないものとして考える *)
      )
  | EffectName name -> ((Node (EffectName name, []), [], []),  false)
  | Empty -> ((Leaf, [], []),  false)

(* handler内の解析を行う *)
let rec change_handler_inside exp_lst handler local_var_lst  = 
  let used_args = ref false in
  let (effect_tree, exception_tree, ret_tree) = handler_lst_analyze handler [] [] Leaf in
  let rec loop tree = match tree with
    | Leaf -> (None, false)
    | Node (name, lst) -> 
      let ((efName, handler, rest_local_var), used_args) = change_efName exp_lst name local_var_lst in
      let (efName, new_used_args) = 
        if List.length handler = 0 then (efName, false)
        else 
          let (new_handler, tmp_used_args) = change_handler_inside exp_lst handler local_var_lst in
          (change_handler efName new_handler local_var_lst, tmp_used_args)
      in
      let new_tree_lst = List.map(fun tree -> loop tree) lst in
      let new_tree_lst = List.filter(fun (tree, _) -> tree != None) new_tree_lst in
      if new_tree_lst = [] && efName = Leaf then 
        (None,false)
      else
        let used_args = new_used_args || used_args || (List.exists (fun (_, used_args) -> used_args) new_tree_lst) in
        let new_tree_lst = List.map (fun (tree, _) -> Option.get tree) new_tree_lst in
        if efName = Leaf then
          (Some (Node (Empty, new_tree_lst)), used_args)
        else
          (Some (add_efName_tree_list efName new_tree_lst), used_args)  
  in
  let effect_tree = 
    List.filter_map (fun (name, tree) -> 
      let result = loop tree in 
      match result with 
        | Some tree, tmp_used_args -> 
          used_args := tmp_used_args || !used_args;
          Some (name, tree)
        | None, _ -> None
    ) 
    effect_tree in
  let exception_tree = List.filter_map (fun (name, tree) -> 
    let result = loop tree in 
    match result with 
      | Some tree, tmp_used_args -> 
        used_args := tmp_used_args || !used_args;
        Some (name, tree)
      | None, _ -> None
    )  exception_tree in
  let ret_tree = 
    match loop ret_tree with
    | Some tree, tmp_used_args -> 
      used_args := tmp_used_args || !used_args;
      tree
    | None, _ -> Leaf 
  in
  ([Effc effect_tree; Exnc exception_tree; Retc ret_tree], !used_args)

let rec change_efNameTree (exp_lst: (((string * int)* efNameTree * localVar list) * bool ) list) tree local_var_lst : (efNameTree option * bool)  = match tree with
  | Leaf -> (None, false)
  | Node (name, lst) -> 
    let ((efName, handler, rest_local_var), used_args) = change_efName exp_lst name local_var_lst in
    (* handlerの解析はあとで行う *)
    let (efName, new_used_args) = 
      if List.length handler = 0 then (efName, false)
      else 
        let (new_handler, tmp_used_args) = change_handler_inside exp_lst handler local_var_lst in
        Printf.printf "efName: %s\n" (efNameTree_to_string efName);
        Printf.printf "pre_handler: %s\n" (handlers_to_string handler);
        Printf.printf "new_handler: %s\n" (handlers_to_string new_handler);
        (change_handler efName new_handler local_var_lst, tmp_used_args)
    in
    let new_tree_lst = List.map(fun tree -> change_efNameTree exp_lst tree local_var_lst) lst in
    let new_tree_lst = List.filter(fun (tree, _) -> tree != None) new_tree_lst in
    if new_tree_lst = [] && efName = Leaf then 
      (None, false)
    else
      let used_args = new_used_args || used_args || (List.exists (fun (_, used_args) -> used_args) new_tree_lst) in
      let new_tree_lst = List.map (fun (tree, _) -> Option.get tree) new_tree_lst in
      if efName = Leaf then
        (Some (Node (Empty, new_tree_lst)), used_args)
      else
        (Some (add_efName_tree_list efName new_tree_lst), used_args)
    


let rec function_call_to_mid_call_flow (exp_lst: (((string * int)* efNameTree * localVar list) * bool) list) (lst: ((string * int) * efNameTree * localVar list)list) = match lst with
  | [] -> []
  | (name,tree, local_var_lst) :: tl -> 
    Printf.printf "function_name: %s\n" (fst name);
    let new_exp_lst: ((string * int) * (efNameTree option * bool)) = (name, change_efNameTree exp_lst tree local_var_lst) in
    match new_exp_lst with
    | (_, (None,_)) -> 
      function_call_to_mid_call_flow exp_lst tl
    | (name, (Some tree, used_args)) ->
        (name, tree, local_var_lst) ::function_call_to_mid_call_flow (((name, tree, local_var_lst), used_args)::exp_lst) tl