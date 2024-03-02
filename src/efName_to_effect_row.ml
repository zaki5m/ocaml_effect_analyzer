open Effect_analyzer_core
open Function_call_util
open Efname_formatter


let analyze_handler (tree: efNameTree) (handler: efNameOfHandler list) = 
  let (effect_lst, exception_lst, ret_tree) = handler_lst_analyze handler [] [] Leaf in
  let rec loop (tree: efNameTree) = match tree with
    | Leaf -> Leaf
    | Node (efName, []) -> Node (efName, [ret_tree])
    | Node (efName, tmp_tree_lst) -> (match efName with
      | EffectName (name, _, _) -> 
        let result = List.find_opt (fun (tmp_name, _, _) -> name = tmp_name) effect_lst in
        (match result with
        | Some (_, tmp_tree, _) ->  
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
  

(* handlerの解析はまだできていない *)
let analyze_efName (exp_lst: ((string * int) * efNameTree * localVar list) list) efName local_var_lst  : ((efNameTree * efNameOfHandler list)) = match efName with
  | FunctionName (name, lst, current_eval, arg_lst) -> 
    Printf.printf "------------😀function_name: %s\n" name;
    if name = "continue" then (* continueの場合は残しておく *)
      (Node (efName, []), lst)
    else
      let result = find_local_var name local_var_lst in
      (match result with
      | Some (LocalVar (_, _, _)) -> (* 要改善 *)
        (Node (efName, []), lst)
      | Some (ArgsVar (_, _)) ->
        let result = find_local_var name current_eval in
        (match result with
        | Some (LocalVar (_,_,new_tree))-> (* 関数の部分適用はないものと考える *)
          Printf.printf "new_tree1: %s\n" (efNameTree_to_string new_tree);
          (new_tree, lst)
        | _ ->
          Printf.printf "name: %s\n" name;
          (Node (efName, []), lst))
      | _ -> 
        let result = find_local_var name current_eval in
        (match result with
        | Some (LocalVar (_,_,new_tree))-> (* 関数の部分適用はないものと考える *)
          Printf.printf "new_tree2: %s\n" (efNameTree_to_string new_tree);
          (new_tree, lst)
        | _ ->
          Printf.printf "🥰name: %s\n" name;
          (Leaf, []))) (* ユーザ定義でない関数からはeffectのperformはないものとして考える *)
  | EffectName (name, _, _) -> (Node (EffectName (name, [], []), []), [])
  | Empty -> (Leaf, [])

(* handler内の解析を行う *)
let rec analyze_handler_inside exp_lst handler local_var_lst = 
  let (effect_tree, exception_tree, ret_tree) = handler_lst_analyze handler [] [] Leaf in
  let rec loop tree = match tree with
    | Leaf -> None
    | Node (name, lst) ->
      (* lstが継続に当たる *) 
      let (efName, handler) = analyze_efName exp_lst name local_var_lst in
      let efName = 
        if List.length handler = 0 then efName
        else 
          let new_handler = analyze_handler_inside exp_lst handler local_var_lst in
          analyze_handler efName new_handler
      in
      let new_tree_lst = List.filter_map(fun tree -> loop tree) lst in
      if new_tree_lst = [] && efName = Leaf then 
        None
      else
        if efName = Leaf then
          Some (Node (Empty, new_tree_lst))
        else
          Some (add_efName_tree_list efName new_tree_lst)    
  in
  let effect_tree = 
    List.filter_map (fun (name, tree, local_var_lst) -> 
      let result = loop tree in 
      match result with 
        | Some tree -> Some (name, tree, local_var_lst)
        | None -> None
    ) 
    effect_tree in
  let exception_tree = List.filter_map (fun (name, tree) -> 
    let result = loop tree in 
    match result with 
      | Some tree -> Some (name, tree)
      | None -> None
    )  exception_tree in
  let ret_tree = 
    match loop ret_tree with
    | Some tree -> tree
    | None -> Leaf 
  in
  [Effc effect_tree; Exnc exception_tree; Retc ret_tree]

let rec analyze_efNameTree (exp_lst: ((string * int)* efNameTree * localVar list ) list) tree local_var_lst : efNameTree option  = match tree with
  | Leaf -> None
  | Node (name, lst) -> 
    Printf.printf "-------😡-------\n";
    Printf.printf "%s\n" (efNameTree_to_string tree);
    Printf.printf "-------😡-------\n";
    let (efName, handler) = analyze_efName exp_lst name local_var_lst in
    Printf.printf  "handler len: %d\n"  (List.length handler);
    let efName = 
      if List.length handler = 0 then efName
      else 
        let new_handler = analyze_handler_inside exp_lst handler local_var_lst in
        analyze_handler efName new_handler
    in
    let new_tree_lst = List.filter_map(fun tree -> analyze_efNameTree exp_lst tree local_var_lst) lst in
    if new_tree_lst = [] && efName = Leaf then 
      None
    else
      if efName = Leaf then
        Some (Node (Empty, new_tree_lst))
      else
        Some (add_efName_tree_list efName new_tree_lst)
    


let rec analyze_function_call (exp_lst: ((string * int)* efNameTree * localVar list) list) (lst: ((string * int) * efNameTree * localVar list)list) = match lst with
  | [] -> []
  | (name,tree, local_var_lst) :: tl -> 
    Printf.printf "function_name: %s\n" (fst name);
    let new_exp_lst = (name, analyze_efNameTree exp_lst tree local_var_lst) in
    match new_exp_lst with
    | (_, None) -> analyze_function_call exp_lst tl
    | (name, Some tree) ->
        (name, tree) ::analyze_function_call ((name, tree, local_var_lst)::exp_lst) tl
