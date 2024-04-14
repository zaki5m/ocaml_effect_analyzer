open Effect_analyzer_core
open Function_call_util
open Efname_formatter
open Effect_row_to_graph


let analyze_handler (tree: efNameTreeWithId) (handler: efNameOfHandler list) next_id = 
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
        let result = List.find_opt (fun (tmp_name, _, _) -> name = tmp_name) effect_lst in
        (match result with
        | Some (_, tmp_tree, _) ->  
          let continuation_tree = List.map (fun tree ->  loop tree) tmp_tree_lst in 
          let (next_tree, next_id) = add_id_to_efNameTree tmp_tree !ref_next_id in
          ref_next_id := next_id;
          let new_tree = NodeWithId (efName, [next_tree], id) in
          let tmp_tree = analyze_handler_serch_continue new_tree continuation_tree in
          tmp_tree
        | None -> 
          let tmp_tree = any_exist_wildcard effect_lst in
          let continuation_tree = List.map (fun tree ->  loop tree) tmp_tree_lst in 
          let (next_tree, next_id) = add_id_to_efNameTree tmp_tree !ref_next_id in
          ref_next_id := next_id;
          let new_tree = NodeWithId (efName, [next_tree], id) in
          let tmp_tree = analyze_handler_serch_continue new_tree continuation_tree in
          tmp_tree
        )
      | _ -> NodeWithId (efName, (List.map (fun tree ->  loop tree) tmp_tree_lst), id)
    )
    | _ -> failwith "error"
  in
  (loop tree, !ref_next_id)
  

(* handlerの解析はまだできていない *)
let rec analyze_efName (exp_lst: ((string * int) * efNameTreeWithId * localVar list) list) efName local_var_lst current_id next_id  : ((efNameTreeWithId * efNameOfHandler list * int)) = match efName with
  | FunctionName (name, lst, current_eval, arg_lst) -> 
    Printf.printf "------------😀function_name: %s\n" name;
    if name = "continue" then (* continueの場合は残しておく *)
      (NodeWithId (efName, [], current_id), lst, next_id)
    else
      let result = find_local_var name local_var_lst in
      (match result with
      | Some (LocalVar (_, _, _)) -> (* 要改善 *)
        (NodeWithId (efName, [], current_id), lst, next_id)
      | Some (ArgsVar (_, _)) ->
        let result = find_local_var name current_eval in
        (match result with
        | Some (LocalVar (_,_,new_tree))-> (* 関数の部分適用はないものと考える *)
          Printf.printf "new_tree1: %s\n" (efNameTree_to_string new_tree);
          let (new_tree, next_id) = add_id_to_efNameTree new_tree next_id in
          (new_tree, lst, next_id)
        | _ ->
          Printf.printf "name: %s\n" name;
          (NodeWithId (efName, [], current_id), lst, next_id))
      | _ -> 
        let result = find_local_var name current_eval in
        (match result with
        | Some (LocalVar (_,_,new_tree))-> (* 関数の部分適用はないものと考える *)
          Printf.printf "new_tree2: %s\n" (efNameTree_to_string new_tree);
          let (new_tree, next_id) = add_id_to_efNameTree new_tree next_id in
          (new_tree, lst, next_id)
        | _ ->
          Printf.printf "🥰name: %s\n" name;
          (LeafWithId current_id, [], next_id))) (* ユーザ定義でない関数からはeffectのperformはないものとして考える *)
  | EffectName (name, _, _) -> (NodeWithId (EffectName (name, [], []), [], current_id), [], next_id)
  | Empty -> (LeafWithId current_id, [], next_id)
  | Root -> NodeWithId (Root, [], current_id), [], next_id
  | Conditions tree_lst -> 
    let (tree_lst, next_id) = add_id_to_efNameTreeList tree_lst next_id in 
    let ref_next_id = ref next_id in
    let rec loop2 tree_lst next_id = match tree_lst with
      | [] -> []
      | tree :: tl ->
        let (new_lst , next_id) = analyze_efNameTree exp_lst tree local_var_lst next_id in
        ref_next_id := next_id;
        new_lst :: loop2 tl next_id
    in
    let next_id = !ref_next_id in
    let new_lst = loop2 tree_lst next_id in
    let new_tree_lst = List.filter(fun tree -> tree != None) new_lst in
    if new_tree_lst = [] then 
      (LeafWithId current_id, [], next_id)
    else
      let new_tree_lst = List.map (fun tree -> Option.get tree) new_tree_lst in
      (ConditionWithId (new_tree_lst, [], current_id), [], next_id)


(* handler内の解析を行う *)
and analyze_handler_inside exp_lst handler local_var_lst next_id = 
  let ref_next_id = ref next_id in
  let (effect_tree, exception_tree, ret_tree) = handler_lst_analyze handler [] [] Leaf in
  let rec loop tree = match tree with
    | Leaf -> None
    | Node (name, lst) ->
      (* lstが継続に当たる *) 
      let (efName, handler, next_id) = analyze_efName exp_lst name local_var_lst (-1) !ref_next_id in
      let(efName, next_id) = 
        if List.length handler = 0 then (efName, next_id)
        else 
          let (new_handler, next_id) = analyze_handler_inside exp_lst handler local_var_lst next_id in
          analyze_handler efName new_handler next_id
      in
      ref_next_id := next_id;
      let new_tree_lst = List.filter_map(fun tree -> loop tree) lst in
      if new_tree_lst = [] && efName = LeafWithId (-1) then 
        None
      else
        (match efName with
          | LeafWithId _ -> Some (Node (Empty, new_tree_lst))
          | _ -> 
            let efName = remove_id_from_tree efName in
            Some (add_efName_tree_list efName new_tree_lst)
        ) 
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
  ([Effc effect_tree; Exnc exception_tree; Retc ret_tree], !ref_next_id)

and analyze_efNameTree (exp_lst: ((string * int)* efNameTreeWithId * localVar list ) list) tree local_var_lst next_id : (efNameTreeWithId option * int)  = 
  let ref_next_id = ref next_id in
  let rec loop tree = match tree with
    | LeafWithId id -> (Some (LeafWithId id))
    | NodeWithId (name, lst, id) -> 
      Printf.printf "-------😡-------\n";
      Printf.printf "%s\n" (efNameTreeWithId_to_string tree);
      Printf.printf "-------😡-------\n";
      let (efName, handler, next_id) = analyze_efName exp_lst name local_var_lst id !ref_next_id in
      Printf.printf  "handler len: %d\n"  (List.length handler);
      let (efName, next_id) = 
        if List.length handler = 0 then (efName, next_id)
        else 
          let (new_handler, next_id) = analyze_handler_inside exp_lst handler local_var_lst next_id in
          analyze_handler efName new_handler next_id
      in
      ref_next_id := next_id;
      let new_tree_lst = List.filter_map(fun tree -> loop tree) lst in
      if new_tree_lst = [] && efName = LeafWithId (-1) then 
        None
      else
        (match efName with
          | LeafWithId _ -> Some (NodeWithId (Empty, new_tree_lst, id))
          | _ -> 
            Some (add_efName_tree_with_id_list efName new_tree_lst)
        )
    | RecNodeWithId id -> (Some (RecNodeWithId id))
    | ConditionWithId (condition_lst, lst, id) -> 
      let new_condition_lst = List.map (fun tree -> loop tree) condition_lst in
      let new_condition_lst = List.filter(fun tree -> tree != None) new_condition_lst in
      let new_condition_lst = List.map (fun tree -> Option.get tree) new_condition_lst in
      let new_tree_lst = List.map (fun tree -> loop tree) lst in
      let new_tree_lst = List.filter(fun tree -> tree != None) new_tree_lst in
      let new_tree_lst = List.map (fun tree -> Option.get tree) new_tree_lst in
      (Some (ConditionWithId (new_condition_lst, new_tree_lst, id)))
  in
  let result = loop tree in
  (result, !ref_next_id)
    


let rec analyze_function_call (exp_lst: ((string * int)* efNameTreeWithId * localVar list) list) (lst: ((string * int) * efNameTreeWithId * localVar list)list) = match lst with
  | [] -> []
  | (name,tree, local_var_lst) :: tl -> 
    Printf.printf "function_name: %s\n" (fst name);
    let next_id = search_next_id tree in
    let new_exp_lst = (name, analyze_efNameTree exp_lst tree local_var_lst next_id) in
    match new_exp_lst with
    | (_, (None, _)) -> analyze_function_call exp_lst tl
    | (name, (Some tree, _)) ->
        (name, tree) ::analyze_function_call ((name, tree, local_var_lst)::exp_lst) tl
