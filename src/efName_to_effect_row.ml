open Effect_analyzer_core
open Function_call_util
open Efname_formatter

let rec handler_lst_analyze lst effect_lst exception_lst ret_lst = match lst with
  | [] -> (effect_lst, exception_lst, ret_lst)
  | hd :: tl -> (match hd with
    | Effc effect_lst -> handler_lst_analyze tl effect_lst exception_lst ret_lst
    | Exnc exception_lst -> handler_lst_analyze tl effect_lst exception_lst ret_lst
    | Retc ret_lst -> handler_lst_analyze tl effect_lst exception_lst ret_lst)

let any_exist_wildcard effect_lst = 
  let result = List.find_opt (fun (name, _) -> name = "_") effect_lst in
  match result with
  | Some (_, ef_lst) -> ef_lst
  | None -> Leaf

let rec analyze_handler_serch_continue tree = match tree with
  | Leaf -> None
  | Node (efName, lst) -> (match efName with 
    | FunctionName (name, tmp_handler, _) ->
      if name = "continue" then 
        Some Leaf
      else 
        Some (Node (efName, (List.filter_map (fun tree -> analyze_handler_serch_continue tree) lst)))
    | _ -> Some (Node (efName, (List.filter_map (fun tree -> analyze_handler_serch_continue tree) lst)))
  )


let analyze_handler (tree: efNameTree) (handler: efNameOfHandler list) = 
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
  

(* handlerの解析はまだできていない *)
let analyze_efName (exp_lst: ((string * int) * efNameTree) list) efName: (efNameTree * efNameOfHandler list) = match efName with
  | FunctionName (name, lst, _) -> 
    if name = "continue" then (* continueの場合は残しておく *)
      (Node (efName, []), lst)
    else
      let result = List.find_opt (fun ((n,_), _) -> n = name) exp_lst in
      (match result with
      | Some (_, tmp_lst) -> (tmp_lst, lst)
      | None -> (Leaf, [])) (* ユーザ定義でない関数からはeffectのperformはないものとして考える *)
  | EffectName name -> (Node (EffectName name, []), [])
  | Empty -> (Leaf, [])

(* handler内の解析を行う *)
let rec analyze_handler_inside exp_lst handler = 
  let (effect_tree, exception_tree, ret_tree) = handler_lst_analyze handler [] [] Leaf in
  let rec loop tree = match tree with
    | Leaf -> None
    | Node (name, lst) -> 
      let (efName, handler) = analyze_efName exp_lst name in
      let efName = 
        if List.length handler = 0 then efName
        else 
          let new_handler = analyze_handler_inside exp_lst handler in
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
    List.filter_map (fun (name, tree) -> 
      let result = loop tree in 
      match result with 
        | Some tree -> Some (name, tree)
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

let rec analyze_efNameTree (exp_lst: ((string * int)* efNameTree) list) tree = match tree with
  | Leaf -> None
  | Node (name, lst) -> 
    let (efName, handler) = analyze_efName exp_lst name in
    let efName = 
      if List.length handler = 0 then efName
      else 
        let new_handler = analyze_handler_inside exp_lst handler in
        analyze_handler efName new_handler
    in
    let new_tree_lst = List.filter_map(fun tree -> analyze_efNameTree exp_lst tree) lst in
    if new_tree_lst = [] && efName = Leaf then 
      None
    else
      if efName = Leaf then
        Some (Node (Empty, new_tree_lst))
      else
        Some (add_efName_tree_list efName new_tree_lst)
    


let rec analyze_function_call (exp_lst: ((string * int)* efNameTree) list) (lst: ((string * int) * efNameTree)list) = match lst with
  | [] -> []
  | (name,tree) :: tl -> 
    Printf.printf "function_name: %s\n" (fst name);
    let new_exp_lst = (name, analyze_efNameTree exp_lst tree) in
    match new_exp_lst with
    | (_, None) -> analyze_function_call exp_lst tl
    | (name, Some tree) ->
        (name, tree) ::analyze_function_call ((name, tree)::exp_lst) tl
