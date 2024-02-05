open Effect_analyzer_core
open Function_call_util
open Efname_formatter

let rec handler_lst_analyze lst effect_lst exception_lst ret_lst = match lst with
  | [] -> (effect_lst, exception_lst, ret_lst)
  | hd :: tl -> (match hd with
    | Effc effect_lst -> Printf.printf "eff\n"; handler_lst_analyze tl effect_lst exception_lst ret_lst
    | Exnc exception_lst -> handler_lst_analyze tl effect_lst exception_lst ret_lst
    | Retc ret_lst -> handler_lst_analyze tl effect_lst exception_lst ret_lst)

let any_exist_wildcard effect_lst = 
  let result = List.find_opt (fun (name, _) -> name = "_") effect_lst in
  match result with
  | Some (_, ef_lst) -> ef_lst
  | None -> []

(* continueしたときにhandlerが定義されていた場合は現在動作不能 *)
(* 今の実装では逆 *)
let rec efName_lst_opt_continue lst = match lst with
  | [] -> None
  | hd :: tl -> (match hd with
    | FunctionName (name, handler) -> 
      if name = "continue" then Some tl
      else efName_lst_opt_continue tl
    | _ -> efName_lst_opt_continue tl)

(* not_confirm_lst, confirm_lstの組みを返す *)
let efName_lst_opt_continue_append (ef_lst: efName list list) = 
  (* confirm_lstはcontinueされなかった時．*)
  (* not_confirm_lstはcontinueされた時．*)
  let rec loop lst (not_confirm_lst: efName list list) (confirm_lst: efName list list) = match lst with
    | [] -> (not_confirm_lst, confirm_lst)
    | hd :: tl -> (match hd with
      | [] -> loop tl not_confirm_lst confirm_lst
      | _ -> 
        let result = efName_lst_opt_continue hd in
        (match result with
        | Some lst -> loop tl (lst :: not_confirm_lst) confirm_lst
        | None -> loop tl not_confirm_lst (hd :: confirm_lst)))
  in
  loop ef_lst [] []

let analyze_handler (lst: efName list) (handler: efNameOfHandler list) = 
  Printf.printf "efNamelst: %s\n" (efName_list_to_string lst);
  Printf.printf "handler: %d\n" (List.length handler);
  let (effect_lst, exception_lst, ret_lst) = handler_lst_analyze handler [] [] [] in
  (* confirm_lstはcontinueされずに列が確定した処理のことを指す *)
  let rec loop lst (confirm_lst: efName list list) = match lst with
    | [] -> ([], confirm_lst)
    | hd :: tl -> (match hd with
      | EffectName name -> 
        Printf.printf "name: %s\n" name;
        Printf.printf "effect_lst: %d\n" (List.length effect_lst);
        let result = List.find_opt (fun (tmp_name, _) -> name = tmp_name) effect_lst in
        let (not_confirm_lst, new_confirm_lst) = 
          (match result with
          | Some (_, ef_lst) -> 
            let tmp_ef_lst = perform_lst_append ef_lst [[hd]] in
            efName_lst_opt_continue_append tmp_ef_lst
          | None -> 
            let tmp_ef_lst = perform_lst_append (any_exist_wildcard effect_lst) [[hd]] in 
            efName_lst_opt_continue_append tmp_ef_lst
          ) in 
        let (tl_lst, new_confirm_lst) = loop tl new_confirm_lst in
        (perform_lst_append not_confirm_lst tl_lst), new_confirm_lst
      | _ ->
        let (not_confirm_lst, new_confirm_lst) = loop tl confirm_lst in 
        (perform_lst_append [[hd]] not_confirm_lst), new_confirm_lst
    )
  in
  let (not_confirm_lst, confirm_lst) = loop lst [] in 
  let not_confirm_lst = 
    if List.length ret_lst = 0 then not_confirm_lst
    else perform_lst_append ret_lst not_confirm_lst 
  in
  confirm_lst @ not_confirm_lst

let rec analyze_efName_lst_with_handler (lst: efName list list) handler =  match lst with
  | [] -> []
  | efName_lst :: tl -> 
    let tmp_lst = analyze_handler efName_lst handler in
    perform_lst_append tmp_lst (analyze_efName_lst_with_handler tl handler)
  

(* handlerの解析はまだできていない *)
let analyze_efName (exp_lst: (string * efName list list) list) efName = match efName with
  | FunctionName (name, lst) -> 
    if name = "continue" then (* continueの場合は残しておく *)
      ([[efName]], lst)
    else
      let result = List.find_opt (fun (n, _) -> n = name) exp_lst in
      (match result with
      | Some (_, tmp_lst) -> (tmp_lst, lst)
      | None -> ([], [])) (* ユーザ定義でない関数からはeffectのperformはないものとして考える *)
  | EffectName name -> ([[EffectName name]], [])

(* handler内の解析を行う *)
let rec analyze_handler_inside exp_lst handler = 
  let (effect_lst, exception_lst, ret_lst) = handler_lst_analyze handler [] [] [] in
  (* efName listを受け取って中身を解析しておく *)
  let rec efName_list_loop lst = match lst with
    | [] -> []
    | hd :: tl -> 
        let (tmp_lst, handler) = analyze_efName exp_lst hd in
        let tmp_lst = 
          if List.length handler = 0 then tmp_lst
          else 
            let new_handler = analyze_handler_inside exp_lst handler in
            analyze_efName_lst_with_handler tmp_lst new_handler
        in
        perform_lst_append tmp_lst (efName_list_loop tl)
  in
  (* efName list　listを受け取って中身を解析しておく *)
  let rec efName_list_list_loop lst = match lst with
    | [] -> []
    | hd :: tl -> 
      let tmp_lst = efName_list_loop hd in
      perform_lst_append tmp_lst (efName_list_list_loop tl)
  in
  let effect_lst = List.map (fun (name, tmp_lst) -> (name, efName_list_list_loop tmp_lst)) effect_lst in
  let exception_lst = List.map (fun (name, tmp_lst) -> (name, efName_list_list_loop tmp_lst)) exception_lst in
  let ret_lst = efName_list_list_loop ret_lst in
  [Effc effect_lst; Exnc exception_lst; Retc ret_lst]

let rec analyze_efName_lst exp_lst lst = match lst with
  | [] -> []
  | efName :: tl -> 
    let (tmp_lst, handler) = analyze_efName exp_lst efName in
    let tmp_lst = 
      if List.length handler = 0 then tmp_lst
      else 
        let new_handler = analyze_handler_inside exp_lst handler in
        analyze_efName_lst_with_handler tmp_lst new_handler
    in
    perform_lst_append tmp_lst (analyze_efName_lst exp_lst tl)

let rec analyze_eflst exp_lst lst = match lst with
  | [] -> []
  | efName_lst :: tl -> 
    let tmp_lst = analyze_efName_lst exp_lst efName_lst in
    perform_lst_append tmp_lst (analyze_eflst exp_lst tl)

let rec analyze_function_call exp_lst (lst: (string * efName list list) list) = match lst with
  | [] -> []
  | (name, lst) :: tl -> 
    Printf.printf "function_name: %s\n" name;
    let new_exp_lst = (name, analyze_eflst exp_lst lst) in
    (* let (aa, bb) = new_exp_lst in
    Printf.printf "function_name: %s\n" name;
    Printf.printf "new_exp_lst: %s\n" (List.fold_left (fun a b -> a ^ efName_list_to_string b) "" bb); *)
    new_exp_lst ::analyze_function_call (new_exp_lst::exp_lst) tl
