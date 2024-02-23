open Printf
open Effect_analyzer_core

let rec handler_to_string (handler: efNameOfHandler) = match handler with
  | Effc lst -> "Effc: " ^ List.fold_left (fun str (name, tmp_lst) -> str ^ efNameOfHandler_to_string name tmp_lst) "" lst
  | Exnc lst -> "Exnc: " ^ List.fold_left (fun str (name, tmp_lst) -> str ^ efNameOfHandler_to_string name tmp_lst) "" lst
  | Retc lst -> "Retc: " ^ efNameTree_to_string lst
and efNameOfHandler_to_string (pattern: string) (efName_tree: efNameTree) :string =
  "pattern: { " ^ pattern ^ " } \n" ^ efNameTree_to_string efName_tree
and handlers_to_string (handlers: efNameOfHandler list) =
  match handlers with
  | [] -> "]"
  | handler::handlers -> (handler_to_string handler) ^ " ] , [ " ^ (handlers_to_string handlers)
and efNameTree_to_string (efName_tree: efNameTree) :string =
  match efName_tree with
  | Leaf -> ""
  | Node (efName, efName_tree) -> efName_to_string efName ^ " -> (" ^ (List.fold_left (fun before a -> before ^ efNameTree_to_string a) "" efName_tree) ^ ")"
and efName_to_string (efName: efName) :string =
  match efName with
  | FunctionName (name,handler, local_var_lst)  -> "(" ^ name ^ " , [ " ^ (handlers_to_string handler) ^ " || " ^ List.fold_left (fun str a -> str ^ local_var_to_string a) "" local_var_lst ^ ")"
  | EffectName name -> name
  | Empty -> "Empty"
and efName_list_to_string efName_list =
  match efName_list with
  | [] -> "\n"
  | efName::efName_list -> (efName_list_to_string efName_list) ^ " -> " ^ (efName_to_string efName)
and local_var_to_string (local_var: localVar) =
  match local_var with
  | LocalVar (name, local_var_lst, _) -> "function name: " ^ name ^ "  " ^ List.fold_left (fun str a -> str ^ local_var_to_string a) "" local_var_lst
  | ArgsVar (name, _) -> "arg: " ^ name ^ " "
  | EmptyVar -> "EmptyVar"