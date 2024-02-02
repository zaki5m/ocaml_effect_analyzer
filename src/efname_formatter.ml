open Printf
open Effect_analyzer_core

let rec handler_to_string (handler: efNameOfHandler) = match handler with
  | Effc lst -> "Effc: " ^ List.fold_left (fun str (name, tmp_lst) -> str ^ efNameOfHandler_to_string name tmp_lst) "" lst
  | Exnc lst -> "Exnc: " ^ List.fold_left (fun str (name, tmp_lst) -> str ^ efNameOfHandler_to_string name tmp_lst) "" lst
  | Retc lst -> "Retc: " ^ List.fold_left (fun str efNamelst -> str ^ efName_list_to_string efNamelst) "" lst
and efNameOfHandler_to_string (pattern: string) (efName_lst_lst: efName list list) :string =
  "pattern: { " ^ pattern ^ " } \n" ^ List.fold_left (fun str efNamelst -> str ^ efName_list_to_string efNamelst) "" efName_lst_lst
and handlers_to_string (handlers: efNameOfHandler list) =
  match handlers with
  | [] -> "]"
  | handler::handlers -> (handler_to_string handler) ^ " ] , [ " ^ (handlers_to_string handlers)
and efName_to_string (efName: efName) :string =
  match efName with
  | FunctionName (name,handler)  -> "(" ^ name ^ " , [ " ^ (handlers_to_string handler) ^ ")"
  | EffectName name -> name
and efName_list_to_string efName_list =
  match efName_list with
  | [] -> "\n"
  | efName::efName_list -> (efName_list_to_string efName_list) ^ " -> " ^ (efName_to_string efName)