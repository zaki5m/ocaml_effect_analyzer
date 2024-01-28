open Printf
open Effect_analyzer_core

let efName_to_string efName =
  match efName with
  | FunctionName name  -> name
  | EffectName name -> name

let rec efName_list_to_string efName_list =
  match efName_list with
  | [] -> "\n"
  | efName::efName_list -> (efName_list_to_string efName_list) ^ " -> " ^ (efName_to_string efName)