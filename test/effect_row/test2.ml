open Function_call
open Function_call__Efname_formatter
open Function_call__Effect_analyzer_core
let file = "../../../../test/ocaml_code/test2.ml"
let function_call_test = 
  let result = effect_row_test file in
  assert (List.length result = 0);
  print_endline "function_call_test <test2> passed"