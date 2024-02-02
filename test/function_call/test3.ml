open Function_call

let file = "../../../../test/ocaml_code/test3.ml"
let function_call_test = 
  let result = parse_test_ocaml_file file in
  assert (List.length result = 3);
  let first = List.hd result in
  assert (first = ("sum_up", [[EffectName "Increment"]]));
  let second = List.hd (List.tl result) in
  assert (second = ("main", [[FunctionName ("sum_up",[Effc [("_", []); ("Increment", [[FunctionName ("continue", [])]])]; Exnc [("_", [[FunctionName ("raise", [])]])]; Retc [[FunctionName ("unknown", [])]]])]]));
  print_endline "function_call_test <test3> passed"