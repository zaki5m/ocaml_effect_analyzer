open Function_call

let file = "../../../../test/ocaml_code/test4.ml"
let function_call_test = 
  let result = parse_test_ocaml_file file in
  assert (List.length result = 3);
  let first = List.hd result in
  assert (first = ("sum_up", [[EffectName "Increment"; EffectName "Increment"]]));
  let second = List.hd (List.tl result) in
  assert (second = ("main", [[FunctionName ("sum_up",[Effc [("_", []); ("Increment", [[FunctionName ("continue", [])]])]; Exnc [("_", [[FunctionName ("raise", [])]])]; Retc [[FunctionName ("sum_up", [Effc [("_", []); ("Increment", [[FunctionName ("continue", [])]])]; Exnc [("_", [[FunctionName ("raise", [])]])]; Retc [[]]])]]])]]));
  let third = List.hd (List.tl (List.tl result)) in
  assert (third = ("_", [[FunctionName ("main", [])]]));
  print_endline "function_call_test <test4> passed"