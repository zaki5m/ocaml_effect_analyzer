open Function_call

let file = "../../../../test/ocaml_code/test2.ml"
let function_call_test = 
  let result = parse_test_ocaml_file file in
  assert (List.length result = 4);
  let first = List.hd result in
  assert (first = ("add", [[FunctionName ("+",[])]]));
  let second = List.hd (List.tl result) in
  assert (second = ("sub", [[FunctionName ("-",[])]]));
  let third = List.hd (List.tl (List.tl result)) in
  assert (third = ("mul", [[FunctionName ("*",[])]]));
  let fourth = List.hd (List.tl (List.tl (List.tl result))) in
  assert (fourth = ("main", [[FunctionName ("sub",[]); FunctionName ("add",[]); FunctionName ("mul",[])]]));
  print_endline "function_call_test <test2> passed"