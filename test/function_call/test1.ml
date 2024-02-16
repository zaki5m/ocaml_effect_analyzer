open Function_call

let file = "../../../../test/ocaml_code/test1.ml"
let function_call_test = 
  let result = parse_test_ocaml_file file in
  assert (List.length result = 4);
  let first = List.hd result in
  assert (first = (("add", 2), [[FunctionName ("+", [])]]));
  let second = List.hd (List.tl result) in
  assert (second = (("sub", 2), [[FunctionName ("-",[])]]));
  let third = List.hd (List.tl (List.tl result)) in
  assert (third = (("mul", 2), [[FunctionName ("*", [])]]));
  let fourth = List.hd (List.tl (List.tl (List.tl result))) in
  assert (fourth = (("main", 0), [[FunctionName ("mul", []); FunctionName ("sub",[]); FunctionName ("add",[])]]));
  print_endline "function_call_test <test1> passed"