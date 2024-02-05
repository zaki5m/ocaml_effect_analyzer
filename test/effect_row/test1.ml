open Function_call

let file = "../../../../test/ocaml_code/test1.ml"
let function_call_test = 
  let result = effect_row_test file in
  assert (List.length result = 4);
  let first = List.hd result in
  assert (first = ("add", []));
  let second = List.hd (List.tl result) in
  assert (second = ("sub", []));
  let third = List.hd (List.tl (List.tl result)) in
  assert (third = ("mul", []));
  let fourth = List.hd (List.tl (List.tl (List.tl result))) in
  assert (fourth = ("main", []));
  print_endline "function_call_test <test1> passed"