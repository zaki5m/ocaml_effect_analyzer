open Function_call
open Function_call__Efname_formatter
open Function_call__Effect_analyzer_core

let file = "../../../../test/ocaml_code/test2.ml"
let function_call_test = 
  let result = parse_test_ocaml_file file in
  assert (List.length result = 4);
  let first = List.hd result in
  let expect_first_node = Node (Empty, [Node (FunctionName ("+", []), [])]) in
  assert (first = (("add", 2), expect_first_node));
  let second = List.hd (List.tl result) in
  let expect_second_node = Node (Empty, [Node (FunctionName ("-", []), [])]) in
  assert (second = (("sub", 2), expect_second_node));
  let third = List.hd (List.tl (List.tl result)) in
  let expect_third_node = Node (Empty, [Node (FunctionName ("*", []), [])]) in
  assert (third = (("mul", 2), expect_third_node));
  let fourth = List.hd (List.tl (List.tl (List.tl result))) in
  let sub_tree = Node (FunctionName ("sub", []), []) in
  let mul_tree = Node (FunctionName ("mul", []), [sub_tree]) in
  let add_tree = Node (FunctionName ("add", []), [mul_tree]) in
  let expect_fourth_node = Node (Empty, [add_tree]) in
  print_endline (efNameTree_to_string (snd fourth));
  assert (fourth = (("main", 0), expect_fourth_node));
  print_endline "function_call_test <test2> passed"