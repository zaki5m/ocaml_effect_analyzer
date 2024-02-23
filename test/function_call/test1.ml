open Function_call
open Function_call__Efname_formatter
open Function_call__Effect_analyzer_core

let file = "../../../../test/ocaml_code/test1.ml"
let function_call_test = 
  let result = parse_test_ocaml_file file in
  assert (List.length result = 4);
  let first = List.hd result in
  let expect_first_node = 
    Node (Empty, 
      [Node (FunctionName ("+", [], [ArgsVar ("b", Leaf);ArgsVar ("a", Leaf)], [ArgVar "a"; ArgVar "b"]), [])]) 
  in
  assert (first = (("add", 2), expect_first_node, [ArgsVar ("a", Leaf); ArgsVar ("b", Leaf)]));
  let second = List.hd (List.tl result) in
  let expect_second_node = Node (Empty, [Node (FunctionName ("-", [], [ArgsVar ("b", Leaf);ArgsVar ("a", Leaf)], [ArgVar "a"; ArgVar "b"]), [])]) in
  assert (second = (("sub", 2), expect_second_node, [ArgsVar ("a", Leaf); ArgsVar ("b", Leaf)]));
  let third = List.hd (List.tl (List.tl result)) in
  let expect_third_node = Node (Empty, [Node (FunctionName ("*", [], [ArgsVar ("b", Leaf);ArgsVar ("a", Leaf)], [ArgVar "a"; ArgVar "b"]), [])]) in
  assert (third = (("mul", 2), expect_third_node, [ArgsVar ("a", Leaf); ArgsVar ("b", Leaf)]));
  let fourth = List.hd (List.tl (List.tl (List.tl result))) in
  let mul_tree = Node (FunctionName ("mul", [], [LocalVar ("b", [], Leaf);LocalVar ("a", [], Leaf)], [ArgVar "a"; ArgVar "b"]), []) in
  let sub_tree = Node (FunctionName ("sub", [], [LocalVar ("b", [], Leaf);LocalVar ("a", [], Leaf)], [ArgVar "a"; ArgVar "b"]), [mul_tree]) in
  let add_tree = Node (FunctionName ("add", [], [LocalVar ("b", [], Leaf);LocalVar ("a", [], Leaf)], [ArgVar "a"; ArgVar "b"]), [sub_tree]) in
  let expect_fourth_node = Node (Empty, [add_tree]) in
  let (_, fourth_tree, _) = fourth in
  print_endline (efNameTree_to_string fourth_tree);
  assert (fourth = (("main", 0), expect_fourth_node, []));
  print_endline "function_call_test <test1> passed"