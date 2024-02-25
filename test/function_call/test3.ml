open Function_call
open Function_call__Efname_formatter
open Function_call__Effect_analyzer_core

let file = "../../../../test/ocaml_code/test3.ml"
let function_call_test = 
  let result = parse_test_ocaml_file file in
  assert (List.length result = 3);
  let first = List.hd result in
  let expect_first_node = Node (Empty, [Node (EffectName "Increment",[])]) in 
  assert (first = (("sum_up", 1), expect_first_node, [ArgsVar ("acc", Leaf)]));
  let second = List.hd (List.tl result) in
  let expect_handler = 
    [Retc (Node (FunctionName ("unknown", [], [ArgsVar ("_", Leaf); ArgsVar ("()", Leaf)], [ArgValue]), []));
    Exnc [("_", Node (FunctionName ("raise", [], [ArgsVar ("e", Leaf); ArgsVar ("()", Leaf)], [ArgVar "e"]), []))]; 
    Effc [("_", Node (Empty, [])); ("Increment", Node (Empty, [Node (FunctionName ("continue", [], [ArgsVar ("k", Leaf); ArgsVar ("eff", Leaf); ArgsVar ("()", Leaf)], [ArgVar "k"; ArgVar "s"]), [])]))]
    ] 
  in
  let (_, second_tree, _) = second in
  print_endline (efNameTree_to_string second_tree);
  let expect_second_node = Node (Empty, [Node (FunctionName ("sum_up", expect_handler, [ArgsVar ("()", Leaf)], [ArgValue]), [])]) in
  assert (second = (("main", 1), expect_second_node, [ArgsVar ("()", Leaf)]));
  let third = List.hd (List.tl (List.tl result)) in
  let expect_third_node = Node (Empty, [Node (FunctionName ("main", [], [], [ArgValue]), [])]) in
  assert (third = (("_", 0), expect_third_node, []));
  print_endline "function_call_test <test3> passed"