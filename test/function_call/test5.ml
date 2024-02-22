open Function_call
open Function_call__Efname_formatter
open Function_call__Effect_analyzer_core

let file = "../../../../test/ocaml_code/test5.ml"
let function_call_test = 
  let result = parse_test_ocaml_file file in
  assert (List.length result = 3);
  let first = List.hd result in
  let expect_first_node =
    Node (Empty, 
      [Node (EffectName "Increment", 
        [Node (FunctionName ("=", []),
          [Node (EffectName "Increment", []); Node (EffectName "Decrement", [])])
        ])
      ]) 
  in
  assert (first = (("sum_up", 1), expect_first_node));
  let second = List.hd (List.tl result) in
  let expect_handler = 
    [
      Effc [("_", Node (Empty, [])); 
            ("Decrement", Node (Empty, [Node (FunctionName ("continue", []), [])]));
            ("Increment", Node (Empty, [Node (FunctionName ("continue", []), [])]))
          ]; 
      Exnc [("_", Node (FunctionName ("raise", []), []))]; 
      Retc (Node (FunctionName ("unknown", []), []))
    ] 
  in
  let expect_second_node = Node (Empty, [Node (FunctionName ("sum_up", expect_handler), [])]) in
  print_endline (efNameTree_to_string (snd second));
  assert (second = (("main", 1), expect_second_node));
  let third = List.hd (List.tl (List.tl result)) in
  let expect_third_node = Node (Empty, [Node (FunctionName ("main", []), [])]) in
  assert (third = (("_", 0), expect_third_node));
  print_endline "function_call_test <test5> passed"