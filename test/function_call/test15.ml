open Function_call
open Function_call__Efname_formatter
open Function_call__Effect_analyzer_core

let file = "../../../../test/ocaml_code/test15.ml"
let function_call_test = 
  let result = parse_test_ocaml_file file in
  assert (List.length result = 4);
  let first = List.hd result in
  let expect_first_node =
    Node (Root, 
      [Node (EffectName ("Open", [ArgsVar ("file_name", Leaf)], [ArgVar "file_name"], false), 
        [Node (Conditions (
          [Node (FunctionName ("unknown", [], [ArgsVar ("file_name", Leaf)], [ArgValue]), 
            [Node (EffectName ("Write", [ArgsVar ("file_name", Leaf)], [ArgValue], false), 
              [Node (EffectName ("Close", [ArgsVar ("file_name", Leaf)], [ArgVar "file_name"], false), 
                [Node (Conditions (
                  [Node (FunctionName ("unknown", [], [ArgsVar ("file_name", Leaf)], [ArgValue]), 
                    [Node (FunctionName ("f1", [], [ArgsVar ("file_name", Leaf)], [ArgVar "file_name"]), []); Leaf])
                  ]
                ), [])])]); 
            Node (EffectName ("Read", [ArgsVar ("file_name", Leaf)], [ArgVar "file_name"], false), 
              [Node (FunctionName ("print_endline", [], [LocalVar ("content", [], Leaf) ;ArgsVar ("file_name", Leaf)], [ArgVar "content"]),
                [Node (EffectName ("Close", [LocalVar ("content", [], Leaf) ;ArgsVar ("file_name", Leaf)], [ArgVar "file_name"], false),[])])
                ])])
          ]), [])])
      ]) 
  in
  let (_,first_tree,_) = first in
  print_endline (efNameTree_to_string first_tree);
  assert (first = (("f1", 1), expect_first_node, [ArgsVar ("file_name", Leaf)]));
  let second = List.hd (List.tl result) in
  let expect_second_node = Node (Root, [Node (EffectName ("Open", [ArgsVar ("file_name", Leaf)], [ArgVar "file_name"], false), [])]) in
  let (_,second_tree,_) = second in
  print_endline (efNameTree_to_string second_tree);
  assert (second = (("f2", 1), expect_second_node, [ArgsVar ("file_name", Leaf)]));
  let third = List.hd (List.tl (List.tl result)) in
  let expect_handler = 
    [
      Retc (Node (FunctionName ("unknown", [], [ArgsVar ("_", Leaf); LocalVar ("file_name", [], Leaf); ArgsVar ("()", Leaf)], [ArgValue]), []));
      Exnc [("_", Node (FunctionName ("raise", [], [ArgsVar ("e", Leaf); LocalVar ("file_name", [], Leaf); ArgsVar ("()", Leaf)], [ArgVar "e"]), []))];
      Effc [("_", Node (Empty, []), []); 
            ("Read", Node (Empty, 
              [Node (Conditions 
                [Node (FunctionName ("=", [], [ArgsVar ("k", Leaf); ArgsVar ("eff", Leaf); LocalVar ("file_name", [], Leaf); ArgsVar ("()", Leaf)], [ArgVar "s"; ArgValue]),
                  [Node (FunctionName ("continue", [], [ArgsVar ("k", Leaf); ArgsVar ("eff", Leaf); LocalVar ("file_name", [], Leaf); ArgsVar ("()", Leaf)], [ArgVar "k"; ArgValue]), []);
                    Node (EffectName ("Copy", [ArgsVar ("k", Leaf); ArgsVar ("eff", Leaf); LocalVar ("file_name", [], Leaf); ArgsVar ("()", Leaf)], [ArgValue], false), [])])], []) ]), 
                  [ArgsVar ("k", Leaf)]);
            ("Write", Node (Empty, [Node (FunctionName ("continue", [], [ArgsVar ("k", Leaf); ArgsVar ("eff", Leaf); LocalVar ("file_name", [], Leaf); ArgsVar ("()", Leaf)], [ArgVar "k"; ArgValue]), [])]), [ArgsVar ("k", Leaf)]);
            ("Close", Node (Empty, [Node (FunctionName ("continue", [], [ArgsVar ("k", Leaf); ArgsVar ("eff", Leaf); LocalVar ("file_name", [], Leaf); ArgsVar ("()", Leaf)], [ArgVar "k"; ArgValue]), [])]), [ArgsVar ("k", Leaf)]);
            ("Open", Node (Empty, [Node (FunctionName ("continue", [], [ArgsVar ("k", Leaf); ArgsVar ("eff", Leaf); LocalVar ("file_name", [], Leaf); ArgsVar ("()", Leaf)], [ArgVar "k"; ArgValue]), [])]), [ArgsVar ("k", Leaf)])
          ] 
    ] 
  in
  let expect_third_node = 
    Node (Root, 
      [Node (FunctionName ("f1", expect_handler, [LocalVar ("file_name", [], Leaf); ArgsVar ("()", Leaf)], [ArgVar "file_name"]), 
        [ Node (FunctionName ("f2", [], [LocalVar ("file_name", [], Leaf); ArgsVar ("()", Leaf)], [ArgVar "file_name"]),
          [Node (EffectName ("Close", [LocalVar ("file_name", [], Leaf); ArgsVar ("()", Leaf)], [ArgVar "file_name"], false), [])])
        ])
      ]) in
  let (_,third_tree,local_var) = third in
  print_endline "-----------";
  print_endline (efNameTree_to_string third_tree);
  print_endline "-----------";
  print_endline (efNameTree_to_string expect_third_node);
  print_endline "-----------";
  List.length local_var |> print_int;
  assert (third = (("main", 1), expect_third_node, [ArgsVar ("()", Leaf)]));
  let forth = List.hd (List.tl (List.tl (List.tl result))) in
  let expect_forth_handler = 
    [
      Retc (Node (FunctionName ("unknown", [], [ArgsVar ("_", Leaf)], [ArgValue]), []));
      Exnc [("_", Node (FunctionName ("raise", [], [ArgsVar ("e", Leaf)], [ArgVar "e"]), []))];
      Effc [("_", Node (Empty, []), []); 
            ("Copy", Node (Empty, [Node (FunctionName ("continue", [], [ArgsVar ("k", Leaf); ArgsVar ("eff", Leaf)], [ArgVar "k"; ArgValue]), [])]), [ArgsVar ("k", Leaf)]);
            ("Read", Node (Empty, [Node (FunctionName ("continue", [], [ArgsVar ("k", Leaf); ArgsVar ("eff", Leaf)], [ArgVar "k"; ArgValue]), [])]), [ArgsVar ("k", Leaf)]);
            ("Write", Node (Empty, [Node (FunctionName ("continue", [], [ArgsVar ("k", Leaf); ArgsVar ("eff", Leaf)], [ArgVar "k"; ArgValue]), [])]), [ArgsVar ("k", Leaf)]);
            ("Close", Node (Empty, [Node (FunctionName ("continue", [], [ArgsVar ("k", Leaf); ArgsVar ("eff", Leaf)], [ArgVar "k"; ArgValue]), [])]), [ArgsVar ("k", Leaf)]);
            ("Open", Node (Empty, [Node (FunctionName ("continue", [], [ArgsVar ("k", Leaf); ArgsVar ("eff", Leaf)], [ArgVar "k"; ArgValue]), [])]), [ArgsVar ("k", Leaf)])
          ] 
    ] 
  in
  let expect_forth_node = Node (Root, 
    [Node (FunctionName ("main", expect_forth_handler, [], [ArgValue]), [])
    ]) in
  let (_,forth_tree,_) = forth in
  print_endline "-----------";
  print_endline (efNameTree_to_string forth_tree);
  print_endline "-----------";
  print_endline (efNameTree_to_string expect_forth_node);
  print_endline "-----------";
  assert (forth = (("_", 0), expect_forth_node, []));
  print_endline "function_call_test <test5> passed"