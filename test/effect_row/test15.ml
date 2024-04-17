open Function_call
open Function_call__Efname_formatter
open Function_call__Effect_analyzer_core
open Function_call__Function_call_util

let file = "../../../../test/ocaml_code/test15.ml"
let function_call_test = 
  let result = effect_row_test file in
  assert (List.length result = 4);
  let first = List.hd result in
  let expect_first_node =
    Node (Root, 
      [Node (EffectName ("Open", [], [], false), 
        [Node (Conditions (
            [Node (EffectName ("Write", [], [], false), 
              [Node (EffectName ("Close", [], [], false), 
                [Node (Conditions (
                  [Node (Empty, []); Leaf]
                ), [])])]); 
            Node (EffectName ("Read", [], [], false), 
                [Node (EffectName ("Close", [], [], false),[])])
          ]), [])])
      ]) 
  in
  let first = (fst first, remove_id_from_tree (snd first)) in
  assert (first = (("f1", 1), expect_first_node));
  let second = List.hd (List.tl result) in
  let expect_second_node = Node (Root, [Node (EffectName ("Open", [], [], false), [])]) in
  let second = (fst second, remove_id_from_tree (snd second)) in
  assert (second = (("f2", 1), expect_second_node));
  let third = List.hd (List.tl (List.tl result)) in
  let expect_third_node =
    Node (Root,
      [Node (Root, 
        [Node (EffectName ("Open", [], [], true), 
          [Node (Conditions (
              [Node (EffectName ("Write", [], [], true), 
                [Node (EffectName ("Close", [], [], true), 
                  [Node (Conditions (
                    [Node (Empty, []); Leaf]
                  ), [])])]); 
              Node (EffectName ("Read", [], [], true), 
                  [Node (Conditions (
                    [Node (EffectName ("Close", [], [], true),[Leaf]); Node (EffectName ("Copy", [], [], false), [])])
                  ,[])])
            ]), 
            [Node (EffectName ("Open", [], [], false), 
              [Node (EffectName ("Close", [], [], false), [])])])])
        ])])
  in
  let third = (fst third, remove_id_from_tree (snd third)) in
  assert (third = (("main", 1), expect_third_node));
  let forth = List.hd (List.tl (List.tl (List.tl result))) in
  let expect_forth_node = 
    Node (Root,
      [Node (Root, 
        [Node (EffectName ("Open", [], [], true), 
          [Node (Conditions (
              [Node (EffectName ("Write", [], [], true), 
                [Node (EffectName ("Close", [], [], true), 
                  [Node (Conditions (
                    [Node (Empty, []); Leaf]
                  ), [])])]); 
              Node (EffectName ("Read", [], [], true), 
                  [Node (Conditions (
                    [Node (EffectName ("Close", [], [], true),[Leaf]); Node (EffectName ("Copy", [], [], true), [Leaf])])
                  ,[])])
            ]), 
            [Node (EffectName ("Open", [], [], true), 
              [Node (EffectName ("Close", [], [], true), [Leaf])])])])
        ])])
  in
  let forth = (fst forth, remove_id_from_tree (snd forth)) in
  print_endline (efNameTree_to_string (snd forth));
  print_endline "====================";
  print_endline (efNameTree_to_string expect_forth_node);
  assert (forth = (("_", 0), expect_forth_node));
  print_endline "function_call_test <test5> passed"