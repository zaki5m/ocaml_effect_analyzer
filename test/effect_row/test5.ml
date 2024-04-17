open Function_call
open Function_call__Efname_formatter
open Function_call__Effect_analyzer_core
open Function_call__Function_call_util

let file = "../../../../test/ocaml_code/test5.ml"
let function_call_test = 
  let result = effect_row_test file in
  assert (List.length result = 3);
  let first = List.hd result in
  let expect_first_node = 
    Node (Root, [
      Node (EffectName ("Increment", [], [], false), 
        [Node (Conditions 
          [Node (EffectName ("Increment", [], [], false), []); Node (EffectName ("Decrement", [], [], false), [])]
          , [])])
    ])
  in
  let first = (fst first, remove_id_from_tree (snd first)) in
  assert (first = (("sum_up", 1), expect_first_node));
  let second = List.hd (List.tl result) in
  let expect_second_node = 
    Node (Root, [
      Node (EffectName ("Increment", [], [], true), 
      [Node (Conditions 
      [Node (EffectName ("Increment", [], [], true), [Leaf]); Node (EffectName ("Decrement", [], [], true), [Leaf])]
      , [])])
    ])
  in
  let second = (fst second, remove_id_from_tree (snd second)) in
  print_endline (efNameTree_to_string (snd second));
  assert (second = (("main", 1), expect_second_node));
  let third = List.hd (List.tl (List.tl result)) in
  let expect_third_node = expect_second_node in
  let third = (fst third, remove_id_from_tree (snd third)) in
  assert (third = (("_", 0), expect_third_node));
  print_endline "function_call_test <test5> passed"