open Function_call
open Function_call__Efname_formatter
open Function_call__Effect_analyzer_core

let file = "../../../../test/ocaml_code/test5.ml"
let function_call_test = 
  let result = effect_row_test file in
  assert (List.length result = 3);
  let first = List.hd result in
  let expect_first_node = 
    Node (Empty, 
      [Node (EffectName "Increment", 
        [Node (Empty, 
          [Node (EffectName "Increment", []); Node (EffectName "Decrement", [])])
        ])
      ]) 
  in
  assert (first = (("sum_up", 1), expect_first_node));
  let second = List.hd (List.tl result) in
  let expect_second_node = 
    Node (Empty, 
      [Node (Empty, 
        [Node (EffectName "Increment", 
          [Node (Empty, 
            [Node (FunctionName ("unit", [], []), 
              [Node (Empty, 
                [Node (EffectName "Increment", 
                  [Node (FunctionName ("unit", [], []), [])
                  ]);
                Node (EffectName "Decrement", 
                  [Node (FunctionName ("unit", [], []), [])
                  ])
                ])
              ])
            ])
          ])
        ])
      ]) 
  in
  assert (second = (("main", 1), expect_second_node));
  let third = List.hd (List.tl (List.tl result)) in
  let expect_third_node = Node (Empty, [expect_second_node])
  in
  assert (third = (("_", 0), expect_third_node));
  print_endline "function_call_test <test5> passed"