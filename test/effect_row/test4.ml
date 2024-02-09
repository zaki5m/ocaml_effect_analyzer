open Function_call

let file = "../../../../test/ocaml_code/test4.ml"
let function_call_test = 
  let result = effect_row_test file in
  assert (List.length result = 3);
  let first = List.hd result in
  assert (first = ("sum_up", [[EffectName "Increment"; EffectName "Increment"]]));
  let second = List.hd (List.tl result) in
  assert (second = ("main", [[EffectName "Increment"; EffectName "Increment";EffectName "Increment"; EffectName "Increment"]]));
  let third = List.hd (List.tl (List.tl result)) in
  assert (third = ("_", [[EffectName "Increment";EffectName "Increment"; EffectName "Increment"; EffectName "Increment"]]));
  print_endline "function_call_test <test4> passed"