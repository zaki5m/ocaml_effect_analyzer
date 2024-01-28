(* 関数の引数で関数呼び出しが起こる *)
let add a b = 
  a + b

let sub a b =
  a - b

let mul a b =
  a * b

let main = 
  let a = 1 in
  let b = 2 in
  let _ = sub (mul a b) (add a b) in
  ()
  