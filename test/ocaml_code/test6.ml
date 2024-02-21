(* local関数が存在する *)
open Effect.Deep
let sum_up acc = 
    acc + 1

let main () = 
    let mul a b = a * b in 
    let x = 1 in
    let y = 2 in
    let z = mul x y in
    sum_up z
      
    
let _ = main ()