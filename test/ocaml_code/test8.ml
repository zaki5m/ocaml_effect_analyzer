(* 関数を引数にとる関数が存在する *)
open Effect.Deep

type _ Effect.t += Increment : int -> int Effect.t
let sum_up acc = 
    let x = perform (Increment acc) in 
    x

let main f = 
  match_with sum_up 10 
  { effc = (fun (type c) (eff: c Effect.t) ->
      match eff with
      | Increment s -> Some (fun (k: (c,_) continuation) -> 
              continue k s)
      | _ -> None);
    exnc = (fun e -> raise e);
    retc = (fun x -> f x)
  }
      
    
let _ = main sum_up