(* local関数が存在する *)
open Effect.Deep

type _ Effect.t += Increment : int -> int Effect.t

let sum_up acc = 
    let tmp = perform (Increment acc) in
    tmp

let main () = 
    let mul a = 
        perform (Increment a) 
    in 
    match_with mul 2
    { effc = (fun (type c) (eff: c Effect.t) ->
        match eff with
        | Increment s -> Some (fun (k: (c,_) continuation) -> 
                continue k s)
        | _ -> None);
      exnc = (fun e -> raise e);
      retc = (fun _ -> Printf.printf "finish\n")
    }
      
    
let _ = main ()