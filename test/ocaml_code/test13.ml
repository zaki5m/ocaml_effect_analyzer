(* 再帰が存在する *)
open Effect.Deep
open Effect

type _ Effect.t += Increment : int -> int Effect.t
type _ Effect.t += Decrement : int -> int Effect.t

let rec sum_up acc = 
    if acc = 0 then 
      perform (Increment acc)
    else 
      let tmp = perform (Decrement acc) in
      sum_up tmp
      

let main () = 
    match_with sum_up 10 
    { effc = (fun (type c) (eff: c Effect.t) ->
        match eff with
        | Increment s -> Some (fun (k: (c,_) continuation) -> 
                continue k s)
        | Decrement s -> Some (fun (k: (c,_) continuation) ->
                continue k s)
        | _ -> None);
      exnc = (fun e -> raise e);
      retc = (fun _ -> Printf.printf "finish\n")
    }
    
let _ = main ()