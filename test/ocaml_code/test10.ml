(* 複雑なケース *)
open Effect.Deep

type _ Effect.t += Increment : int -> int Effect.t
type _ Effect.t += Decrement : int -> int Effect.t
type _ Effect.t += Get : unit -> int Effect.t
type _ Effect.t += Set : int -> unit Effect.t

let getter () = perform (Get ())
let setter s = perform (Set s)

let sum_up acc = 
    let tmp = perform (Increment acc) in
    if tmp = 0 then 
      perform (Increment tmp)
    else  
      perform (Decrement tmp)

let get_set () = 
    let tmp = getter () in
    if tmp > 0 then
      let tmp2 = sum_up tmp in
      setter tmp2
    else
      setter 0

let main () = 
    match_with get_set () 
    { effc = (fun (type c) (eff: c Effect.t) ->
        match eff with
        | Increment s -> Some (fun (k: (c,_) continuation) -> 
                continue k s)
        | Decrement s -> Some (fun (k: (c,_) continuation) ->
                continue k s)
        | Get s -> Some (fun (k: (c,_) continuation) -> 
                  continue k 1)
        | Set y -> Some (fun (k: (c,_) continuation) ->
                  continue k ())
        | _ -> None);
      exnc = (fun e -> raise e);
      retc = (fun _ -> Printf.printf "finish\n")
    }
    
let _ = main ()