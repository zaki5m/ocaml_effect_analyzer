(* Effectの発生が一回起こる(handler内でcontinue) *)
open Effect.Deep

(*type _ Effect.t += Xchg: int -> int t
type _ Effect.t += Nam: unit -> int t
type _ Effect.t += Fib: int -> int t
type _ Effect.t += Inf: int -> int t*)
type _ Effect.t += Increment : int -> int Effect.t

let sum_up acc = 
    let tmp = perform (Increment acc) in
    let tmp1 = perform (Increment tmp) in
    tmp1

let main () = 
    match_with sum_up 10 
    { effc = (fun (type c) (eff: c Effect.t) ->
        match eff with
        | Increment s -> Some (fun (k: (c,_) continuation) -> 
                continue k s)
        | _ -> None);
      exnc = (fun e -> raise e);
      retc = (fun a -> 
        match_with sum_up a {
          effc = (fun (type c) (eff: c Effect.t) ->
              match eff with
              | Increment s -> Some (fun (k: (c,_) continuation) -> 
                      continue k s)
              | _ -> None);
          exnc = (fun e -> raise e);
          retc = (fun a -> a)
      })
    }
    
let _ = main ()