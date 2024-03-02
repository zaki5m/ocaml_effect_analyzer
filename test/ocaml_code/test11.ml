(* 関数を引数にとる関数が存在する *)
open Effect
open Effect.Deep

type _ Effect.t += Get : unit -> int Effect.t
type _ Effect.t += Set : int -> unit Effect.t
let f1 a = 
    let x = perform (Get ()) in
      let _ = perform (Set (a + x)) in
      perform (Get ())

let main1 () = 
  let m = ref 0 in
  match_with f1 10 
  { effc = (fun (type c) (eff: c Effect.t) ->
      match eff with
      | Get () -> Some (fun (k: (c,_) continuation) -> 
              continue k !m)
      | Set s -> Some (fun (k: (c,_) continuation) -> 
              continue k ())
      | _ -> None);
    exnc = (fun e -> raise e);
    retc = (fun x -> x)
  }
      
    
let _ = main1 ()