(* プロシンデモ用 *)

open Effect.Deep
open Effect

type _ Effect.t += Open : string -> unit Effect.t
type _ Effect.t += Close : string -> unit Effect.t
type _ Effect.t += Write : (string * string) -> unit Effect.t
type _ Effect.t += Read : string -> string Effect.t
type _ Effect.t += Copy : (string * string) -> unit Effect.t

let rec f1 file_name = 
  perform (Open file_name);
  if Random.bool () then
    (perform (Write (file_name, "Hello"));
    perform (Close file_name);
    if Random.bool () then
      f1 file_name
    else
      ())
  else
    let content = perform (Read file_name) in 
    print_endline content;
    perform (Close file_name);
    ()

let f2 file_name = 
  perform (Open file_name);
  ()

let main () = 
  let file_name = "test.txt" in
  let _ = match_with f1 file_name 
    { effc = (fun (type c) (eff: c Effect.t) ->
        match eff with
        | Open s -> Some (fun (k: (c,_) continuation) -> 
                continue k ())
        | Close s -> Some (fun (k: (c,_) continuation) -> 
                continue k ())
        | Write (s1, s2) -> Some (fun (k: (c,_) continuation) -> 
                continue k ())
        | Read s -> Some (fun (k: (c,_) continuation) -> 
                if s = "test.txt" then
                  continue k "Hello"
                else
                  perform (Copy (s, "test.txt"))
              )
        | _ -> None);
      exnc = (fun e -> raise e);
      retc = (fun _ -> Printf.printf "finish\n")
    }
  in
  let _ = f2 file_name in
  perform (Close file_name)