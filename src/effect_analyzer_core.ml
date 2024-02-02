(* 関数名かエフェクト名かの区別を行う *)
type efName = 
  | FunctionName of string * efNameOfHandler list
  | EffectName of string
and efNameOfHandler = 
  | Effc of (string * efName list list) list (* エフェクト名とそれをcatchした時のefName list *)
  | Exnc of (string * efName list list) list (* exception名とそれをcatchした時のefName list *)
  | Retc of efName list list (* 正常にフローを抜けた時のefName list *)

  (* handlerが変数で与えられた際の型を定義しておく必要がある *)
