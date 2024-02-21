(* 関数名かエフェクト名かの区別を行う *)
type efName = 
  | FunctionName of string * efNameOfHandler list
  | EffectName of string
  | Empty
and efNameOfHandler = 
  | Effc of (string * efNameTree) list (* エフェクト名とそれをcatchした時のefName list *)
  | Exnc of (string * efNameTree) list (* exception名とそれをcatchした時のefName list *)
  | Retc of efNameTree (* 正常にフローを抜けた時のefName list *)
and efNameTree = (* 木構造を表現するためのデータ構造 *)
  | Node of efName * efNameTree list
  | Leaf (* この先には何もないことを示す *)

  (* handlerが変数で与えられた際の型を定義しておく必要がある *)

