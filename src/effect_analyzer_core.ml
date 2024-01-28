(* 関数名かエフェクト名かの区別を行う *)
type efName = 
  | FunctionName of string
  | EffectName of string