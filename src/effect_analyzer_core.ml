(* 関数名かエフェクト名かの区別を行う *)
type efName = 
  | FunctionName of string * efNameOfHandler list * localVar list * arg list 
  | EffectName of string * localVar list * arg list * bool (* bool値はeffectがhandle済みか未ハンドルかを区別するもの *)
  | Conditions of efNameTree list
  | Empty
  | Root
and efNameOfHandler = 
  | Effc of (string * efNameTree * localVar list) list (* エフェクト名とそれをcatchした時のefName list, local_var_lstは引数 *)
  | Exnc of (string * efNameTree) list (* exception名とそれをcatchした時のefName list *)
  | Retc of efNameTree (* 正常にフローを抜けた時のefName list *)
and efNameTree = (* 木構造を表現するためのデータ構造 *)
  | Node of efName * efNameTree list
  | Leaf (* この先には何もないことを示す *)
(* localで定義された変数(関数)に関する型 *)
and localVar = 
  | LocalVar of string * localVar list * efNameTree (* 変数名(関数名), 引数の内容, その関数の中身のtree *)
  | LocalVarWithId of string * localVar list * efNameTreeWithId (* 変数名(関数名), 引数の内容, その関数の中身のtree, id *)
  | ArgsVar of string * efNameTree (* 引数の変数名, その変数の中身のtree(Leafならば値) *)
  | EmptyVar
(* 関数にapplyするときの引数の型 *)
and arg =
  | ArgVar of string (* 引数が変数で与えられた場合の中身 *)
  | ArgValue 
  (* | ArgApply of efNameTree 引数で関数適用があった場合に対応が必要 *)

  (* handlerが変数で与えられた際の型を定義しておく必要がある *)

(* treeにidを付与した型 *)
and efNameTreeWithId = 
  | NodeWithId of efName * efNameTreeWithId list * int
  | ConditionWithId of efNameTreeWithId list * efNameTreeWithId list * int (* 一つ目が条件分岐のリストでその後ろがその後に続くTreeのリスト *)
  | LeafWithId of int
  | RecNodeWithId of int
