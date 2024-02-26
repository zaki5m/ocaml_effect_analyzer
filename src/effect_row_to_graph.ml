open Effect_analyzer_core
open Efname_formatter
open Yojson

(* efNametreeのリストに対してEmptyを除去しつつノードにidをふる関数 *)
let rec add_id_to_efNameTreeList (trees: efNameTree list) (id: int) : (efNameTreeWithId list * int) =
  let rec add_id_to_efNameTreeList' (trees: efNameTree list) (id: int) : (efNameTreeWithId list * int) =
    match trees with
    | [] -> ([], id)
    | tree::rest -> 
      let (tree_with_id, new_id) = add_id_to_efNameTree tree id in
      let (rest_with_id, new_id') = add_id_to_efNameTreeList' rest new_id in
      (tree_with_id::rest_with_id, new_id')
  in
  add_id_to_efNameTreeList' trees id
(* efNameTreeに対してEmptyを除去しつつノードにidをふる関数 *)
and add_id_to_efNameTree (tree: efNameTree) (id: int) : (efNameTreeWithId * int) =
  match tree with
  | Leaf -> (LeafWithId id, id+1)
  | Node (name, children) -> ( match name with
    | Empty -> 
      if List.length children = 1 then
        let child = List.hd children in
        add_id_to_efNameTree child id
      else
        let (children_with_id, new_id) = add_id_to_efNameTreeList children id in
        (NodeWithId (name, children_with_id, new_id), new_id+1 )
    | _ -> 
      let (children_with_id, new_id) = add_id_to_efNameTreeList children id in
      NodeWithId (name, children_with_id, new_id), new_id+1
  )

(* efNameから名前を出力する *)
let get_name (efName: efName) : string = match efName with
  | FunctionName (name,_,_,_) -> name
  | EffectName name -> name
  | Empty -> ""

let get_id = function
  | LeafWithId id -> id
  | NodeWithId (_, _, id) -> id


(* nodeとedgeのlistに分ける *)
(* 返り値はnode, edgeタプル *)
let split_node_edge (tree: efNameTreeWithId) : (((int * string) * (int * int)) list * (int * int)  list) =
  let rec loop tree node edge x y = match tree with 
      | LeafWithId _ -> (node, edge)
      | NodeWithId (name, children, id) -> 
        let now_y = ref y in 
        let node' = ((id, (get_name name)), (x, y)) :: node in
        let edge' = List.fold_left (fun acc child -> (id, get_id child) :: acc) edge children in
        now_y := !now_y - 100;
        List.fold_left (fun (node, edge) child -> now_y := !now_y + 100; loop child node edge (x + 100) !now_y) (node', edge') children 
  in
  loop tree [] [] 100 100

(* nodeをjsonに変換する *)
let node_to_json (node: (int * string)) (position: (int * int)) = 
  let (id, name) = node in
  let (x, y) = position in
  let id =  string_of_int id in
  `Assoc [ ("data", 
    `Assoc [("id", `String id); ("name", `String name); ("label", `String "Effect")]
  );
  ("position", 
    `Assoc [("x", `Int x); ("y", `Int y)]
  )
  ]

(* edgeをjsonに変換する *)
let edge_to_json (edge: (int * int)) = 
  let (from, to_) = edge in
  let id = (to_ + 1 ) * (-1) in 
  let id = string_of_int id in
  let from = string_of_int from in
  let to_ = string_of_int to_ in
  `Assoc [ ("data", 
    `Assoc [("id", `String id); ("source", `String from); ("target", `String to_)]
  )]

(* nodeのリストをjsonに変換する *)
let nodes_to_json (nodes: ((int * string) * (int * int)) list) = 
  `List (List.map (fun (node, position) -> node_to_json node position) nodes)

(* edgeのリストをjsonに変換する *)
let edges_to_json (edges: (int * int) list) = 
  `List (List.map edge_to_json edges)

(* (nodes, edegs)をjsonに変換する *)
let effect_row_to_json (nodes: ((int * string) * (int * int)) list) (edges: (int * int) list) = 
  `Assoc [("nodes", nodes_to_json nodes); ("edges", edges_to_json edges)]

(* jsonをelements.jsonに書き込み *)
let write_json json (filename: string) = 
  let oc = open_out filename in
  let str = Yojson.Basic.pretty_to_string json in
  Printf.fprintf oc "%s" str;
  close_out oc

(* efNameTreeWithIdをjsonに変換する *)

