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
  | Node (name, children) ->
      let (children_with_id, new_id) = add_id_to_efNameTreeList children id in
      NodeWithId (name, children_with_id, new_id), new_id+1

(* efNameから名前を出力する *)
let get_name (efName: efName) : string = match efName with
  | FunctionName (name,_,_,_) -> name
  | EffectName (name, _, _) -> name
  | Empty -> ""
  | Root -> ""
  | Conditions _ -> ""

let get_id = function
  | LeafWithId id -> id
  | NodeWithId (_, _, id) -> id
  | RecNodeWithId id -> id
  | ConditionWithId (_,_,id) -> id

(* condition listの最後のidを全て取得する *)
let rec get_last_id_of_condition (lst: efNameTreeWithId list) = 
  let rec loop lst acc = match lst with
    | [] -> acc
    | hd :: tl -> ( match hd with
      | NodeWithId (_, [], id) -> loop tl (id :: acc)
      | NodeWithId (_, children, id) -> 
        let children = List.filter_map (fun tree ->match tree with LeafWithId _ -> None | RecNodeWithId _ -> None | _ -> Some tree) children in 
        if children = [] then loop tl (id :: acc) else loop tl (loop children acc)
      | ConditionWithId (condition_lst, [], _) ->
        let new_last_lst = get_last_id_of_condition condition_lst in
        loop tl (new_last_lst @ acc)
      | ConditionWithId (_, children, _) ->
        let children = List.filter_map (fun tree ->match tree with LeafWithId _ -> None | _ -> Some tree) children in 
        if children = [] then loop tl acc else loop tl (loop children acc)
      | _ -> loop tl acc
    )
  in
  loop lst []


(* efNameTreeWithIdをjsonに変換する *)

(* nodeとedgeのlistに分ける *)
(* 返り値はnode, edgeタプル *)
let split_node_edge (tree: efNameTreeWithId) : (((int * string) * (int * int)) list * (int * int)  list) =
  let rec loop tree node edge x y = match tree with 
    | NodeWithId (name, children, id) -> 
      let now_y = ref y in 
      let node' = ((id, (get_name name)), (x, y)) :: node in
      let edge' = List.fold_left (fun acc child -> (id, get_id child) :: acc) edge children in
      now_y := !now_y - 100;
      List.fold_left (fun (node, edge) child -> now_y := !now_y + 100; loop child node edge (x + 100) !now_y) (node', edge') children
    | ConditionWithId (condition_lst, children, id) -> 
      let now_y = ref y in 
      let node' = ((id, ""), (x, y)) :: node in
      let edge' = List.fold_left (fun acc child -> (id, get_id child) :: acc) edge condition_lst in
      now_y := !now_y - 100;
      let condition_last_id_lst = get_last_id_of_condition condition_lst in
      let edge' = List.fold_left (fun acc child -> List.map (fun id -> (id, get_id child)) condition_last_id_lst @ acc) edge' children in
      let (node', edge') = List.fold_left (fun (node, edge) child -> now_y := !now_y + 100; loop child node edge (x + 100) !now_y) (node', edge') condition_lst in
      List.fold_left (fun (node, edge) child -> now_y := !now_y + 100; loop child node edge (x + 100) !now_y) (node', edge') children
    | _ -> (node, edge)
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
let edge_to_json (edge: ((int * int)* int)) = 
  let ((from, to_), id) = edge in
  let id = string_of_int id in
  let from = string_of_int from in
  let to_ = string_of_int to_ in
  `Assoc [ ("data", 
    `Assoc [("id", `String id); ("source", `String from); ("target", `String to_)]
  )]

let pre_edges_to_json (edges: (int * int) list) = 
  let edge_id = ref 0 in
  let rec loop edges acc = match edges with
    | [] -> acc
    | edge::rest -> 
      edge_id := !edge_id - 1;
      let new_edge = (edge, !edge_id) in 
      new_edge :: loop rest acc
  in
  loop edges []

(* efNameTreeWithIdをjsonに変換する *)

(* nodeのリストをjsonに変換する *)
let nodes_to_json (nodes: ((int * string) * (int * int)) list) = 
  `List (List.map (fun (node, position) -> node_to_json node position) nodes)

(* edgeのリストをjsonに変換する *)
let edges_to_json (edges: (int * int) list) = 
  `List (List.map edge_to_json (pre_edges_to_json edges))

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

