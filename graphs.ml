open Order

(* A signature for directed graphs with unweighted edges *)
module type GRAPH =
sig 
  type node = int

  type graph 
    
  val empty : graph

  val nodes : graph -> node list

  val is_empty : graph -> bool
    
  val add_node : graph -> node -> graph

  (* Adds the nodes if they aren't already present. *)
  (* and the weight of that edge *)
  val add_edge : graph -> node -> node -> float -> graph
    
  (* Return None if node isn't in the graph *)
  val neighbors : graph -> int -> (node * float) list option

  (* Return None if node isn't in the graph *)
  (* val outgoing_edges : graph -> node -> (node * float * node) list option*)
    
  val has_node : graph -> node -> bool

  val num_nodes : graph -> int

  val string_of_graph : graph -> string

  val from_edges : (node * float * node) list -> graph

end
   
module Graph : GRAPH =
struct
  open Order
  type node = int
    
  (* We'll represent a graph as an edge dictionary:
     dictionary: node -> neighbor set
     Every node in the graph must be a key in the dictionary.
  *)
    
(* not sure which one is correct here because of the merge *)
  module NeighborSet = Myset.Make(
     struct
        type t = node * float
        let compare (n1, w1) (n2, w2) = int_compare n1 n2
        let string_of_t (n, w) = string_of_int n ^ ", " ^ string_of_float w
    (*    let gen () = (N.gen (), 1.)
        let gen_random () = (N.gen (), 1.)
        let gen_gt x () = (N.gen (), 2.)
        let gen_lt x () = (N.gen (), 0.)
        let gen_between x y () = None *)
      end)

    
  module EdgeDict = Dict.Make(
    struct
      type key = node
      type value = NeighborSet.set
      let compare = int_compare
      let string_of_key = string_of_int
      let string_of_value ns = NeighborSet.string_of_set ns
    (*  let gen_key () = 0
      let gen_key_random () = 0
      let gen_key_gt x () = 1
      let gen_key_lt x () = (-1)
      let gen_key_between x y () = None
       let gen_value () = N.gen 
      let gen_pair () = (gen_key(),gen_value()) *)
    end)
    
  module IntNode = Dict.Make(
    struct 
      type key = int
      type value = node
      let compare = int_compare
      let string_of_key = string_of_int
      let string_of_value = string_of_int
    (*  let gen_key () = 0
      let gen_key_random () = 0
      let gen_key_gt x () = 1
      let gen_key_lt x () = (-1)
      let gen_key_between x y () = None
      let gen_value () = N.gen
      let gen_pair () = (gen_key(),gen_value()) *)
    end)
    
 type graph = { edges : EdgeDict.dict ;
                num_nodes : int ;
                index_to_node_map : IntNode.dict }

 let empty : graph = { edges = EdgeDict.empty;
                       num_nodes = 0;
                       index_to_node_map = IntNode.empty }

 let add_node g n =
   if EdgeDict.member g.edges n then g
   else
     { edges = EdgeDict.insert g.edges n (NeighborSet.empty) ;
       num_nodes = g.num_nodes + 1 ;
       index_to_node_map = 
         IntNode.insert g.index_to_node_map g.num_nodes n }

  let nodes g =
    EdgeDict.fold (fun k v r -> k :: r) [] g.edges
      
  let is_empty g = (g.num_nodes = 0)
          
  (* Adds the nodes if they aren't already present. *)
  let add_edge g src dst wt =
    let new_neighbors = match EdgeDict.lookup g.edges src with
      | None -> NeighborSet.insert (dst, wt) NeighborSet.empty 
      | Some s -> NeighborSet.insert (dst, wt) s
    in
      (* ensure both src and dst in the graph before adding edge *)
    let g' = (add_node (add_node g src) dst) in
      {edges = EdgeDict.insert g'.edges src new_neighbors;
       num_nodes = g'.num_nodes;
       index_to_node_map = g'.index_to_node_map}

  let neighbors g node_id : (node * float) list option = 
    match EdgeDict.lookup g.edges node_id with
      | None -> None
      | Some s -> Some (NeighborSet.fold (fun neigh r -> neigh :: r) [] s)
          
 (* let outgoing_edges g src : (node * float * node) list option = 
    match EdgeDict.lookup g.edges src with
      | None -> None
      | Some s -> Some (NeighborSet.fold (fun (dst, wt) r -> 
                                             (src, wt, dst) :: r) [] s) *)
  let has_node g n = 
    match EdgeDict.lookup g.edges n with
      | None -> false
      | _ -> true

 (* let get_random_node g = 
    if g.num_nodes = 0 then None else
      let r = Random.int (g.num_nodes) in
        IntNode.lookup g.index_to_node_map r *)

  let num_nodes g =
    g.num_nodes

  let string_of_graph g = 
    "Graph: " ^ (EdgeDict.string_of_dict g.edges)

  let from_edges (es: (node *  float * node) list) : graph =
    List.fold_left (fun g (src, wt, dst) -> add_edge g src dst wt) empty es
end
