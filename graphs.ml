(* A signature for directed graphs with unweighted edges *)
module type GRAPH =
sig 
  type node = int

  type graph 
    
  val empty : int -> graph

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
   
module Dictionary : GRAPH =
struct
  open Order
  type node = int
    
  (* We'll represent a graph as an edge dictionary:
     dictionary: node -> neighbor set
     Every node in the graph must be a key in the dictionary.
  *)
    
  module NeighborSet = Myset.Make(
    struct
      type t = node * float
      let compare (n1, w1) (n2, w2) = int_compare n1 n2
      let string_of_t (n, w) = string_of_int n ^ ", " ^ string_of_float w
    end)

    
  module EdgeDict = Dict.Make(
    struct
      type key = node
      type value = NeighborSet.set
      let compare = int_compare
      let string_of_key = string_of_int
      let string_of_value ns = NeighborSet.string_of_set ns
    end)
    
  module IntNode = Dict.Make(
    struct 
      type key = int
      type value = node
      let compare = int_compare
      let string_of_key = string_of_int
      let string_of_value = string_of_int
    end)
    
  type graph = { edges : EdgeDict.dict ;
                 num_nodes : int ;
                 index_to_node_map : IntNode.dict }
    
  let empty s : graph = { edges = EdgeDict.empty;
			num_nodes = 0;
			index_to_node_map = IntNode.empty }
    
  let add_node g n =
   if EdgeDict.member g.edges n then g
   else
     { edges = EdgeDict.insert g.edges n (NeighborSet.empty) ;
       num_nodes = g.num_nodes + 1;
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
    List.fold_left (fun g (src, wt, dst) -> add_edge g src dst wt) (empty 0) es
end

module Matrix : GRAPH =
struct
  type node = int
    
  type adj_mat = float array array
  type graph = { mutable num_nodes : int; (* nodes added *)
		 size : int; (* max nodes *)
		 nodes : node array ;
		 m : adj_mat} (* cost between nodes *)

  (* seems problematic *) 
  let empty s =
    {num_nodes = 0; size = s; nodes = Array.make s 0;
     m = Array.create_matrix s s infinity}
      
  let nodes g = 
    let rec arraytolist nodea i currentlist =
      if i = g.num_nodes then currentlist
      else Array.get nodea i :: (arraytolist nodea (i + 1) currentlist)
    in
    arraytolist g.nodes 0 []
      
  let is_empty g = g.num_nodes = 0
    
  (* Checks if the node n is contained in the graph g. *)
  (* IMPROVE THIS *) 
  let has_node g n =
    let rec aux i =
      (i < g.size) & ((g.nodes.(i) = n) or (aux (i+1)))
    in aux 0;;

  let add_node g n =
    if g.num_nodes = g.size then failwith "the graph is full"
    else if has_node g n then g
    else (g.nodes.(g.num_nodes) <- n; g.num_nodes <- g.num_nodes + 1; g)

  (* The function index returns the index of the node n in the graph g. 
     If the node does not exist, a Not_found exception is thrown.*)
  let index n g = 
    let rec aux i = 
      if i >= g.size then raise (Failure "Not_found")
      else if g.nodes.(i) = n then i 
      else aux (i+1)
    in aux 0 ;;

  (* Adds the nodes if they aren't already present. *)
  (* and the weight of that edge *)
  let add_edge g e1 e2 c = 
    try
      let x = index e1 g and y = index e2 g in 
      g.m.(x).(y) <- c ; g
    with Not_found -> failwith "node does not exist" ;;
  
 (* Return None if node isn't in the graph *)
  let neighbors g n =
   (* row of matrix containing n's neighbors is g.m.n *) 
    let rec aux n i neighbor_list = 
      if i = g.size then neighbor_list
     (* put all of the neighbors in a list *) 
     else 
	match g.m.(n).(i) with 
	| infinity -> aux n (i+1) neighbor_list
	| _ -> aux n (i + 1) ((i, g.m.(n).(i))::neighbor_list)
   in
   let list = aux n 0 [] in
   match list with
   | [] -> None
   | _ -> Some list
     
 (* Return None if node isn't in the graph *)
 (* val outgoing_edges : graph -> node -> (node * float * node) list option*)
  
  let num_nodes g = g.num_nodes
    
  let string_of_graph g = ""
	
  let sized nfnlist =
    let all = List.fold_left (fun l (src, wt, dst) -> src::dst::l) [] nfnlist in
    let rec unique nodes =
      match nodes with
      | [] -> []
      | head :: tail ->
	let newtail = unique tail in
	if List.mem head newtail then newtail else head :: newtail
    in
    List.length (unique all)
      
  let from_edges es =
    let s = sized es in
    List.fold_left (fun g (src, wt, dst) -> ignore (add_node g src); 
      ignore (add_node g dst); add_edge g src dst wt) (empty s) es
      
  let g = from_edges [(0,1.,1); (1, 5., 4); (0, 2., 2); 
		      (2, 3., 4); (3, 6., 4); (2, 4., 3)];;
  assert(print_string (string_of_int g.size) = ());;
  
      
end
