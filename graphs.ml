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

  let num_nodes g =
    g.num_nodes

  let string_of_graph g = 
    "Graph: " ^ (EdgeDict.string_of_dict g.edges)

  let from_edges (es: (node *  float * node) list) : graph =
    List.fold_left (fun g (src, wt, dst) ->
      if wt < 0. then failwith "No negative edge weights."
      else add_edge g src dst wt) empty es
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
  let empty =
    {num_nodes = 0; size = 1; nodes = Array.make 1 0;
     m = Array.create_matrix 1 1 infinity}
      
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
    else if has_node g n then failwith "the node already exists"
    (* we might not want this to fail but just not include it and keep
       going *)
    else (g.nodes.(g.num_nodes) <- n; g.num_nodes <- g.num_nodes + 1); g

 (*The function index returns the index of the node n in the graph g. If the node does not exist, a Not_found exception is thrown.*)
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
   (* UNUSED MATCH CASE WHAT OCAML *)
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
    let all = List.fold_left (fun l (src, wt, dst) -> src :: dst :: l) [] nfnlist in
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
    let g =
      {num_nodes = 0; size = s; nodes = Array.make s 0;
       m = Array.create_matrix s s infinity}
    in
    List.fold_left (fun g (src, wt, dst) ->
      if wt < 0. then failwith "No negative edge weights."
      else add_edge g src dst wt) g es


  let g = from_edges [(0,1.,1); (1, 5., 4); (0, 2., 2); 
		      (2, 3., 4); (3, 6., 4); (2, 4., 3)];;
  assert(print_string (string_of_int g.size) = ());;
  
      
end
