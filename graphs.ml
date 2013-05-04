type node = int

(* A signature for directed graphs with unweighted edges *)
module type GRAPH =
sig 
  type graph 
    
  val empty : int -> graph

  val nodes : graph -> node list

  val is_empty : graph -> bool
    
  val add_node : graph -> node -> graph

  val add_edge : graph -> node -> node -> float -> graph
    
  val neighbors : graph -> int -> (node * float) list option

  val has_node : graph -> node -> bool

  val num_nodes : graph -> int

  val print_graph : graph -> unit

  val from_edges : (node * float * node) list -> graph

  val run_tests : unit -> unit

end
   
module Dictionary : GRAPH =
struct
  open Order
    
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

  let print_graph g = 
    print_string ("Graph: " ^ (EdgeDict.string_of_dict g.edges))

  let from_edges (es: (node *  float * node) list) : graph =
    List.fold_left (fun g (src, wt, dst) -> add_edge g src dst wt) (empty 0) es
  
  let run_tests () =
    let g = from_edges [(0,1.,1); (1, 5., 4); (0, 2., 2); 
			(2, 3., 4); (3, 6., 4); (2, 4., 3)] in
    assert(g.num_nodes = 5);
    (*assert(print_graph g = ());*)

end

module Matrix : GRAPH =
struct
  type adj_mat = float array array
  type graph = { mutable num_nodes : int; (* nodes added *)
		 size : int; (* max nodes *)
		 m : adj_mat} ;;(* cost between nodes *)

  (* seems problematic *) 
  let empty s =
    {num_nodes = 0; size = s; m = Array.create_matrix s s infinity}
      
  let nodes g =
    let rec aux (i: int) (lst: int list) : int list =
      if i >= 0
      then aux (i - 1) (i::lst)     
      else lst
    in
    aux (g.num_nodes - 1) []

  let is_empty g = g.num_nodes = 0
    
  (* Checks if the node n is contained in the graph g. *)
  (* IMPROVE THIS *) 
  let has_node g n =
    n < g.num_nodes

  let add_node g n : graph =
    if has_node g n then g
    else (g.num_nodes <- g.num_nodes + 1; g)

  let print_graph g = 
    let rec aux n i = 
      if n = g.size then print_string "!"
      else if i = g.size then (print_string "\n"; aux (n+1) 0)
      else (print_string (string_of_int n ^ " - " ^ string_of_int i ^ ": " ^
			   string_of_float g.m.(n).(i) ^ "; "); aux n (i+1))
    in
    aux 0 0

  (* and the weight of that edge *)
  let add_edge g x y c =
    g.m.(x).(y) <- c; g
  
  (* Return None if no neighbors. *)
  let neighbors g n =
    (* row of matrix containing n's neighbors is g.m.n *) 
    let rec aux n i neighbor_list = 
      if i = g.size then neighbor_list
     (* put all of the neighbors in a list *) 
      else if g.m.(n).(i) = infinity then aux n (i+1) neighbor_list
      else aux n (i + 1) ((i, g.m.(n).(i))::neighbor_list)
   in
   let list = aux n 0 [] in
   match list with
   | [] -> None
   | _ -> Some list
     
  let num_nodes g = g.size

  let sized nfnlist : int =
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
    let emptyg = empty s in
    List.fold_left (fun g (src, wt, dst) ->
      let a = add_node g src in 
      let b = add_node a dst in
      add_edge b src dst wt) emptyg es
      
  let run_tests () =
    let g = from_edges [(0,1.,1); (1, 5., 4); (0, 2., 2); 
			(2, 3., 4); (3, 6., 4); (2, 4., 3)] in
    assert(g.size = 5);
    assert(num_nodes g = 5);
    assert(g.m.(0).(1) = 1.);
    assert(g.m.(1).(4) = 5.);
    assert(g.m.(0).(2) = 2.);
    assert(g.m.(2).(4) = 3.);
    assert(g.m.(3).(4) = 6.);
    assert(g.m.(2).(3) = 4.);
    
end;;

Dictionary.run_tests ();;
Matrix.run_tests ()

(*
module TestGraph =
struct 

  module G = Dictionary 

  let g = G.add_edge (G.empty 0) 0 1 3.;;
  let g2 = G.add_edge g 0 2 4.;;

  let _ = (
    assert (G.has_node g 0);
    assert (G.has_node g 1);
    assert (G.has_node g 2 = false);
    assert (G.has_node g2 2);
    assert (G.has_node g2 3 = false);

    assert (List.length (G.nodes (G.empty 0)) = 0) ;
    assert (List.length (G.nodes (G.add_node (G.empty 0) 1)) = 1) ;

    assert (List.length (G.nodes g) = 2) ;

    assert (List.length (G.nodes g2) = 3) ;

    assert (let t = G.neighbors g2 0 in
            t = Some [(1, 3.);(2,4.)] or t = Some [(2,4.);(1,3.)]) )
    
  let g3 = G.from_edges [(0,2.,1);(0,3.,2);(1,4.,3);
		       (3,5.,4);(3,6.,5);(7,8.,4);
		       (6,7.,5);(7,9.,6)] ;;

  assert(let a = G.neighbors g3 0 in 
	 a = [(1,2.);(2,3.)] or a = [(2,3.);(1,2.)]) ;
  assert(deopt_lst (G.neighbors g3 1) = [(3,4.)]) ;
  assert(deopt_lst (G.neighbors g3 2) = []) ;
  assert(let d = deopt_lst (G.neighbors g3 3) in 
	 d = [(4,5.);(5,6.)] or d = [(5,6.);(4,5.)]) ;
  assert(deopt_lst (G.neighbors g3 4) = []) ;
  assert(deopt_lst (G.neighbors g3 5) = []) ;
  assert(deopt_lst (G.neighbors g3 6) = [(5,7.)]) ;
  assert(let h = deopt_lst (G.neighbors g3 7) in 
	 h = [(4,8.);(6,9.)] or h = [(6,9.);(4,8.)]) 

  let tester oelist : unit =
    match oelist with
    | None -> print_string ""
    | Some s -> print_string (List.fold_right (fun (dst, wt, src) y -> 
      dst ^ " & " ^ string_of_float wt ^ " & " ^ src ^ " ; " ^ y) s "")
;;

end
  *)
