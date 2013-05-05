type node = int

module type GRAPH =
sig 
  type graph 
  val empty : int -> graph
  val neighbors : graph -> int -> (node * float) list option
  val num_nodes : graph -> int
(*  val print_graph : graph -> unit *)
  val from_edges : (node * float * node) list -> graph
  val run_tests : unit -> unit
end

(*******************************************************************************)
(********************   Graph using Edge Dictionary   **************************)
(*******************************************************************************)
   
(* Edge dictionary:
   key -> node
   value -> (weight * neighbor) set *)
  
module Dictionary : GRAPH =
struct
  open Order
    
  module NeighborSet = Myset.Make(
    struct
      type t = node * float
      let compare (n1, w1) (n2, w2) = int_compare n1 n2
      let string_of_t (n, w) = "("^string_of_int n^", "^string_of_float w^")"
      
      let gen () = (0, 0.)
      let gen_random () = (Random.int 100, Random.float 100.)
      let gen_gt (x,y) () = (x + 1, y +. 1.)
      let gen_lt (x,y) () = (x - 1, y -. 1.)
      let gen_between x y () = None
    end)
    
  module EdgeDict = Dict.Make(
    struct
      type key = node
      type value = NeighborSet.set
      let compare = int_compare
      let string_of_key = string_of_int
      let string_of_value = NeighborSet.string_of_set

      let gen_key () = 0
      let gen_key_gt x () = x + 1
      let gen_key_lt x () = x - 1
      let gen_key_between x y () = None
      let gen_key_random () = Random.int 100
      let gen_value () = NeighborSet.empty
      let gen_pair () = (gen_key (), gen_value ())
    end)
    
  module IntNode = Dict.Make(
    struct 
      type key = int
      type value = node
      let compare = int_compare
      let string_of_key = string_of_int
      let string_of_value = string_of_int
	
      let gen_key () = 0
      let gen_key_gt x () = x + 1
      let gen_key_lt x () = x - 1
      let gen_key_between x y () = None
      let gen_key_random () = Random.int 100
      let gen_value () = gen_key ()
      let gen_pair () = (gen_key (), gen_value ())
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

  let is_empty g = (g.num_nodes = 0)
          
  let add_edge g src dst wt =
    let new_neighbors =
      match EdgeDict.lookup g.edges src with
      | None -> NeighborSet.insert (dst, wt) NeighborSet.empty 
      | Some s -> NeighborSet.insert (dst, wt) s
    in
    let g' = (add_node (add_node g src) dst) in
      {edges = EdgeDict.insert g'.edges src new_neighbors;
       num_nodes = g'.num_nodes;
       index_to_node_map = g'.index_to_node_map}

  let neighbors g node_id : (node * float) list option = 
    let ns =
      match EdgeDict.lookup g.edges node_id with
      | None -> []
      | Some s -> NeighborSet.fold (fun neigh r -> neigh :: r) [] s
    in
    match ns with
    | [] -> None
    | l -> Some l

  let num_nodes g =
    g.num_nodes

  let print_graph g = 
    print_string ("Graph: " ^ (EdgeDict.string_of_dict g.edges))

  let from_edges (es: (node *  float * node) list) : graph =
    List.fold_left (fun g (src, wt, dst) ->
      if src = dst then g else add_edge g src dst wt) (empty 0) es
  
  let run_tests () =
    EdgeDict.run_tests ();
    IntNode.run_tests ();
    let g = from_edges [(0,1.,1); (1, 5., 4); (0, 2., 2); 
			(2, 3., 4); (3, 6., 4); (2, 4., 3)] in
    assert(g.num_nodes = 5);
    
    let g = from_edges [(0,2.,1);(0,3.,2);(1,4.,3);
			(3,5.,4);(3,6.,5);(7,8.,4);
			(6,7.,5);(7,9.,6)] in
    assert (g.num_nodes = 8);
    assert(neighbors g 0 = Some [(2,3.);(1,2.)]);
    assert(neighbors g 1 = Some [(3,4.)]);
    assert(neighbors g 2 = None);
    assert(neighbors g 3 = Some [(5,6.);(4,5.)]);
    assert(neighbors g 4 = None);
    assert(neighbors g 5 = None);
    assert(neighbors g 6 = Some [(5,7.)]);
    assert(neighbors g 7 = Some [(6,9.);(4,8.)]);

end;;

Dictionary.run_tests ();;

(*******************************************************************************)
(***********************     Graph using Matrix    *****************************)
(*******************************************************************************)

module Matrix : GRAPH =
struct
  type adj_mat = float array array
  type graph = { mutable num_nodes : int; (* nodes added *)
		 size : int; (* max nodes *)
		 m : adj_mat} ;;(* cost between nodes *)

  (* seems problematic *) 
  let empty s =
    {num_nodes = 0; size = s; m = Array.create_matrix s s infinity}

  (* Checks if the node n is contained in the graph g. *)
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
      if src = dst then g else
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

Matrix.run_tests ()
