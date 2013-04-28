(* from http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora125.html *)
open Array
  
exception Not_found

(* type  cost  = Nan | Cost of float;; (* Nan is infinitity *)*) 
  
type  adj_mat =  float array array;;
type 'a graph = { mutable ind : int;  (* ind is how many nodes have been added*)
                  size : int; (* max number of nodes *)
                  nodes : 'a array;  (* the actual array of nodes *)
                  m : adj_mat};; (* the costs between any two nodes *)

(*
let print_matrix (g : int graph) : unit = 
  let rec helper_print g (n: int) = 
    if n = g.size then () 
    else (print_string (string_of_float g.m.(n).(n)); helper_print g (n+1)) 
  in helper_print g 0 
*)


(* create an s by s matrix, initialized to infinity. *) 
(* array of nodes initialized to n *)
let create_graph n (s: int) = 
  { ind = 0; size = s; nodes = Array.make s n; 
    m = Array.create_matrix s s infinity } ;;

let neighbors (n: int) (g: int graph) : (int * float) list = 
   (* row of matrix containing n's neighbors is g.m.n *) 
  let rec aux n i neighbor_list = 
    if i = g.size then neighbor_list
     (* put all of the neighbors in a list *) 
    else 
      (match g.m.(n).(i) with 
      | infinity -> aux n (i+1) neighbor_list
      | _ -> aux n (i + 1) ((i, g.m.(n).(i))::neighbor_list))
  in aux n 0 []
  
 (*The function index returns the index of the node n in the graph g. If the node does not exist, a Not_found exception is thrown.*)
 let index n g = 
   let rec aux i = 
     if i >= g.size then raise (Failure "Not_found")
     else if g.nodes.(i) = n then i 
     else aux (i+1)
   in aux 0 ;;

(*The function belongs_to checks if the node n is contained in the graph g. *)
 let belongs_to n g = 
   let rec aux i =
     (i < g.size) & ((g.nodes.(i) = n) or (aux (i+1)))
   in aux 0;;

(*The next two functions are for adding nodes and edges of cost c to graphs.*)
 let add_node n g = 
   if g.ind = g.size then failwith "the graph is full"
   else if belongs_to n g then failwith "the node already exists"
   (* we might not want this to fail but just not include it and keep
      going *)
   else (g.nodes.(g.ind) <- n; g.ind <- g.ind + 1) ;;
 
 (* MAKE THIS NOT TAKE A G *) 
 let add_edge e1 e2 c g = 
   try
     let x = index e1 g and y = index e2 g in 
     g.m.(x).(y) <- c 
   with Not_found -> failwith "node does not exist" ;;

open Graphs

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
