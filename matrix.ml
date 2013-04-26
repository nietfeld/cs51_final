(* from http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora125.html *)
open Array

exception Not_found

 (* type  cost  = Nan | Cost of float;; (* Nan is infinitity *)*) 

type  adj_mat =  float array array;;
type 'a graph = { mutable ind : int;  (* ind is how many nodes have been added*)
                  size : int; (* max number of nodes *)
                  nodes : 'a array;  (* the actual array of nodes *)
                  m : adj_mat};; (* the costs between any two nodes *)

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
 
 let add_edge e1 e2 c g = 
   try
     let x = index e1 g and y = index e2 g in 
     g.m.(x).(y) <- c 
   with Not_found -> failwith "node does not exist" ;;


let test_aho () = 
  let g = create_graph "nothing" 5 in 
  List.iter (fun x -> add_node x g) ["A"; "B"; "C"; "D"; "E"];
  List.iter (fun (a,b,c) -> add_edge a b c g) 
    ["A","B",10.;
     "A","D",30.;
     "A","E",100.0;
     "B","C",50.;
     "C","E",10.;
     "D","C",20.;
     "D","E",60.];
  for i=0 to g.ind -1 do g.m.(i).(i) <- 0.0 done;
  g;;

let a = test_aho();;

(*

val a : string graph =
  {ind=5; size=5; nodes=[|"A"; "B"; "C"; "D"; "E"|];
   m=[|[|Cost 0; Cost 10; Nan; Cost 30; Cost ...|]; ...|]} *)

module Matrix (s: int) : GRAPH =
struct
  type node = int
    
  type adj_mat = float array array
  type graph = { mutable ind : int; (* nodes added *)
		 size : int; (* max nodes *)
		 nodes : node array ;
		 m : adj_mat} (* cost between nodes *)
    
  let empty =
    {ind = 0; size = s; nodes = Array.make s 0;
     m = Array.create_matrix s s infinity}
      
  let nodes g = 
    let rec arraytolist nodes i currentlist =
      if i = g.ind then currentlist
      else Array.get nodes i :: (arraytolist (i + 1) currentlist)
    in
    arraytolist g.nodes 0 []
      
  let is_empty g = g = empty
    
  (*The function belongs_to checks if the node n is contained in the graph g. *)

  let add_node g n =
    if g.ind = g.size then failwith "the graph is full"
    else if has_node n g then failwith "the node already exists"
    (* we might not want this to fail but just not include it and keep
       going *)
    else (g.nodes.(g.ind) <- n; g.ind <- g.ind + 1) ;;
  
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
      let x = index g e1 and y = index e2 g in 
      g.m.(x).(y) <- c 
    with Not_found -> failwith "node does not exist" ;;
  


  (* Return None if node isn't in the graph *)
  val neighbors g n =
    (* row of matrix containing n's neighbors is g.m.n *) 
    let rec aux n i neighbor_list = 
      if i = g.size then neighbor_list
      (* put all of the neighbors in a list *) 
      else 
	(match g.m.(n).(i) with 
	| infinity -> aux n (i+1) neighbor_list
	| _ -> aux n (i + 1) ((i, g.m.(n).(i))::neighbor_list))
    in
    let list = aux n 0 [] in
    match list with
    | [] -> None
    | _ -> Some list
      

  (* Return None if node isn't in the graph *)
  (* val outgoing_edges : graph -> node -> (node * float * node) list option*)
      
  let has_node g n =
    let rec aux i =
      (i < g.size) & ((g.nodes.(i) = n) or (aux (i+1)))
    in aux 0;;
  
  val num_nodes g =
    g.ind
      
  val string_of_graph : graph -> string
    
  val from_edges : (node * float * node) list -> graph
    
end

