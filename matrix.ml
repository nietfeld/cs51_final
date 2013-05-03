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


