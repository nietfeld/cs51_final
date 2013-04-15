(*
(* Here is where the graph signature and the modules that implement it go *)

module type GRAPH =
sig
  exception GraphEmpty

  (* What's being stored in the graph *)
  type node

  type edge

  (* Represents the neighbors to a given node *) 
  type neighbors 

  (* The graph itself (stores things of type node) *)
  type graph

  (* Returns an empty queue *)
  val empty : graph

  (* Takes a queue, and returns whether or not it is empty *)
  val is_empty : graph -> bool

  (* Takes a node and a graph, returns a new graph *) 
  val add : node -> neighbors -> graph -> graph

  (* Given a node and a graph, returns the neighbors of that node *) 
  val get_neighbors : node -> graph -> neighbors 

  (* Run invariant checks on the implementation of this graph.
   * May raise Assert_failure exception *)
  val run_tests : graph -> unit
end

(* REPLACE C *) 
module AdjacencyList (C : COMPARABLE_AND_GENABLE) : PRIOQUEUE with type node = C.t =
struct
  exception GraphEmpty

  type node = C.t 
  type edge = float
 
  type neighbors = (edge * node) list 
  type graph = (node * neighbors) list

  let empty = []
  
  let is_empty (g: graph) = g = empty  

  (* neighbors must include even neighbors that are not already in graph. *)
  (* List.map *)
  let rec add (n: node) (ns: neighbors) (g: graph) : graph =
    match g with
    | [] -> (n*ns)::g
    | (n1,ns1)::tl as current ->
      match C.compare n n1 with
      | Less | Equal -> (n*ns)::current
      | Greater -> (n1*ns1)::(add n ns tl)

  let rec get_neighbors (n: node) (g: graph) : neighbors =
    (* How to check if the two nodes are the same *) 
    let (n1, neighbor_list) = List.find (fun (h,t) -> h = n) g in 
    neighbor_list

  let run_tests () = ()
end
*)
