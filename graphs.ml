open Order

module type NODE = 
sig 
  type node
  val compare : node -> node -> Order.order 
  val string_of_node : node -> string
  val gen : unit -> node 
 (* val from_edges : (node * float * node) list -> graph *)
end

(* A signature for directed graphs with unweighted edges *)
module type GRAPH =
sig 
  module N : NODE
  type node = N.node
  type graph
    
  val empty : graph

  val nodes : graph -> node list

  val is_empty : graph -> bool
    
  val add_node : graph -> node -> graph

  (* Adds the nodes if they aren't already present. *)
  (* and the weight of that edge *)
  val add_edge : graph -> node -> node -> float -> graph
    
  (* Return None if node isn't in the graph *)
  val neighbors : graph -> node -> (node * float) list option

  (* Return None if node isn't in the graph *)
  val outgoing_edges : graph -> node -> (node * float * node) list option
    
  val has_node : graph -> node -> bool

  (* Return None if the graph is empty *)
  val get_random_node : graph -> node option

  val string_of_graph : graph -> string

end
   
module Graph (NA: NODE) : (GRAPH with module N = NA) =
struct
  open Order
  module N = NA
  type node = N.node
    
  (* We'll represent a graph as an edge dictionary:
     dictionary: node -> neighbor set
     Every node in the graph must be a key in the dictionary.
  *)
    
(* not sure which one is correct here because of the merge *)
  module NeighborSet = Myset.Make(
     struct
        type t = N.node * float
        let compare (n1, w1) (n2, w2) = N.compare n1 n2
(*
        let string_of_t (n, w) = N.string_of_node n ^ ", " ^ string_of_int w
        let gen () = (N.gen (), 1)
        let gen_random () = (N.gen (), 1)
        let gen_gt x () = (N.gen (), 2)
        let gen_lt x () = (N.gen (), 0)
*)
        let string_of_t (n, w) = N.string_of_node n ^ ", " ^ string_of_float w
        let gen () = (N.gen (), 1.)
        let gen_random () = (N.gen (), 1.)
        let gen_gt x () = (N.gen (), 2.)
        let gen_lt x () = (N.gen (), 0.)
(*
        type t = node * float
        let compare = N.compare
        let string_of_t = N.string_of_node
        let gen = N.gen
        let gen_random = N.gen
        let gen_gt x () = N.gen ()
        let gen_lt x () = N.gen ()
*)
        let gen_between x y () = None
      end)
    
  module EdgeDict = Dict.Make(
    struct
      type key = node
      type value = NeighborSet.set
      let compare = N.compare 
      let string_of_key = N.string_of_node
      let string_of_value ns = NeighborSet.string_of_set ns
      let gen_key = N.gen
      let gen_key_random = N.gen
      let gen_key_gt x () = N.gen ()
      let gen_key_lt x () = N.gen ()
      let gen_key_between x y () = None
      let gen_value () = NeighborSet.empty
      let gen_pair () = (gen_key(),gen_value())
    end)
    
  module IntNode = Dict.Make(
    struct 
      type key = int
      type value = node
      let compare = int_compare
      let string_of_key = string_of_int
      let string_of_value = N.string_of_node
      let gen_key () = 0
      let gen_key_random () = 0
      let gen_key_gt x () = 1
      let gen_key_lt x () = (-1)
      let gen_key_between x y () = None
      let gen_value = N.gen
      let gen_pair () = (gen_key(),gen_value())
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
          
  let outgoing_edges g src : (node * float * node) list option = 
    match EdgeDict.lookup g.edges src with
      | None -> None
      | Some s -> Some (NeighborSet.fold (fun (dst, wt) r -> 
                                             (src, wt, dst) :: r) [] s)
  let has_node g n = 
    match EdgeDict.lookup g.edges n with
      | None -> false
      | _ -> true

  let get_random_node g = 
    if g.num_nodes = 0 then None else
      let r = Random.int (g.num_nodes) in
        IntNode.lookup g.index_to_node_map r

  let string_of_graph g = 
    "Graph: " ^ (EdgeDict.string_of_dict g.edges)

end

(* implement the module graph and test it *)


module NamedGraph = 
struct
  include(Graph(struct
                  type node = string
                  let compare = Order.string_compare
                  let string_of_node = fun x -> x
                  let gen () = ""
                end))
  let from_edges (es: (string *  float * string) list) : graph =
    List.fold_left (fun g (src, wt, dst) -> add_edge g src dst wt) empty es
end

module TestGraph = 
struct 
  module G = NamedGraph

  let g = G.add_edge G.empty "a" "b" 3.;;
  let g2 = G.add_edge g "a" "c" 4.;;

  let deopt_len lo =
    match lo with
      | None -> 0
      | Some xs -> List.length xs;;

  let deopt_lst lo =
    match lo with
      | None -> []
      | Some xs -> xs;;

  let deopt_node no =
    match no with
      | None -> "None"
      | Some n -> n;;

  let _ = (
    assert (G.has_node g "a");
    assert (G.has_node g "b");
    assert (G.has_node g "c" = false);
    assert (G.has_node g2 "c");
    assert (G.has_node g2 "d" = false);

    assert (List.length (G.nodes G.empty) = 0) ;
    assert (List.length (G.nodes (G.add_node G.empty "a")) = 1) ;

    assert (List.length (G.nodes g) = 2) ;

    assert (List.length (G.nodes g2) = 3) ;

    assert (deopt_len (G.outgoing_edges g2 "a") = 2) ;
    assert (deopt_len (G.outgoing_edges g2 "b") = 0) ;
    assert (deopt_len (G.outgoing_edges g2 "c") = 0) ;
    assert (G.outgoing_edges g2 "d" = None) ;

    assert (deopt_len (G.neighbors g2 "a") = 2) ;
    assert (deopt_len (G.neighbors g2 "b") = 0) ;
    assert (deopt_len (G.neighbors g2 "c") = 0) ;
    assert (G.neighbors g2 "d" = None) ;

    assert (let t = deopt_lst (G.neighbors g2 "a") in
              t = [("b", 3.);("c",4.)] or t = [("c",4.);("b",3.)]) )

  let g3 = G.from_edges [("a",2.,"b");("a",3.,"c");("b",4.,"d");
		       ("d",5.,"e");("d",6.,"f");("h",8.,"e");
		       ("g",7.,"f");("h",9.,"g")] ;;

  assert(let a = deopt_lst (G.neighbors g3 "a") in 
	 a = [("b",2.);("c",3.)] or a = [("c",3.);("b",2.)]) ;
  assert(deopt_lst (G.neighbors g3 "b") = [("d",4.)]) ;
  assert(deopt_lst (G.neighbors g3 "c") = []) ;
  assert(let d = deopt_lst (G.neighbors g3 "d") in 
	 d = [("e",5.);("f",6.)] or d = [("f",6.);("e",5.)]) ;
  assert(deopt_lst (G.neighbors g3 "e") = []) ;
  assert(deopt_lst (G.neighbors g3 "f") = []) ;
  assert(deopt_lst (G.neighbors g3 "g") = [("f",7.)]) ;
  assert(let h = deopt_lst (G.neighbors g3 "h") in 
	 h = [("e",8.);("g",9.)] or h = [("g",9.);("e",8.)]) 

  let tester oelist : unit =
    match oelist with
    | None -> print_string ""
    | Some s -> print_string (List.fold_right (fun (dst, wt, src) y -> 
      dst ^ " & " ^ string_of_float wt ^ " & " ^ src ^ " ; " ^ y) s "")
;;

(* Tested by printing out all graph edges and checking manually.

assert(tester (G.outgoing_edges g3 "a") = ()) ;
assert(tester (G.outgoing_edges g3 "b") = ()) ;
assert(tester (G.outgoing_edges g3 "c") = ()) ;
assert(tester (G.outgoing_edges g3 "d") = ()) ;
assert(tester (G.outgoing_edges g3 "e") = ()) ;
assert(tester (G.outgoing_edges g3 "f") = ()) ;
assert(tester (G.outgoing_edges g3 "g") = ()) ;
assert(tester (G.outgoing_edges g3 "h") = ())
*)
end

module IdGraph = (Graph(struct
  type node = int
  let compare = Order.int_compare
  let string_of_node = string_of_int
  let gen () = 0
(*  let from_edges (es: (node *  float * node) list) : graph =
    List.fold_left (fun g (src, wt, dst) -> add_edge g src dst wt) empty es *)
end))


