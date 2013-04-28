(* CYCLES?
WHAT DO THEN?
CYCLEEEES? *)

open Prio_q
open Array
open Graphs

open Graph
open ListQueue

exception Not_found
exception QueueEmpty

(* Need to finish implementing prioq *) 
module IntListQueue =  ListQueue
module IntHeapQueue =  BinaryHeap
module My_graph = Matrix

(* right now -- would have to change this when we test *)
module My_queue = IntListQueue

(* FIX CURR_NODE TO BETTER TYPE *) 
let initialize_queue (n: int) start =
  let rec add_elts pq (to_add: int) =
    if to_add = (-1) then My_queue.add ({id = start; tent_dist = 0.}) (My_queue.delete start pq)
    else add_elts (My_queue.add {id = to_add; tent_dist = infinity} pq) 
      (to_add - 1)
  (* for starting node, we want the id and dist 0. *) 
  in (add_elts My_queue.empty (n-1));;

let rec update_queue pq (curr_node: int*float) neighbor_list dist prev = 
  let (node_id, distance_from_start) = curr_node in 
  match neighbor_list with 
  | None | Some [] -> pq
  | Some ((n,e)::tl) -> 
    (match Array.get dist n with
    | infinity ->  
      let {id=k; tent_dist=d} = My_queue.lookup n pq in
      let new_dist = e +. distance_from_start in 
      if new_dist < d then 
	(Array.set prev n node_id;
	 let new_pq =
	   My_queue.update k new_dist pq in 
	 update_queue new_pq curr_node (Some tl) dist prev)
      (* don't update, do next neighbor *) 
      else update_queue pq curr_node (Some tl) dist prev)

let one_round (pq : queue) (my_graph : graph) (dist : float array) 
    (prev : int array) : queue = 
  let (curr_node, new_q) = My_queue.take pq in 
  let neighbor_list = My_graph.neighbors my_graph curr_node.id in 
  print_string "right before array set" ; print_string (string_of_int curr_node.id); print_string "\n";
  Array.set dist curr_node.id curr_node.tent_dist; (* update dist array*)
  print_string "Finished array set \n";
  update_queue new_q (curr_node.id, curr_node.tent_dist) neighbor_list dist prev


(*
let print_results (dist : float array) (prev: int array) (graph_size: int) (start_node: int) : unit =
  print_string "I'm printing this out now!!\n";
  let rec helper_dist (dist: float array) (n: int) =
    if n = graph_size then Printf.printf "Done!"
    else (print_string ((string_of_int start_node)^"->"^(string_of_int n)^"("^(string_of_float (Array.get dist n))^ ") \n");
	  helper_dist dist (n+1))
  in
  helper_dist dist 0  *)


let rec reconstruct_help (end_node: int) (start_node : int) (prev: int array) : string =
  if start_node = end_node then "I'm done"
  else "Ya"
(*
   (Array.get prev end_node)*)



let print_results (dist : float array) (prev: int array) (graph_size: int) (start_node: int) : unit =
  print_string "I'm printing this out now!!\n";
  let rec helper_dist (dist: float array) (n: int) =
    if n = graph_size then Printf.printf "Done!"
    else (print_string ((string_of_int start_node)^"->"^(* here is where we would print out the in the middle *)(reconstruct_help n start_node prev)^"("^(string_of_float (Array.get dist n))^ ") \n");
	  helper_dist dist (n+1))
  in
  helper_dist dist 0 





let dij (start : node) (g : graph) (pq : queue) =
  (* make a prioq with all nodes in graph set to infinity *) 
  if has_node g start then 
    let graph_size = My_graph.num_nodes g in
    (* make distance and prev arrays *)
    (* write way to have them all be in scope *) 
    let dist = Array.make graph_size infinity in 
    let prev = Array.make graph_size (-1) in 
    let prioq = initialize_queue graph_size start in 
    Printf.printf "I'm here and done initializing q \n";
    (* we want to do an infinite loop and then catch an exception, but instead we'll just loop through *) 
    let rec iterate (pq : queue) (number_rounds: int) : unit = 
      match number_rounds with 
      | (-1) -> Printf.printf "Finished bitches \n" 
      | _ -> let new_q = one_round pq g dist prev in 
             iterate new_q (number_rounds-1) 
    in iterate prioq (graph_size-1);
      print_results dist prev graph_size start
  else failwith "dij: node unknown";; 

(*
let g = My_graph.from_edges [(1,1.,2);(1,7.,3);(1,4.,5);(2,5.,3);(2,2.,5);
		    (5,3.,4);(4,6.,3)];; *)

(*
let g = My_graph.from_edges [(0,1.,1); (0,2.,2)];;

let pq = My_queue.empty

let r = dij 0 g pq;;

this one is working fine
*)

let g = My_graph.from_edges [(0,1.,1); (1, 5., 4); (0, 2., 2); 
			     (2, 3., 4); (3, 6., 4); (2, 4., 3)];;
let pq = My_queue.empty

let r = dij 0 g pq;;
