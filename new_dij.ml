(* CYCLES?
WHAT DO THEN?
CYCLEEEES? *)



open Prio_q
open Array
open Graphs


open Graph
open BinaryHeap 

exception Not_found
exception QueueEmpty

(* Need to finish implementing prioq *) 
module IntListQueue =  ListQueue
module IntHeapQueue =  BinaryHeap
module My_graph = Graph

(* right now -- would have to change this when we test *)
module My_queue = IntHeapQueue

let delete_min pq =
  try 
    My_queue.take pq
  with QueueEmpty -> failwith "done" ;; 
(* this will prob be something very diff *)


(* FIX CURR_NODE TO BETTER TYPE *) 
let initialize_queue (n: int) start =
  let rec add_nodes pq (to_add: int) =
    if to_add = 0 then pq 
    else add_nodes (My_queue.add {id = to_add; tent_dist = infinity} pq) 
      (to_add - 1)
  (* for starting node, we want the id and dist 0. *) 
  in print_string "initialize queue";
  add_nodes (My_queue.add {id = start; tent_dist = 0.} My_queue.empty) (n-1)
 (* add the starting node with dist 0*) 


let rec update_queue pq (curr_node: int*float) neighbor_list dist prev = 
  let (node_id, distance_from_start) = curr_node in 
  print_string "update queue \n";
  match neighbor_list with 
  | None | Some [] -> pq
  | Some ((n,e)::tl) -> 
    print_string "in update queue \n" ;
    (* assume look up k is a function into index k of the array *) 
    (match Array.get dist n with (* CHANGE TO NEW DISTANCE NOTATION *) 
    | infinity -> 
      print_string "Array.get is broken";
      (* look up id n in prioq- is there a way to only do this way? *) 
      let {id=k; tent_dist=d} = My_queue.lookup n pq in 
      let new_dist = e +. distance_from_start in 
      if new_dist < d then 
	(print_string "it's array.set" ;
	 Array.set prev k n;
	 (* update priority q NEED UPDATE KEY METHOD!!!!! let new_pq =
	    My_queue.update_key k new_dist in *)
	 let new_pq = pq in 
	 print_string "done with update \n" ;
	 update_queue new_pq curr_node (Some tl) dist prev)
      (* don't update, do next neighbor *) 
      else update_queue pq curr_node (Some tl) dist prev
    | _ -> print_string "We need a new match statement in update queue" )


let one_round (pq : queue) (my_graph : graph) (dist_array : float array) 
    (prev : int array) : queue = 
  let (curr_node, new_q) = delete_min pq in 
  let neighbor_list = My_graph.neighbors my_graph curr_node.id in 
  (* for more general elements, use node int compare to get string to
     an int *)
  print_string "one_round "; 
  Array.set dist_array curr_node.id curr_node.tent_dist; (* update dist array*)
  update_queue pq (curr_node.id, curr_node.tent_dist) neighbor_list dist_array prev


let print_result graph_size dist prev = 
  let rec print_dist (i: int) dist = 
    if i = graph_size then Printf.printf "distances done" 
    else print_string (string_of_float (dist.(i)));
         print_dist (i+1) dist in 
  print_string "printing bitch " ; 
  (* let rec print_path (i: int) (s: int) prev = 
    print_string "shortest path from"^(string_of_float i);*) 
    print_dist graph_size dist 

let dij (start : node) (g : graph) (pq : queue) =
  (* make a prioq with all nodes in graph set to infinity *) 
  if has_node g start then 
  let graph_size = My_graph.num_nodes g in
    (* make distance and prev arrays *)
    (* write way to have them all be in scope *) 
  let dist = Array.make graph_size infinity in 
  let prev = Array.make graph_size (-1) in 
  let prioq = initialize_queue graph_size start in 
    (* we want to do an infinite loop and then catch an exception, but instead we'll just loop through *) 
  let rec iterate (pq : queue) (n : node) : unit = 
    match n with 
    | 0 -> Printf.printf "Finished bitches" 
    | _ -> let new_q = one_round pq g dist prev in 
           iterate new_q (n-1) (* make this tail recursive maybe? *)
  in print_string "dij"; 
  iterate prioq graph_size
  (*print_result graph_size dist prev;*)
 else failwith "dij: node unknown";; 



(* displaying the results here *)
(* Will update for better display *) 
 (*let display_state f (g,st)  dest = 
   if belongs_to dest g then 
      let d = index dest g in
        let rec aux is = 
           if is = st.source then Printf.printf "%a"  f g.nodes.(is)
           else (
             let old = st.paths.(is) in 
             aux old;
             Printf.printf " -> (%4.1f) %a" (float_of_cost g.m.(old).(is))
                                          f g.nodes.(is)  
           )
        in 
          if not(a_cost st.distances.(d)) then Printf.printf "no way\n"
          else (
            aux d;
            Printf.printf " = %4.1f\n" (float_of_cost st.distances.(d)));;
 *)




let g = My_graph.from_edges [(1,1.,2);(1,7.,3);(1,4.,5);(2,5.,3);(2,2.,5);
		    (5,3.,4);(4,6.,3)];;

let pq = My_queue.empty

let r = dij 1 g pq;;


(*
display_state (fun x y -> Printf.printf "%s!" y) (a,r) "E";; 
(* this is what should return *)
*)
