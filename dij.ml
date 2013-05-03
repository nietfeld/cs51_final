open Prio_q
open Array
open Graphs 
open Matrix
open Dictionary

open ListQueue 
module IntListQueue =  ListQueue


open Graphs
open Dictionary

(*
open BinaryHeap 
module IntHeapQueue = BinaryHeap *)

exception Not_found
exception QueueEmpty
  
(* SPECIFY AND THE GRAPH AND Q BEING USED *)
module My_graph = Dictionary
module My_queue = IntListQueue
  
let initialize_queue (n: int) (start: node) =
  let rec add_elts (pq: queue) (to_add: int) = 
    if to_add = (-1) then My_queue.update start 0. pq
    else add_elts (My_queue.add {id = to_add; tent_dist = infinity} pq) 
      (to_add - 1)
  in (add_elts (My_queue.empty ()) (n-1))

let rec update_queue pq (curr_node: int*float) neighbor_list dist prev = 
  let (node_id, distance_from_start) = curr_node in 
  match neighbor_list with 
  | None | Some [] -> pq
  | Some ((n,e)::tl) -> 
    (match Array.get dist n with
    | infinity ->   
      (match (My_queue.lookup n pq) with
      | None ->  print_string "not returuning anything \n"; pq
      | Some {id=k; tent_dist=d} -> 
	(let new_dist = e +. distance_from_start in 
	 print_string ("N: "^(string_of_int n)^"K: "^(string_of_int k)^"D:  "^
			  (string_of_float d));
	 print_string ("THIS IS THE NEW DIST:"^(string_of_float new_dist)^"\n");
	 if new_dist < d then 
	   (Array.set prev n (Some node_id); 
	    let new_pq =
	      My_queue.update n new_dist pq in 
	    My_queue.print_q new_pq;
	    update_queue new_pq curr_node (Some tl) dist prev)
	 (* don't update, do next neighbor *) 
	 else update_queue pq curr_node (Some tl) dist prev)))
      
let one_round (pq : queue) (my_graph : graph) (dist : float array) 
    (prev : int option array) : queue = 
  let (curr_node, new_q) = My_queue.take pq in 
  let neighbor_list = My_graph.neighbors my_graph curr_node.id in 
  print_string "right before array set" ;
  print_string (string_of_int curr_node.id); print_string "\n";
  Array.set dist curr_node.id curr_node.tent_dist; (* update dist array*)
  print_string "Finished array set \n";
  update_queue new_q (curr_node.id, curr_node.tent_dist) neighbor_list dist prev
    

let deopt (x: int option) : int =
  match x with
  | None -> raise (Failure "Impossible")
  | Some int -> int


(* helper functions for printing out the result *)
let rec reconstruct_help (end_node: int) (start_node : int)
    (prev: int option array) : string =
  (* if start_node = end_node then "" (* meow *)
     else *)
  let last = (Array.get prev end_node) in
  if (last = None || start_node = deopt last) then ""
  else ("->"^(string_of_int (deopt last))
	^(reconstruct_help (deopt last) start_node prev))
    
(* just some debugging functions *)
let deopt_p (x: int option) : string =
  match x with
  | None -> "_"
  | Some int -> string_of_int int
    
let print_dist_array arr =
  print_string ("Prev array:"^ (List.fold_left (fun x y -> (string_of_float y)^x) 
				  "" (Array.to_list arr))^"\n")

let print_prev_array arr =
  print_string ("Dist array:"^(List.fold_left (fun x y -> (deopt_p y)^x) ""
				 (Array.to_list arr))^"\n")


(* this printing function prints backwards???? *)
let print_results (dist : float array) (prev: int option array) (graph_size: int)
    (start_node: int) : unit =
  print_prev_array prev;
  print_dist_array dist;
  print_string "\n Done \n";
  let rec helper_dist (dist: float array) (n: int) =
    if n = graph_size then Printf.printf "Done!"
    else (print_string ((string_of_int start_node)^
			   (reconstruct_help n start_node prev)^"->"^
			   (string_of_int n)^"("^
			   (string_of_float (Array.get dist n))^ ") \n");
	  helper_dist dist (n+1))
  in
  helper_dist dist 0



let dij (start : node) (g : graph) =
  if has_node g start then 
    let graph_size = My_graph.num_nodes g in
    let dist = Array.make graph_size infinity in 
    let prev = Array.make graph_size (None) in 
    let prioq = initialize_queue graph_size start in 
    Printf.printf "I'm here and done initializing q \n";
    (* we want to do an infinite loop and then catch an exception, but instead we'll just loop through *) 
    let rec iterate (pq : queue) (number_rounds: int) : unit = 
      match number_rounds with 
      | 0 -> Printf.printf "Finished bitches \n"
      | _ -> let new_q = one_round pq g dist prev in 
	     iterate new_q (number_rounds-1) 
    in iterate prioq graph_size; (* used to be -1 *)
    print_results dist prev (graph_size) start;
    (dist,prev) (* return this for testing *)
  else failwith "dij: node unknown";; 

let exe_time f g ss =
  let t0 = Sys.time() in
  Printf.printf "Start (%5.5f)\n" t0;
  f g ss;
  let t1 = Sys.time() in
  Printf.printf "End (%5.5f)\n" t1;
  Printf.printf "Duration = (%5.5f)\n" (t1 -. t0) ;;


(* the array being printed here are simpl in reverse order *)
let run_tests () =

  let pq = My_queue.empty () in
  let g = My_graph.from_edges [(0,1.,1); (0,2.,2)] in
  let (dist,prev) = dij 0 g  in
  let prev_array = 
    List.fold_left (fun x y -> (deopt_p y)^x) "" (Array.to_list prev) in
  let dist_array =
    List.fold_left (fun x y -> (string_of_float y)^x) "" (Array.to_list dist) in  
  let g1 = My_graph.from_edges [(0,1.,1); (1, 5., 4); (0, 2., 2); 
				(2, 3., 4); (3, 6., 4); (2, 4., 3)] in
  let (dist_1, prev_1) = dij 2 g1 in
  let prev_array_1 = 
    List.fold_left (fun x y -> (deopt_p y)^x) "" (Array.to_list prev_1) in
  let dist_array_1 =
    List.fold_left (fun x y -> (string_of_float y)^x) "" (Array.to_list dist_1) in 
  
  let g2 = 
    My_graph.from_edges [(0,1.1,1); (1, 2.1, 2); (2, 3.1, 3); (4, 6.1, 3);
			 (3, 4.1, 1); (0, 5.1, 3); (1, 8.1, 5); (4, 7.1, 5)] in
  let (dist_2, prev_2) = dij 0 g2 in
  let prev_array_2 =  (List.fold_left (fun x y -> (deopt_p y)^x) "" (Array.to_list prev_2)) in
  let dist_array_2 = (List.fold_left (fun x y -> (string_of_float y)^x) "" (Array.to_list dist_2)) in 

let g3 =
    My_graph.from_edges [(0, 2.2, 1);(0, 4.2, 2);(2, 1.2, 4);(4, 2.2, 6);
			 (6, 4.2, 5);(3, 11.2, 4);(3, 7.2, 5);(2, 3.2, 5);
			 (1, 5.2, 3); (0, 1.2, 3); (3, 0.2, 1)] in
  let (dist_3, prev_3) = dij 3 g3 in
  let prev_array_3 = 
    List.fold_left (fun x y -> (deopt_p y)^x) "" (Array.to_list prev_3) in
  let dist_array_3 =
    List.fold_left (fun x y -> (string_of_float y)^x) "" (Array.to_list dist_3) in  

  assert (prev_array = "00_"); 
  assert (dist_array = "2.1.0.");
  assert (prev_array_1 = "22___"); 
  assert (dist_array_1 = "3.4.0.infinf");
  assert (prev_array_2 = "1_010_");
  assert (dist_array_2 = "9.2inf5.13.21.10." );
  assert (print_string prev_array_3 = ()); 
  assert (prev_array_3 = "433__3_");
  assert (dist_array_3 = "13.47.211.20.inf0.2inf") 
;;

run_tests ();

(* here is where we could do the timing functions *)

  let g3 = My_graph.from_edges [(0, 2.2, 1);(0, 4.2, 2);(2, 1.2, 4);(4, 2.2, 6);(6, 4.2, 5);
				(3, 11.2, 4);(3, 7.2, 5);(2, 3.2, 5); (1, 5.2, 3); (0, 1.2, 3); (3, 0.2, 1)] in
  exe_time dij 3 g3;;



 
let g3 = 
  My_graph.from_edges [(0, 2.2, 1);(0, 4.2, 2);(2, 1.2, 4);(4, 2.2, 6);
		       (6, 4.2, 5);(3, 11.2, 4);(3, 7.2, 5);(2, 3.2, 5);
		       (1, 5.2, 3);(0, 1.2, 3); (3, 0.2, 1)] in
  exe_time dij 3 g3;;


(* this one still works fine *)
let g5 = My_graph.from_edges [(0, 6.2, 1);(1, 7.1, 2);(2, 8.4, 3);(3, 6.3, 4);(4, 7.3, 5);(6, 6.7, 5);(7, 11.4, 6);(8, 6.1, 4);(7, 5.6, 5);(4, 2.8, 9);(9, 3.2, 10);(10, 1.9, 11);(11, 11.1, 0);(0, 7.4, 11)]
in 
dij 0 g5;;



let g6 = My_graph.from_edges [(0, 6.2, 1);(1, 7.1, 2);(2, 8.4, 3);(3, 6.3, 4);(4, 7.3, 5);(6, 6.7, 5);(7, 11.4, 6);(8, 6.1, 4);(7, 5.6, 5);(4, 2.8, 9);(9, 3.2, 10);(10, 1.9, 11);(11, 11.1, 0);(0, 7.4, 11);(1, 1.2,0);(2, 11.5, 12);(7, 3.4, 8);(10, 0.2, 13);(13, 0.3, 11);(2, 23.5, 8)]
in 
dij 1 g6;;


(*
 let course_graph = My_graph.from_edges [(0,1.05,1);(0,1.74,2);(0,2.0,3);(0,1.15,4);(0,2.08,11);(0,1.03,12);(0,1.57,13);
(0,1.2,14);(0,1.42,15);(1,2.0,3);(1,1.15,4);(1,0.36,7);(2,1.15,4);(1,2.08,11);(1,1.03,12);
(1,1.57,13);(1,1.2,14);(1,1.42,15);(3,1.43,5);(3,1.42,15);(3,1.03,12);(2,0.36,7);(2,2.08,11);
(2,1.03,12);(2,1.57,13);(2,1.2,14);(2,1.42,15);(4,1.43,5);(5,1.65,6);(7,1.65,6);(7,0.32,8);
(4,1.03,12);(5,1.03,12);(6,1.18,9);(4,1.42,15);(5,1.42,15);(6,1.03,12);(6,1.42,15);(8,1.65,6);
(9,1.03,12);(9,1.42,15);(8,1.18,9);(13,0.69,10);(8,1.03,12);(8,1.42,15);(7,1.03,12);(7,1.42,15);
(12,1.42,15);(10,1.03,12);(10,1.42,15);(11,1.03,12);(11,1.42,15);(13,1.03,12);(13,1.42,15);
(14,0.69,10);(14,1.03,12);(14,1.42,15);(15,1.03,12);(15,1.42,15)] in
  print_string "RUNNINGINGINGIGNG \n\n\n\n";
  let (dist,prev) =  dij 0 course_graph in
  print_string "ya done running yall";
  let prev_array =  (List.fold_left (fun x y -> (deopt_p y)^x) "" (Array.to_list prev)) in
  let dist_array = (List.fold_left (fun x y -> (string_of_float y)^x) "" (Array.to_list dist)) in 
  assert (1=1);;

*)
