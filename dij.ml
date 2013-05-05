open Prio_q
open Array
open Graphs 

(* SPECIFY AND THE GRAPH AND Q BEING USED *)
(* possibilities for what we could substitute *)
module My_graph = Dictionary
module My_queue = BinSQueue

(* create a priority queue tracking all of the nodes in the graph *) 
let initialize_queue (n: int) (start: node) =
  let rec add_elts pq (to_add: int) = 
    if to_add = (-1) then My_queue.update start 0. pq
    else add_elts (My_queue.add {id = to_add; tent_dist = infinity} pq) 
      (to_add - 1)
  in (add_elts (My_queue.empty ()) (n-1))

(* check if we've found a shorted distance to any neighbor *) 
let rec update_queue pq (curr_node: int*float) neighbor_list dist prev = 
  let (node_id, distance_from_start) = curr_node in 
  match neighbor_list with 
  | None | Some [] -> pq
  | Some ((n,e)::tl) -> 
    if dist.(n) = infinity then
      (match (My_queue.lookup n pq) with
      | None -> pq
      | Some {id=k; tent_dist=d} -> 
	(let new_dist = e +. distance_from_start in 
	 if new_dist < d then 
	   (prev.(n) <- (Some node_id); 
	    let new_pq =
	      My_queue.update n new_dist pq in 
	    update_queue new_pq curr_node (Some tl) dist prev)
	 else update_queue pq curr_node (Some tl) dist prev))
      else update_queue pq curr_node (Some tl) dist prev

(* runs dijkstra's on one node *) 
let one_round pq my_graph (dist : float array) 
    (prev : int option array) = 
  let (curr_node, new_q) = My_queue.take pq in 
  let neighbor_list = My_graph.neighbors my_graph curr_node.id in 
  dist.(curr_node.id) <- curr_node.tent_dist; 
  update_queue new_q (curr_node.id, curr_node.tent_dist) neighbor_list dist prev
    

let deopt (x: int option) : int =
  match x with
  | None -> raise (Failure "Impossible")
  | Some int -> int


(* helper functions for printing out the result *)
let rec reconstruct_help (end_node: int) (start_node : int)
    (prev: int option array) : string =
  let last = (Array.get prev end_node) in
  if (last = None || start_node = deopt last) then ""
  else ("->"^(string_of_int (deopt last))
	^(reconstruct_help (deopt last) start_node prev))
    
(* just some debugging functions *)
let deopt_p (x: int option) : string =
  match x with
  | None -> "_"
  | Some i -> string_of_int i
    
let print_dist_array arr =
  print_string ("Dist array:"^(List.fold_left (fun x y -> (string_of_float y)^x) 
				  "" (Array.to_list arr))^"\n")

let print_prev_array arr =
  print_string ("Prev array:"^(List.fold_left (fun x y -> (deopt_p y)^x) ""
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
			   (string_of_float (dist.(n))^ ") \n");
			helper_dist dist (n+1)))
  in
  helper_dist dist 0

(* Run dijkstra's over the wholegraph *) 
let dij (start: node) g =
    let graph_size = My_graph.num_nodes g in
    let dist = Array.make graph_size infinity in 
    let prev = Array.make graph_size (None) in 
    let prioq = initialize_queue graph_size start in 
    let rec iterate pq (number_rounds: int) : unit = 
      match number_rounds with 
      | 0 -> ()
      | _ -> let new_q = one_round pq g dist prev in 
	     iterate new_q (number_rounds-1) 
    in iterate prioq graph_size; 
    print_results dist prev (graph_size) start;
    (dist,prev) (* return this for testing *)
 
let exe_time f g ss =
  let t0 = Sys.time() in
  Printf.printf "Start (%5.5f)\n" t0;
  f g ss;
  let t1 = Sys.time() in
  Printf.printf "End (%5.5f)\n" t1;
  Printf.printf "Duration = (%5.5f)\n" (t1 -. t0) ;;


(* the arrays being printed here are simple in reverse order *)
let run_tests () =
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

  let g4 = My_graph.from_edges [(0, 6.2, 1);(1, 7.1, 2);(2, 8.4, 3);(3, 6.3, 4);
				(4, 7.3, 5);(6, 6.7, 5);(7, 11.4, 6);(8, 6.1, 4);
				(7, 5.6, 5);(4, 2.8, 9);(9, 3.2, 10);(10, 1.9, 11);
				(11, 11.1, 0);(0, 7.4, 11)]
  in 
  let (dist_4, prev_4) = dij 0 g4 in
  let prev_array_4 = 
    List.fold_left (fun x y -> (deopt_p y)^x) "" (Array.to_list prev_4) in
  let dist_array_4 =
    List.fold_left (fun x y -> (string_of_float y)^x) "" (Array.to_list dist_4) in  

  let g5 = My_graph.from_edges [(0, 6.2, 1);(1, 7.1, 2);(2, 8.4, 3);
				(3, 6.3, 4);(4, 7.3, 5);(6, 6.7, 5);(7, 11.4, 6);(8, 6.1, 4);
				(7, 5.6, 5);(4, 2.8, 9);(9, 3.2, 10);(10, 1.9, 11);(11, 11.1, 0);
				(0, 7.4, 11);(1, 1.2,0);(2, 11.5, 12);(7, 3.4, 8);
				(10, 0.2, 13);(13, 0.3, 11);(2, 23.5, 8)] 

  in 
  let (dist_5, prev_5) = dij 1 g5 in

  let prev_array_5 =
    List.fold_left (fun x y -> (deopt_p y)^x) "" (Array.to_list prev_5) in
  let dist_array_5 =
    List.fold_left (fun x y -> (string_of_float y)^x) "" (Array.to_list dist_5) in  

  print_string "\n This is the courses graph: \n";
  let course_graph = My_graph.from_edges 
    [(0,1.05,1);(0,1.74,2);(0,2.0,3);(0,1.15,4);(0,2.08,11);(0,1.03,12);
     (0,1.57,13);(0,1.2,14);(0,1.42,15);

     (1,2.0,3);(1,1.15,4);(1,0.36,7);(1,2.08,11);(1,1.03,12);(1,1.57,13);(1,1.2,14);(1,1.42,15);

     (2,1.15,4);(2,0.36,7);(2,2.08,11);(2,1.03,12);(2,1.57,13);(2,1.2,14);(2,1.42,15);

     (3,1.43,5);(3,1.42,15);(3,1.03,12);

     (4,1.43,5);(4,1.03,12);(4,1.42,15);

     (5,1.65,6);(5,1.03,12);(5,1.42,15);

     (6,1.03,12);(6,1.42,15);(6,1.18,9);

     (7,1.65,6);(7,0.32,8);(7,1.03,12);(7,1.42,15);

     (8,1.65,6);(8,1.18,9);(8,1.03,12);(8,1.42,15);

     (9,1.03,12);(9,1.42,15);

     (10,1.03,12);(10,1.42,15);

     (11,1.03,12);(11,1.42,15);

     (12,1.42,15);

     (13,1.03,12);(13,1.42,15);(13,0.69,10);

     (14,0.69,10);(14,1.03,12);(14,1.42,15);


     (15,1.03,12)] in

  let (dist_course, prev_course) = dij 0 course_graph in
  let prev_array_course =  (List.fold_left (fun x y -> (deopt_p y)^x) "" (Array.to_list prev_course)) in 
  let dist_array_course = (List.fold_left (fun x y -> (string_of_float y)^x) "" (Array.to_list dist_course)) in 

  let burton =
    My_graph.from_edges [(1,1.,2);(2,1.,1);(1,1.,4);(4,1.,1);(1,1.,5);(5,1.,1);
			 (1,1.,6);(6,1.,1);(1,1.,3);(3,1.,1);(1,1.,11);(11,1.,1);
			 (1,1.,12);(12,1.,1);(1,1.,13);(13,1.,1);(1,1.,18);(18,1.,1);
			 (1,1.,19);(19,1.,1);(1,1.,20);(20,1.,1);(1,1.,0);(0,1.,1);
			 
			 (2,1.,4);(4,1.,2);(2,1.,5);(5,1.,2);(2,1.,6);(6,1.,2);
			 (2,1.,7);(7,1.,2);(2,1.,8);(8,1.,2);(2,1.,9);(9,1.,2);
			 (2,1.,10);(10,1.,2);

			 (3,1.,11);(11,1.,3);(3,1.,12);(12,1.,3);(3,1.,13);(13,1.,3);
			 (3,1.,14);(3,0.5,14);(14,1.,3);(3,1.,15);(15,1.,3);(3,1.,16);(16,1.,3);
			 (3,1.,17);(17,1.,3);

			 (4,1.,5);(4,1.,6);
			 (5,1.,4);(5,1.,6);
			 (6,1.,5);(6,1.,4);
			 (7,1.,8);(7,1.,9);(7,1.,10);
			 (8,1.,7);(8,1.,9);(8,1.,10);
			 (9,1.,7);(9,1.,8);(9,1.,10);
			 (10,1.,7);(10,1.,8);(10,1.,9);

			 (11,1.,12);(11,1.,13);
			 (12,1.,11);(12,1.,13);
			 (13,1.,11);(13,1.,12);
			 
			 (14,1.,15);(14,1.,16);(14,1.,17);
			 (15,1.,14);(15,1.,16);(15,1.,17);
			 (16,1.,14);(16,1.,15);(16,1.,17);
			 (17,1.,14);(17,1.,15);(17,1.,16);

			 (18,1.,19);(18,1.,20);(18,1.,0);
			 (19,1.,18);(19,1.,20);(19,1.,0);
			 (20,1.,18);(20,1.,19);(20,1.,0);
			 (0,1.,18);(0,1.,19);(0,1.,20)]
  in
  let (dist_burton, prev_burton) = dij 1 burton in
  let prev_array_burton =
    List.fold_left (fun x y -> (deopt_p y)^x) "" (Array.to_list prev_burton) in
  let dist_array_burton =
    List.fold_left (fun x y -> (string_of_float y)^x) "" (Array.to_list dist_burton) in  
  assert (prev_array = "00_");
  assert (dist_array = "2.1.0.");
  assert (prev_array_1 = "22___");
  assert (dist_array_1 = "3.4.0.infinf");
  assert (prev_array_2 = "1_010_");
  assert (dist_array_2 = "9.2inf5.13.21.10." );
  assert (print_string prev_array_3 = ());
  assert (prev_array_3 = "433__3_"); 
  assert (dist_array_3 ="13.47.211.20.inf0.2inf");
  (* 4, 5, course, burton *)
  assert (dist_array_4 = "7.434.30.8infinfinf35.328.21.713.36.20.");
  assert (prev_array_4 = "094___43210_");
  assert (prev_array_5 = "1020942__4321_1"); 
  assert (dist_array_5 = "28.18.68.627.824.630.6infinf29.121.815.57.10.1.2");
  assert (prev_array_course = "0000014871740000_");
  assert (dist_array_course = "1.421.21.571.032.081.892.911.731.413.062.581.152.1.741.050.");
  assert (prev_array_burton = "1113333111222211111_1"); 
  assert (dist_array_burton = "1.1.1.2.2.2.1.51.1.1.2.2.2.2.1.1.1.1.1.0.1.");;
 
run_tests ();

