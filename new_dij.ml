open Prio_q
open Array

exception Not_found

type elt = int
type queue = elt list 

(* Need to finish implementing prioq *) 
module IntListQueue = (ListQueue(NodeCompare) :
                        PRIOQUEUE with type elt = NodeCompare.t)
module IntHeapQueue = (BinaryHeap(NodeCompare) :
                        PRIOQUEUE with type elt = NodeCompare.t)
let list_module = (module IntListQueue : PRIOQUEUE with type elt = NodeCompare.t)
let heap_module = (module IntHeapQueue : PRIOQUEUE with type elt = NodeCompare.t)

(* the thing we're popping off prioq *)
(* will make tuple more general type later - now node ID in graph and tentative distance from A *)
(* type in prio_q *) 
let delete_min (pq : queue) : ((int*float) * queue) = 
  try 
     pq.take
    (* but we don't want to fail just end try display_state from djikstras.ml *)
  with QueueEmpty -> failwith "done" ;;

(* loop through neighbors list (assume for now int * float) *) 
(* FIX CURR_NODE TO BETTER TYPE *) 
 let rec update_queue (pq : queue) (curr_node: int*float) (neighbor_list : (node * float) list) : queue = 
   let (node_id, distance_from_start) = curr_node in 
   match neighbor_list with 
   | [] -> pq 
   | (n,e)::tl -> 
     (* assume look up k is a function into index k of the array *) 
     (match Array.get n.id dist with
     (* CHANGE TO NEW DISTANCE NOTATION *) 
     | Nan -> 
       (* look up id n in prioq- is there a way to only do this way? *) 
       let (k, tent_dist) = pq.lookup n.id in
       let new_dist = e +. distance_to in 
       if new_dist < tent_dist then 
	 (* update priority q *)
	 (let new_pq = pq.update_key k new_dist in 
	 (* we have a new shorter path to k through n *) 
	  Array.set prev k n.id;
	  (* ARE WE PUTTING IN THE RIGHT THING FOR CURRENT NODE *) 
	  update_queue new_pq curr_node tl)
       (* don't update, do next neighbor *) 
       else update_queue pq curr_node tl
     | _ -> update_queue pq curr_node tl) 
      

 let one_round (pq: queue) (my_graph: node graph) : queue = 
   let (curr_node, distance_to) = delete_min pq in 
   let neighbor_list = neighbors n my_graph in 
   (* for more general elements, use node int compare to get string to an int *)
   (* update the distance array *) 
   Array.set dist curr_node distance_to;
   update_queue pq (curr_node, distance_to) neighbor_list


let initialize_queue (n: int) (start: queue) : queue = 
  let rec add_nodes (pq: queue) (to_add: int) : queue = 
    if to_add = 0 then pq 
    (* FIX THIS *) 
    else add_nodes (P.add (to_add, -.00000000004) pq) (to_add - 1)
  (* for starting node, we want the id and dist 0. *) 
  in add_nodes (P.add (start, 0) P.empty) (n-1)
 (* add the starting node with dist 0*) 

let dij start g (pq : (module PRIOQUEUE with type elt=NodeCompare.t)) = 
  let module P = (val (pq) : PRIOQUEUE with type elt = NodeCompare.t) in
  (*let prioq = P.empty in*)
  (* make a prioq with all nodes in graph set to infinity *) 
  if g.has_node start g then 
    (* MAKE FUNCTION *) 
    let graph_size = g.number_nodes in
    (* make distance and prev arrays *)
    (* write way to have them all be in scope *) 
    let dist = Array.make graph_size Nan in 
    let prev = Array.make graph_size -1 in 
    let prioq = initialize_queue graph_size start in 
    (* we want to do an infinite loop and then catch an exception, but instead we'll just loop through *) 
    let rec iterate (pq: queue) (n: int)  = 
      match n with 
      | 0 -> Printf.printf "Finished bitches" 
      | _ -> let new_q = one_round pq g in 
             iterate new_q (n-1)
    (* HOW TO MAKE TAIL RECURSIVE ????? *) 
    in iterate pq graph_size
  else failwith "dij: node unknown";;

(*
(* displaying the results here *)
(* Will update for better display *) 
 let display_state f (g,st)  dest = 
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

let g = test_aho ();;
let r = dij "A" g list_module;;

display_state (fun x y -> Printf.printf "%s!" y) (a,r) "E";; 
(* this is what should return *)
*)
