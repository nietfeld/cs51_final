open Prio_q
open Order
(*
type elt = {id : int; mutable tent_dist : float };;

let compare x y = 
  if x.tent_dist < y.tent_dist then Less else if x.tent_dist > y.tent_dist then Greater else Eq
;;*)

module BinHeap : PRIOQUEUE =
struct
  exception QueueEmpty
  exception Impossible
    
  (* a head is either full or not full *) 
  type balance = Even | Odd

  type queue = Leaf | Heap of balance * elt * queue * queue 

  let empty = Leaf 

  let is_empty (q : queue) = 
    (match q with 
    | Leaf -> true
    | Heap (a,b,c,d) -> false)

  let fix (q:queue) : queue = 
    match q with
    | Leaf -> q 
    (* was just inserted to the right *) 
    | Heap (Even,e1,q1,q2) -> 
      (* IS Q1 or Q2 *) 
      (match q2 with 
      (* nothing to fix *) 
      | Leaf -> q
      (* combine next two cases *)
      | Heap (Odd,e2,q3,q4) -> 
	if e1.tent_dist > e2.tent_dist 
	  then Heap(Even, e2, (Heap (Odd,e1,q3,q4)), q2) 
	else q
      | Heap (Even,e2,q3,q4) -> 
	if e1.tent_dist > e2.tent_dist 
	  then Heap (Even, e2, (Heap (Even,e1,q3,q4)), q2) 
	else q
      )
    (* was just inserted to the left *) 
    | Heap(Odd,e1,q1,q2) -> 
      (match q1 with 
      | Leaf -> q
      | Heap (Odd,e2,q3,q4) -> 
	if e1.tent_dist > e2.tent_dist 
	  then Heap(Odd,e2,q1,(Heap(Odd,e1,q3,q4)))
	else q 
      | Heap(Even,e2,q3,q4) -> 
	if e1.tent_dist > e2.tent_dist 
	then Heap(Odd,e2,q1,(Heap(Even,e1,q3,q4)))
	else q
      )

  (* now we'll need to fix it *) 
  let rec add (e: elt) (q: queue) : queue = 
    match q with  
    | Leaf -> Heap(Even,e,Leaf,Leaf)
    (* insert from left to right *) 
    | Heap (Even, e1, q1, q2) -> (fix (Heap(Odd,e1,(add e q1),q2)))
    (* insert from right to left *)  
    | Heap (Odd, e1, q1, q2) -> (fix (Heap(Even,e1,q1,(add e q2))))


  (* FILL IN THE REST OF THE STUFF HERE *) 
  let take (q:queue) : (elt * queue) = 
    ({id = 1; tent_dist = 0.}, q)

  let lookup i q = None

  let update i f (q:queue) = q 

  let delete i q = q

  let print_elt (e: elt) : unit = 
    print_string " id: ";
    print_string (string_of_int e.id);
    print_string " tent_dist "; 
    print_string (string_of_float e.tent_dist);
    print_string " "

  let rec print_h (q: queue) : unit = 
    match q with  
    | Leaf -> print_string "Leaf " 
    | Heap (Even, e1, q1, q2) -> 
      (print_string " Heap (Even, ";  print_elt e1; print_h q1; print_h q2; print_string ")")
    | Heap (Odd, e1, q1, q2) -> 
      (print_string " Heap (Odd, "; print_elt e1; print_h q1;  print_h q2; print_string ")")

(*
  let print_simple_queue = 
    let q = add {id = 1; tent_dist = 0.5} empty in 
    let q2 = add {id = 2; tent_dist = 0.2} q in 
    let q3 = add {id = 3; tent_dist = 0.3} q2 in 
    let q4 = add {id = 4; tent_dist = 0.6} q3 in 
    let q5 = add {id = 5; tent_dist = 0.1} q4 in
    let q6 = add {id = 6; tent_dist = 0.8} q5 in
    (*print_h empty; 
    print_string "\n";
    print_h q; 
    print_string "\n"; 
    print_h q2;
    print_string "\n"; 
    print_h q3;
    print_string "\n"; 
    print_h q4;
    print_string "\n";*) 
    print_h q6 *) 

  let run_tests () = 
    (*print_simple_queue;*)
    let q = add {id=1; tent_dist=0.5} empty in 
    let q2 = add {id=2; tent_dist=0.4} q in 
    assert(q = Heap(Even,{id=1; tent_dist=0.5},Leaf,Leaf));
    print_h q2;
    assert(q2 = Heap(Odd,{id=1; tent_dist=0.4},
		     (Heap(Even,{id=1; tent_dist=0.5},Leaf,Leaf)),Leaf)) 

   
    

end;;
  
 (* BinHeap.run_tests ();; *)
      
