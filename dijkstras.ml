(*
Djikstra's 
deletemin(h) - pop off top of heap
insert(x,y,h) -  
change(x,y,h) - 
insert and change is pretty much the same. 

let s be the start

distance array = s= 0, infinity 
prevarray = NULL
priority queue = {s = 0} // distance to self is zero

delete min off the heap. 
get s. 

look at all of s's neighbor. Can  we improve the distance of any of them? (look up in array and compare)
if diet(w) > dist(u) + lenght(u,v)
	// UPDATE VALUES 
	diet (w) = diet u + length(u,v) 
	prev(w) = v
	// put vertices on the heap 
	insert/change (w, dist[w], [H])

pop one off the heap 
	go through code above 
	*)

(*
(* this is going to be very similar *)

let sort (m : (module PRIOQUEUE with type elt=IntCompare.t)) (lst : int list) =
  let module P = (val (m) : PRIOQUEUE with type elt = IntCompare.t) in

  let rec extractor pq lst =
    if P.is_empty pq then lst else
    let (x, pq') = P.take pq in
    extractor pq' (x::lst) in
  let pq = reduce (fun x y -> P.add x y) P.empty lst in
  List.rev (extractor pq [])


(* Hurray!! Now, we can pass in the modules into sort and get out
 * different sorts!! *)

(* Sorting with a priority queue with an underlying heap
 * implementation is equivalent to heap sort! *)
let heapsort = sort heap_module

(* Sorting with a priority queue with your underlying tree
 * implementation is *almost* equivalent to treesort;
 * a real treesort relies on self-balancing binary search trees *)

let treesort = sort tree_module

(* Sorting with a priority queue with an underlying unordered list
 * implementation is equivalent to heap sort! If your implementation of
 * ListQueue used ordered ilsts, then this is really insertion sort *)
let selectionsort = sort list_module
*)
