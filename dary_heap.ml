type order = Equal | Less | Greater 

type elt = {id : int; tent_dist : float};;


let compare x y = 
  if x.id < y.id then Less else if x.id > y.id then Greater else 
      (print_string "Uh oh check compare \n" 
       print_string (string_of_int x.id); 
       print_string (string_of_int y.id);
       Equal)
;;

module DaryHeap (d: int) : PRIOQUEUE = 
struct 
  exception QueueEmpty
  exception Impossible
  exception ViolatesInvariant

  (* d must be at least 2 *) 
  assert(d >= 2);  

  (* need to keep track of number in list and also number of children....*) 
  type heap =   Leaf 
              | Heap of elt * (heap list)

  let is_empty (q: queue) = q = Leaf 

  (* allows us to update Leafs to heaps *) 
  let child_list : heap list = 
    let rec build_list (n : int) (cl : heap list) = 
      if n = 0 then cl 
      else build_list (n-1) Leaf::cl in 
    build_list d []

  let add (e: elt) (h: heap) = 
    (* do we need a recursive helper at all? *) 
    let rec add_to_tree (e: elt) (h: heap) : heap = 
      match h with 
      | Leaf  -> Heap (e, child_list) 
       (* THIS IS WRONG BUT WANT IT TO COMPILE *) 
      | Heap (el, hl) -> Heap (e1, h1)
	(*(match h1 with 
	(* always filling in 
	| Leaf -> Heap (e, child_list)
	| Heap (e2, e1) -> 
        )*)
    in add_to_tree e h
