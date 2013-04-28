open Prioqueue
open Order

type elt = {id : int; mutable tent_dist : float };;

let compare x y = 
  if x.tent_dist < y.tent_dist then Less else if x.tent_dist > y.tent_dist then Greater else Eq
;;

module BinHeap : PRIOQUEUE 
struct 
  (* a head is either full or not full *) 
  type balance = Even | Odd

  type queue = Leaf | Heap of balance * queue * elt * queue 

  let empty = Leaf 

  let is_empty (q : queue) = 
    (match q with 
    | Leaf -> true
    | Heap a -> false)

  (* now we'll need to fix it *) 
  let rec add (e: elt) (q: queue) : queue = 
    match q with  
    | Leaf -> Heap (Even, Leaf, e, Leaf) 
    (* insert from left to right *) 
    | Heap (Even, q1, e1, q2) -> Heap (Odd, (add e q1), e1, q2)
    (* insert from right to left *)  
    | Heap (Odd, q1, e1, q2) -> Heap (Even, q1, e1, (add e q2))

  (* FILL IN THE REST OF THE STUFF HERE *) 

end 
      
