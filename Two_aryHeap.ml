module Two_aryHeap : PRIOQUEUE = 
struct 
  exception QueueEmpty
  exception Impossible

  (* how do we get the 1000 in there? How do we take the d as argument?? *) 
  let n = 1000
  let d = 2 
 
  (*type first_empty = int ref*)

  type lookup_table = int option array
  
  type q_array = elt option array 

  (* Need to initialize this at the beginning *) 
  let lookup_table (n: int) : int option array = 
    Array.make n None 

  type queue = {q = q_array; lt = lookup_table; mutable first_empty = int}
 
    
  let empty : queue = 
    {q = Array.make n None; lt = Array.make n None ; first_empty <- 0}

  (* fold over the array- is everything None? *) 
  let is_empty (q: queue): 
      !first_empty = 0
      (*Array.fold_left (fun a y -> if y = None then a else false) true q*)

  (* UPDATE LOOKUP TABLE IN HERE *) 
  let rec fix_up (i: int) (q:queue) : q = 
    let parent_index = (*floor*) (i - 1)/d in 
    let Some parent_elt = Array.get q (parent_index i) in 
    let Some child_elt = Array.get q i in 
    let swap =  Array.set q parent_index Some child_elt; 
                Array.set q i Some parent_elt; 
    let compare_parent = 
      (* swap the parent and child *) 
      if parent_elt.tent_dist > child_elt.tent_dist then 
	(swap;
	 fix_up parent_index q)
      else q in 
    if i = 0 then q else compare_parent

  let add (e: elt) (q: queue) : q =
    (* put an element in the first_empty slot *) 
    Array.set q !first_empty (Some e);
    fix !first_empty q;
    first_empty := first_empty +1;
    q

(* ALSO UPDATE LOOKUP TABLE HERE *) 
(* check this *) 
  let rec fix_down (i: int) (q:queue) : q = 
    let child_index = (*check*) d*i + 1  in 
    let Some child_elt = Array.get q (parent_index i) in 
    let Some parent_elt = Array.get q i in 
    let swap =  Array.set q child_index Some parent_elt; 
                Array.set q i Some child_elt; 
    let compare_parent = 
      (* swap the parent and child *) 
      if parent_elt.tent_dist > child_elt.tent_dist then 
	(swap;
	 fix_down parent_index q)
      else q in 
    if i = 0 then q else compare_parent

  let take (q: queue) : ely * queue = 
    let Some min_elt = Array.get q 0 in 
    let new_front = Array.get q !first_empty in 
    let updated_q = fix_down !first_empty (Array.set q 0 new_front) in 
    Array.set q !first_empty None;
    first_empty := first_empty - 1; 
    (min_elt, updated_q)

  let lookup (i: int) (q: queue) = 
    match Array.get i lookup_table with 
    | None -> None 
    | Some x -> Array.get x q
   
  let update (i: int) (f: float) (q: queue) = 
    (* lookup the index in the array *) 
    match Array.get i lookup_table with 
    | None -> print_string "problem in two-ary heap";  raise (Failure "Impossible") 
    | Some x -> fix_up x (Array.set x q {id = i; tent_dist = f})
   
  let print_item (x : elt option) : unit = 
    match x with 
    | None -> () 
    | Some x -> 
      (print_string (("id: ")^(string_of_int x.id)^(" tent_dist: ")^(string_of_float x.tent_dist))); 


  let print_q (q: queue) = 
    Array.iter (fun x -> print_item x) q 
      
  let run_tests () : unit = ()
end ;; 
   
  

  
    
