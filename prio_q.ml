open Order

exception QueueEmpty
exception Impossible
  
type elt = {id : int; mutable tent_dist : float };;

let compare x y = 
  if x.tent_dist < y.tent_dist then Less 
  else if x.tent_dist > y.tent_dist then Greater 
  else Eq
;;

module type PRIOQUEUE =
sig
  type queue

  (* Returns an empty queue *)
  val empty :  unit -> queue

  (* Takes a queue, and returns whether or not it is empty *)
  val is_empty : queue -> bool
  val add : elt -> queue -> queue
  val take : queue -> elt * queue
  val lookup : int -> queue -> elt option
  val update : int -> float -> queue -> queue
  val print_q : queue -> unit
  val run_tests : unit -> unit
end

(*******************************************************************************)
(********************    Priority Q using Lists   ******************************)
(*******************************************************************************)

module ListQueue : PRIOQUEUE = 
struct

  type queue = elt list
    
  let empty () = []
  
  let is_empty (t: queue) = t = empty () 

  let rec add (e : elt) (q : queue) =
    match q with
    | [] -> [e]
    | hd::tl ->
      match compare e hd with
      | Less -> e::q
      | Greater | Eq -> hd::(add e tl)

  let print_q (q: queue) : unit = 
    List.iter (fun x -> (print_string "\n id: "; print_string (string_of_int x.id);
    print_string " tent_dist: "; print_string (string_of_float x.tent_dist);)) q
    
  let take (q : queue) : (elt * queue) =
    (*print_string "Current:"; print_queue q; print_string "\n ******** \n";*)
    match q with
    | [] -> raise QueueEmpty 
    | hd::tl -> hd, tl
      
  let lookup (l_id: int) (q: queue) : elt option =	
    List.fold_left (fun a y -> if y.id = l_id then Some y else a) None q
      
  (*let delete (a: int) (q: queue) : queue = 
    List.fold_left (fun x y -> if y.id = a then x else y::x) [] q*)

  let delete (a: int) (q: queue) : queue = 
    List.filter (fun x -> x.id<>a) q
      
  let update (a: int) (new_dist: float) (q: queue) : queue =
    let new_queue = delete a q in
    add {id = a; tent_dist = new_dist} new_queue
      
  let run_tests () = 
    let a = empty () in
    let b = add {id=0; tent_dist=4.} a in
    let c = add {id=3; tent_dist=1.} b in
    let d = add {id=1; tent_dist=3.} c in
    let e = add {id=2; tent_dist=2.} d in
    (* test add *)
    assert (e = [{id=3; tent_dist=1.};{id=2; tent_dist=2.};  
		 {id=1; tent_dist=3.};  {id=0; tent_dist=4.}]);
    let f = add {id=6; tent_dist=2.3} e in
    assert (f = [{id=3; tent_dist=1.};{id=2; tent_dist=2.}; 
		 {id=6; tent_dist=2.3};
		 {id=1; tent_dist=3.}; {id=0; tent_dist=4.}]);
    (* test take *)
    let (e1,q1) = take f in
    assert (e1 = {id=3; tent_dist=1.});
    assert (q1 = [{id=2; tent_dist=2.}; 
		 {id=6; tent_dist=2.3};
		 {id=1; tent_dist=3.}; {id=0; tent_dist=4.}]);
    (* test lookup *)
    assert (lookup 3 e = Some {id=3; tent_dist=1.});
    assert (lookup 2 e = Some {id=2; tent_dist=2.});
    assert (lookup 1 e = Some {id=1; tent_dist=3.});
    assert (lookup 0 e = Some {id=0; tent_dist=4.});
    (* test delete *)
    let wrong = delete 3 f in
   (* print_string "HERERERERERE \n\n\n\n\n";
    print_queue wrong;
    print_string " \n\n\n\n\n";*)
    assert (delete 3 f = [{id=2; tent_dist=2.}; 
		 {id=6; tent_dist=2.3};
		 {id=1; tent_dist=3.}; {id=0; tent_dist=4.}]);
    assert (delete 0 f = [{id=3; tent_dist =1.};{id=2; tent_dist=2.}; 
		 {id=6; tent_dist=2.3};
		 {id=1; tent_dist=3.}]);
    (* test update *)
    let q = update 6 1.2 f in
    assert (q = [{id=3;tent_dist=1.};{id=6;tent_dist=1.2};{id=2;tent_dist=2.};
		 {id=1; tent_dist=3.}; {id=0; tent_dist=4.}]);
    assert (update 2 2.6 q = [{id=3;tent_dist=1.};{id=6;tent_dist=1.2};
			      {id=2;tent_dist=2.6};{id=1;tent_dist=3.};
			      {id=0;tent_dist=4.}])
      
end;;

ListQueue.run_tests ();;

(*******************************************************************************)
(********************    Priority Q using Binary Heap   ************************)
(*******************************************************************************)

module BinaryHeap : PRIOQUEUE =
struct

  type balance = Even | Odd

  type tree =   TwoBranch of balance * elt * tree * tree
              | OneBranch of elt * elt
              | Leaf of elt

  type queue = Empty | Tree of tree

  let empty () = Empty

  let is_empty (q : queue) = q = Empty

  let hash = Hashtbl.create 10

  let add (e : elt) (q : queue) : queue =
    Hashtbl.add hash e.id e.tent_dist;
    let rec add_to_tree (e : elt) (t : tree) : tree =
      match t with
      | Leaf e1 ->
        (match compare e e1 with
         | Eq | Greater -> OneBranch (e1, e)
         | Less -> OneBranch (e, e1))
      | OneBranch(e1, e2) ->
        (match compare e e1 with
         | Eq | Greater -> TwoBranch (Even, e1, Leaf e2, Leaf e)
         | Less -> TwoBranch (Even, e, Leaf e2, Leaf e1))
      | TwoBranch(Even, e1, t1, t2) ->
        (match compare e e1 with
         | Eq | Greater -> TwoBranch(Odd, e1, add_to_tree e t1, t2)
         | Less -> TwoBranch(Odd, e, add_to_tree e1 t1, t2))
      | TwoBranch(Odd, e1, t1, t2) ->
        match compare e e1 with
        | Eq | Greater -> TwoBranch(Even, e1, t1, add_to_tree e t2)
        | Less -> TwoBranch(Even, e, t1, add_to_tree e1 t2)
    in
    match q with
    | Empty -> Tree (Leaf e)
    | Tree t -> Tree (add_to_tree e t)

  let get_top (t : tree) : elt =
    match t with
    | Leaf e
    | TwoBranch (_,e,_,_)
    | OneBranch (e,_) -> e

  type dir = Left | Right | Neither

  let compare3 (e1 : elt) (e2 : elt) (e3 : elt) =
    match compare e2 e3 with
    | Less | Eq ->
      (match compare e1 e2 with
      | Less | Eq -> Neither
      | Greater -> Left)
    | Greater ->
      match compare e1 e3 with
      | Less | Eq -> Neither
      | Greater -> Right

  let swap (e : elt) (t : tree) =
    match t with
    | Leaf _ -> Leaf e
    | OneBranch (_,e1) -> OneBranch (e,e1)
    | TwoBranch (b,_,t1,t2) -> TwoBranch (b,e,t1,t2)

  let rec fix (t : tree) : tree =
    match t with
    | Leaf e -> t
    | OneBranch (e1,e2) ->
      (match compare e1 e2 with
      | Less | Eq -> t
      | Greater -> OneBranch(e2,e1))
    | TwoBranch (b,e,t1,t2) ->
      let top1, top2 = get_top t1, get_top t2 in
      match compare3 e top1 top2 with
        | Neither -> t
        | Left -> TwoBranch(b, top1, fix (swap e t1), t2)
        | Right -> TwoBranch(b, top2, t1, fix (swap e t2))

  let extract_tree (q : queue) : tree =
    match q with
    | Empty -> raise QueueEmpty 
    | Tree t -> t

let print_elt (e: elt) : unit = 
    print_string " id: ";
    print_string (string_of_int e.id);
    print_string " tent_dist "; 
    print_string (string_of_float e.tent_dist);
    print_string " "

  let print_q (q:queue) = 
    let rec print_h t =
    match t with  
    | Leaf x -> print_string "Leaf "; print_elt x; 
    | OneBranch (e1, e2) -> 
      (print_string "One Branch(";  print_elt e1; print_elt e2; print_string ")")
    | TwoBranch (Even, e1, t1, t2) -> 
      (print_string "TwoBranch (Even, "; print_elt e1; print_h t1; print_h t2;
       print_string ")")
    | TwoBranch (Odd, e1, t1, t2) -> 
      (print_string "TwoBranch (Odd, "; print_elt e1; print_h t1; print_h t2; 
       print_string ")")
    in 
    match q with 
    | Empty -> print_string "Empty String"
    | Tree t -> print_h t


  let rec print_t (t: tree) =
    match t with  
    | Leaf x -> print_string "Leaf "; print_elt x; 
    | OneBranch (e1, e2) -> 
      (print_string "One Branch(";  print_elt e1; print_elt e2; print_string ")")
    | TwoBranch (Even, e1, t1, t2) -> 
      (print_string "TwoBranch (Even, "; print_elt e1; print_t t1; print_t t2;
       print_string ")")
    | TwoBranch (Odd, e1, t1, t2) -> 
      (print_string "TwoBranch (Odd, "; print_elt e1; print_t t1; print_t t2; 
       print_string ")")
	
  let rec get_last (t : tree) : elt * queue =
    match t with
    | Leaf e -> e, Empty
    | OneBranch (e1, e2) -> e2, Tree (Leaf e1)
    | TwoBranch (Even, e, t1, t2) ->
      let (last, q2') = get_last t2 in
      (match q2' with
      | Empty -> last, Tree (OneBranch(e, get_top t1))
       | Tree t2' -> last, Tree (TwoBranch(Odd, e, t1, t2')))
    | TwoBranch (Odd, e, t1, t2) ->
      let (last, q1') = get_last t1 in
      last, Tree (TwoBranch(Even, e, extract_tree q1', t2))
	
   let take (q : queue) : elt * queue =
    match extract_tree q with
    | Leaf e -> e, Empty
    | OneBranch (e1, e2) -> e1, Tree (Leaf e2)
    | TwoBranch (Even, e, t1, t2) ->
      let (last, q2') = get_last t2 in
      (match q2' with
      | Empty -> (e, Tree (fix (OneBranch (last, get_top t1))))
      | Tree t2' -> (e, Tree (fix (TwoBranch (Odd, last, t1, t2')))))
    | TwoBranch (Odd, e, t1, t2) ->
      let last, q1' = get_last t1 in
      match q1' with
      | Empty -> raise (Failure "The weak invariant has been broken")
      | Tree t1' -> e, Tree (fix (TwoBranch (Even, last, t1', t2)))

  let reconcile (res1: elt option) (res2: elt option) =
     match (res1, res2) with
     | (None, None) -> None
     | (Some x, None) -> Some x
     | (None, Some x) -> Some x
     | (Some x, Some y) -> raise (Failure "Impossible")

  let lookup (id: int) (q: queue) : elt option = 
    let rec match_tree (t: tree) : elt option  = 
      match t with
      | Leaf v -> 
	if v.id = id then Some v else None
      | OneBranch (l, r) ->
        if l.id = id then Some l
	else if r.id = id then Some r
        else None
      | TwoBranch (b, v, l, r) ->
	if v.id = id then Some v
	else reconcile (match_tree l) (match_tree r)
    in
    match q with
    | Empty -> None
    | Tree t -> match_tree t

   let update (id: int) (new_dist: float)(q: queue) : queue = 
    let new_rec = {id = id; tent_dist = new_dist} in
    let rec helper (id: int) (new_dist: float) (t: tree) : tree =
      match t with
      | Leaf x -> 
	if x.id = id then Leaf new_rec
        else Leaf x
      | OneBranch (l, r) -> 
	if l.id = id then OneBranch (new_rec, r)
	else if r.id = id then
	  (if l.tent_dist > new_dist then OneBranch (new_rec, l)
           else OneBranch (l, new_rec))
	else OneBranch (l, r)
      | TwoBranch (Even, v, l, r) ->
	if v.id = id then TwoBranch (Even, new_rec, l, r)
 	else (fix (TwoBranch (Even, v, (helper id new_dist l), 
			      (helper id new_dist r))) )
      | TwoBranch (Odd, v, l, r) ->
	if v.id = id then TwoBranch (Odd, new_rec, l, r)
	else (fix (TwoBranch  (Odd, v, (helper id new_dist l), 
			       (helper id new_dist r))) )
    in
    match q with  
    | Empty -> raise (Failure "trying to update an empty q")
    | Tree t -> Tree (helper id new_dist t)


  let run_tests () = 
    let test_1 = 
      Tree (TwoBranch (Even, {id = 0; tent_dist = 0.}, 
		       OneBranch({id=1;tent_dist=1.},{id=2;tent_dist=2.}), 
		       OneBranch({id=4;tent_dist=4.},{id=5;tent_dist=5.}))) in
    (* test lookup *)
    assert((lookup 0 test_1) = Some {id = 0; tent_dist = 0.});
    assert((lookup 5 test_1) = Some {id = 5; tent_dist = 5.});
    assert((lookup 1 test_1) = Some {id = 1; tent_dist = 1.});
    assert((lookup 2 test_1) = Some {id = 2; tent_dist = 2.});
    assert((lookup 4 test_1) = Some {id = 4; tent_dist = 4.});
    
    let test_2 = 
      Tree (TwoBranch (Even, {id= 2; tent_dist= 0.}, 
		       OneBranch({id=3;tent_dist=infinity},
				 {id=1;tent_dist=infinity}), 
		       OneBranch({id=4;tent_dist=infinity},
				 {id=0;tent_dist=infinity}))) in
    assert((lookup 3 test_2) = Some{id= 3; tent_dist=infinity});

    let test_3 = 
      Tree(TwoBranch(Odd,{id=4;tent_dist=3.},
		     OneBranch({id=3;tent_dist=infinity},
			       {id= 1;tent_dist=infinity}),
		     Leaf{id=0;tent_dist=infinity})) in
    assert((lookup 3 test_3)=Some{id=3;tent_dist=infinity});

    (* test update *)
    assert (update 0 0.2 test_1=Tree
	(TwoBranch(Even,{id=0;tent_dist=0.2},OneBranch({id=1;tent_dist=1.},
						       {id=2;tent_dist=2.}), 
		   OneBranch({id=4;tent_dist=4.},{id= 5; tent_dist = 5.}))));
    
    assert(update 4 0.2 test_1 = Tree
	(TwoBranch (Even,{id=0;tent_dist=0.},OneBranch({id=1;tent_dist=1.},
						       {id=2; tent_dist=2.}), 
		    OneBranch({id=4;tent_dist=0.2},{id=5;tent_dist=5.}))))
end
  

(*****************************************************************************0*)
(******************   Priority Q using Binary Search Tree **********************)
(*******************************************************************************)



(* binary search tree invariant -- if its smaller, then it goes on the 
left, it its bigger, it goes on the right *)
module BinSQueue : PRIOQUEUE = 
struct
  type queue =  Leaf | Branch of queue * elt * queue

  let empty () = Leaf

  let print_q (q: queue) = ()
  
  let is_empty (t: queue) = t = empty () 

  (* the second arguments needs to be a queue and not a tree *)
  let rec add (x : elt) (t : queue) : queue = 
    match t with
    | Leaf -> Branch(Leaf, x, Leaf)
    | Branch (l, v, r) ->
      if x.tent_dist < v.tent_dist then Branch (add x l, v, r) 
      else Branch (l, v, add x r) 
        
   (* helper for take *)
  let rec pull_min (t : queue) : elt * queue =
    match t with
    | Leaf -> raise QueueEmpty
    | Branch (Leaf, v, r) -> (v, r)
    | Branch (l, v, r) -> let min, t' = pull_min l in (min, Branch (t', v, r))
    
  (* this used to be an elt list -- why would it think that? *)
  let rec take (t : queue) : elt * queue =
    match t with
    | Leaf -> raise QueueEmpty
    | Branch (Leaf, v, r) -> (v, r)
    | Branch (l, v, r) -> let min, t' = pull_min l in (min, Branch (t', v, r))

(* we want lookup to return an elt option *)
(* this lookup assumes that our tree is organized based on ids
 !!!!!!!!!!!!!!!!!!!!!! *)
(* Need to go through the whole tree *) 				       
  let reconcile (res1: elt option) (res2: elt option) =
     match (res1, res2) with
     | (None, None) -> None
     | (Some x, None) -> Some x
     | (None, Some x) -> Some x
     | (Some x, Some y) -> raise (Failure "Impossible")

   let rec lookup (x : int) (t : queue) : elt option = 
    match t with
    | Leaf -> None (* q's empty *)
    | Branch (l, v, r) -> 
       if v.id = x then Some v
       else reconcile (lookup x l) (lookup x r)


  let rec delete (x : int) (t : queue) : queue =
    match t with
    | Leaf -> Leaf
    | Branch (Leaf, v, Leaf) -> 
      if v.id = x then Leaf else Branch (Leaf, v, Leaf)
    | Branch (Leaf, v, r) ->
      if v.id = x then r
      else Branch (Leaf, v, delete x r) 
    | Branch (l, v, Leaf) -> 
      if v.id = x then l 
      else Branch (delete x l, v, Leaf)
    | Branch (l, v, r) ->
      if v.id = x then 
	(match l with 
	| Leaf -> raise Impossible
	| Branch (l2, v2, r2) -> Branch (delete v2.id l, v2, r))
      else Branch (delete x l, v, delete x r)


  (* change this function completely *)
  let update (id: int) (new_dist: float) (pq: queue) : queue =
    let take_out = delete id pq in
    add {id = id; tent_dist = new_dist} take_out
          

  let run_tests () = 
    () 
end



(*******************************************************************************)
(********************    Priority Q using D-ary Heap   *************************)
(*******************************************************************************)



(*******************************************************************************)
(********************    Priority Q using Fib Heap    **************************)
(*******************************************************************************)

open Fibsource

module EltOrd =
struct
  type t = elt
  let compare a b =
    if a.tent_dist < b.tent_dist then (-1)
    else if a.tent_dist > b.tent_dist then 1
    else 0
  (* ??? *) 
  let min = {id=0;tent_dist=0.}
end

(* INTIALIZED ARRAY WITH 1000 NODES OF DISTANCE INFINITY *)

module FibHeap : PRIOQUEUE = 
struct
  exception QueueEmpty
  exception Impossible
  module F = Make(EltOrd)
  open F

  type hashtable = (int, float fibnode) Hashtbl.t
  type queue = float fibheap * hashtable
     
  let hash () = Hashtbl.create 10 
  
  let empty () = (fibheap_create (), hash ())

  let is_empty (q: queue) : bool = q = empty ()
    
  let add (e: elt) (q: queue) =
    let (heap, hash) = q in
    let node = fibnode_new e e.tent_dist in
    fibheap_insert heap node; Hashtbl.add hash e.id node; q
      
  let take (q: queue) : elt * queue =
    let (heap, hash) = q in
    let node = fibheap_extract_min heap in
    Hashtbl.remove hash node.key.id;
    ({id=node.key.id;tent_dist=node.data},q)
  
  let lookup (id: int) (q: queue) =
    let (heap, hash) = q in
    let node = try Some (Hashtbl.find hash id) with Not_found -> None in
    match node with 
    | None -> None 
    | Some i -> Some {id=i.key.id;tent_dist=i.data}

  let delete (id: int) (q: queue) : queue =
    let (heap, hash) = q in
    let node = Hashtbl.find hash id in
    Hashtbl.remove hash id; fibheap_delete heap node; q
      
  let update (id: int) (d: float) (q: queue) : queue =
    let (heap, hash) = q in
    let node = Hashtbl.find hash id in
    Hashtbl.remove hash id; fibheap_delete heap node;
    add {id=id;tent_dist=d} q
      
  let print_q (q: queue) =
    let (heap, hash) = q in
    fibheap_print string_of_float Format.std_formatter heap

  let run_tests () = 
    let a = empty () in
    let _ = add {id=0;tent_dist=1.} a in
    let _ = add {id=1;tent_dist=2.} a in
    let _ = add {id=2;tent_dist=3.} a in
    let _ = add {id=3;tent_dist=4.} a in
    let _ = add {id=4;tent_dist=5.} a in
    let _ = add {id=5;tent_dist=6.} a in  

    let (el, b) = take a in
    assert(el = ({id=0;tent_dist=1.}));
    let (el, c) = take b in
    assert(el = ({id=1;tent_dist=2.}));
    let (el, d) = take c in
    assert(el = ({id=2;tent_dist=3.}));
    let (el, e) = take d in
    assert(el = ({id=3;tent_dist=4.}));
    

    
end;;

FibHeap.run_tests ();;

module type ARG =
sig
  val d : int
  val n : int
end

module DHeap (A : ARG) : PRIOQUEUE = 
struct 

  (* how do we get the 1000 in there? How do we take the d as argument?? *) 
  let n = A.n
  let d = A.d
    
  type queue = {heap : elt option array; 
		lt : int option array; 
		mutable first_empty: int}
    
  let empty _ : queue = 
    {heap = Array.make n None; lt = Array.make n None ; first_empty = 0}

  (* fold over the array- is everything None? *) 
  let is_empty (q: queue) : bool = q.first_empty = 0

  let print_item (x : elt option) : unit = 
    match x with 
    | None -> () 
    | Some x -> 
      (print_string (("id: ")^(string_of_int x.id)^(" tent_dist: ")^
			(string_of_float x.tent_dist)))

  let print_q (q: queue) = 
    Array.iter (fun x -> print_item x) q.heap;
    print_string "\n"

(* MAKE OPTION OR CHECK WHEN CALLED THAT THERE ARE CHILDREN *) 
  (* looks at the children from index i, finds the one with the minimum tent_dist. 
     Returns child's index and element *)
  let min_child (i:int) (q:queue) : int * elt = 
    let rec loop (current:int) (last:int) (min_e: int * elt) (q:queue) : int * elt = 
      let (min_e_index, min_e_elt) = min_e in 
      if current > last then min_e else 
	(match q.heap.(current) with 
	(* reached the end of the queue *) 
	| None -> min_e
	| Some x -> (print_string (("child candidate: ")^
				      (string_of_float x.tent_dist));
		     if (x.tent_dist < min_e_elt.tent_dist)
	  then loop (current + 1) last (current, x) q
	  else loop (current + 1) last min_e q))
    in 
    print_string (("current: ")^(string_of_int (d*i +1))^
		     (" last : ")^(string_of_int ((d*i) + d))^("\n"));
    (*if ((d*i + 1) < q.first_empty) then *)
    loop (d*i + 1) (min (d*i+d) (q.first_empty-1))
      (500, {id=500;tent_dist = infinity}) q
      
     
  
   (*check against first_empty *) 
  let fix_down (i: int) (q:queue) : queue = 
    (* need to check d*i to d*i + d-1 and find the minimum to know where to insert*) 
    let rec fix (i: int) (q:queue) : queue = 
      let current_elt = match Array.get q.heap i with 
	| None -> raise (Failure "check fix_down") (* this is wrong *) 
	| Some e -> e in 
      let (min_child_index, min_child_elt) = min_child i q in 
      let swap = Array.set q.heap min_child_index (Some current_elt); 
        Array.set q.heap i (Some min_child_elt); 
	Array.set q.lt i (Some min_child_index);
	Array.set q.lt min_child_index (Some i);
      in (* swap the parent and child *) 
      if current_elt.tent_dist > min_child_elt.tent_dist then 
	(swap;
	 fix min_child_index q)
      else q 
    in fix i q


  let deopt (e: elt option) : elt = 
    match e with 
    | None -> raise (Failure "deoptionalize")
    | Some x -> x 

  let rec fix_up (i: int) (q:queue) : queue = 
    let parent_index = (i - 1)/d in 
    let parent_elt = deopt (Array.get q.heap parent_index) in 
    let child_elt = deopt (Array.get q.heap i) in 
    (* WATCH OUT FOR THIS *) 
    let swap =  (* Swap the items in the heap *) 
                Array.set q.heap parent_index (Some child_elt); 
                Array.set q.heap i (Some parent_elt); 
		(* Update the lookup table *)
		Array.set q.lt child_elt.id (Some parent_index);
		Array.set q.lt parent_elt.id (Some i)
    in 
    let compare_parent = 
      (* swap the parent and child *) 
      if parent_elt.tent_dist > child_elt.tent_dist then 
	(swap;
	 (* FOR TESTING *) 
	 print_string "was swapped: ";
	 print_q q;
	 fix_up parent_index q)
      (* only done if parent is smaller than child*) 
      else if ((d*i+1) >= q.first_empty) then q 
      else 
	(let (mind_child_index, min_child_elt) = min_child parent_index q in
	   print_string "min child of "; 
	   print_int parent_index;
	   print_string " : ";
	   print_string (("id: ")^(string_of_int min_child_elt.id)^
			    (" tent_dist : "));
	   print_string (string_of_float min_child_elt.tent_dist);
	   print_string "\n";
	   if min_child_elt.tent_dist > parent_elt.tent_dist then q
	   else (print_string "Caught the problem \n \n"; 
		 fix_down parent_index q)) in 
    (* must end when queue is empty *) 
    (* WAS JUST q BEFORE *) 
    if i <= 0 then q else compare_parent

  
let add (e: elt) (q: queue) : queue =
    (* put an element in the first_empty slot *) 
    Array.set q.heap q.first_empty (Some e);
    Array.set q.lt e.id (Some q.first_empty);
    q.first_empty <- (q.first_empty + 1);
    fix_up (q.first_empty - 1) q


  let take (q: queue) : elt * queue = 
    print_string "Kittens \n\n\n\n"; flush_all ();
    let min_elt = deopt (Array.get q.heap 0) in
    (* get the last element in the heap *) 
    let new_front = deopt (Array.get q.heap (q.first_empty - 1)) in
    let updated_q = (Array.set q.heap 0 (Some new_front); 
		     Array.set q.heap (q.first_empty -1) None; 
                     q.first_empty <- (q.first_empty - 1); 
		     fix_down 0 q)
    in 
    (min_elt, updated_q)


  let lookup (i: int) (q: queue) : elt option = 
    match q.lt.(i) with 
    | None -> raise QueueEmpty
    | Some x -> q.heap.(x)
   
  let update (i: int) (f: float) (q: queue) : queue = 
    (* lookup the index in the array *) 
    match q.lt.(i) with 
    | None -> print_string "problem in two-ary heap";  raise (Failure "Impossible") 
    | Some x -> (fix_up x 
		   (q.heap.(x) <- (Some {id = i; tent_dist = f});
		    q))
  
  let listify (q: queue) : elt list = 
    List.fold_left (fun y x -> match x with 
    | None -> y 
    | Some z -> z::y) [] (Array.to_list q.heap)

let run_tests () = 
    let a = empty () in
    print_string "PRINTING A\n";
    print_q a;
    let b = add {id=0; tent_dist=4.} a in
    print_string "PRINTING B\n";
    print_q b;
    let c = add {id=3; tent_dist=1.} b in
    print_string "PRINTING C\n";
    print_q c;
    let d = add {id=1; tent_dist=3.} c in
    print_string "PRINTING D\n";
    print_q d;
    let e = add {id=2; tent_dist=2.} d in
    print_string "PRINTING E\n";
    print_q e; flush_all ();
    assert(1 = 1)
    (* test add *)
    (*assert((listify e)= [{id=3; tent_dist=1.};{id=2; tent_dist=2.};  
		 {id=1; tent_dist=3.};  {id=0; tent_dist=4.}]);
    let f = add {id=6; tent_dist=2.3} e in
    assert (f = [{id=3; tent_dist=1.};{id=2; tent_dist=2.}; 
		 {id=6; tent_dist=2.3};
		 {id=1; tent_dist=3.}; {id=0; tent_dist=4.}]);
    (* test take *)
    let (e1,q1) = take f in
    assert (e1 = {id=3; tent_dist=1.});
    assert (q1 = [{id=2; tent_dist=2.}; 
		 {id=6; tent_dist=2.3};
		 {id=1; tent_dist=3.}; {id=0; tent_dist=4.}]);
    (* test lookup *)
    assert (lookup 3 e = Some {id=3; tent_dist=1.});
    assert (lookup 2 e = Some {id=2; tent_dist=2.});
    assert (lookup 1 e = Some {id=1; tent_dist=3.});
    assert (lookup 0 e = Some {id=0; tent_dist=4.});
    (* test delete *)
    let wrong = delete 3 f in
   (* print_string "HERERERERERE \n\n\n\n\n";
    print_queue wrong;
    print_string " \n\n\n\n\n";*)
    assert (delete 3 f = [{id=2; tent_dist=2.}; 
		 {id=6; tent_dist=2.3};
		 {id=1; tent_dist=3.}; {id=0; tent_dist=4.}]);
    assert (delete 0 f = [{id=3; tent_dist =1.};{id=2; tent_dist=2.}; 
		 {id=6; tent_dist=2.3};
		 {id=1; tent_dist=3.}]);
    (* test update *)
    let q = update 6 1.2 f in
    assert (q = [{id=3; tent_dist=1.}; {id=6; tent_dist=1.2};{id=2; tent_dist=2.};{id=1; tent_dist=3.}; {id=0; tent_dist=4.}]);
    assert (update 2 2.6 q =  [{id=3; tent_dist=1.}; {id=6; tent_dist=1.2};{id=2; tent_dist=2.6};{id=1; tent_dist=3.}; {id=0; tent_dist=4.}])*)
end ;; 

module Two_aryHeap = 
  DHeap(struct
    let n = 1000
    let d = 2
  end);;

Two_aryHeap.run_tests ()
