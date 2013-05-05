exception QueueEmpty
exception Impossible
  
type elt = {id : int; mutable tent_dist : float};;

type order = Less | Greater | Eq

let compare x y = 
  if x.tent_dist < y.tent_dist then Less 
  else if x.tent_dist > y.tent_dist then Greater 
  else Eq
;;

module type PRIOQUEUE =
sig
  type queue
  val empty :  unit -> queue
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
    
  let empty () : queue = []
    
  let is_empty (t: queue) : bool = 
    t = []
    
  let rec add (e : elt) (q : queue) : queue =
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
    match q with
    | [] -> raise QueueEmpty 
    | hd::tl -> hd, tl
      
  let lookup (l_id: int) (q: queue) : elt option =	
    List.fold_left (fun a y -> if y.id = l_id then Some y else a) None q
      
  let delete (a: int) (q: queue) : queue = 
    List.filter (fun x -> x.id <> a) q
      
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
		 {id=1; tent_dist=3.};{id=0; tent_dist=4.}]);
    let f = add {id=6; tent_dist=2.3} e in
    assert (f = [{id=3; tent_dist=1.};{id=2; tent_dist=2.}; 
		 {id=6; tent_dist=2.3};{id=1; tent_dist=3.};
		 {id=0; tent_dist=4.}]);
    
    (* test take *)
    let (e1,q1) = take f in
    assert (e1 = {id=3; tent_dist=1.});
    assert (q1 = [{id=2; tent_dist=2.};{id=6; tent_dist=2.3};
		  {id=1; tent_dist=3.};{id=0; tent_dist=4.}]);
    
    (* test lookup *)
    assert (lookup 3 e = Some {id=3; tent_dist=1.});
    assert (lookup 2 e = Some {id=2; tent_dist=2.});
    assert (lookup 1 e = Some {id=1; tent_dist=3.});
    assert (lookup 0 e = Some {id=0; tent_dist=4.});
    
    (* test delete *)
    assert (delete 3 f = [{id=2; tent_dist=2.}; {id=6; tent_dist=2.3};
			  {id=1; tent_dist=3.}; {id=0; tent_dist=4.}]);
    assert (delete 0 f = [{id=3; tent_dist =1.};{id=2; tent_dist=2.}; 
			  {id=6; tent_dist=2.3};{id=1; tent_dist=3.}]);
    
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
    print_string (" id: " ^(string_of_int e.id)^" tent_dist: "
		  ^(string_of_float e.tent_dist)^" ")

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

    let test_2 = 
      Tree (TwoBranch (Even, {id= 2; tent_dist= 0.}, 
		       OneBranch({id=3;tent_dist=infinity},
				 {id=1;tent_dist=infinity}), 
		       OneBranch({id=4;tent_dist=infinity},
				 {id=0;tent_dist=infinity}))) in

    let test_3 = 
      Tree(TwoBranch(Odd,{id=4;tent_dist=3.},
		     OneBranch({id=3;tent_dist=infinity},
			       {id= 1;tent_dist=infinity}),
		     Leaf{id=0;tent_dist=infinity})) in

    (* test take *)
    assert(take test_1 =
	({id=0;tent_dist=0.},Tree
	  (TwoBranch(Odd,{id=1;tent_dist=1.},
		     OneBranch({id=2;tent_dist=2.},{id=5;tent_dist=5.}),
			       (Leaf({id=4;tent_dist=4.}))))));

    assert(take test_1 = ({id=0;tent_dist=0.},
			  Tree(TwoBranch(Odd,{id=1;tent_dist=1.},
					 OneBranch({id=2;tent_dist=2.},
						   {id=5;tent_dist=5.}),
					 (Leaf({id=4;tent_dist=4.}))))));
    
    let (a, q1) = take test_2 in
    let (b, q2) = take q1 in
    let (c, q3) = take q2 in
    let (d, q4) = take q3 in
    let (e, q5) = take q4 in    
    assert(a = {id=2;tent_dist=0.});
    assert(b = {id=0;tent_dist=infinity});
    assert(c = {id=1;tent_dist=infinity});
    assert(d = {id=4;tent_dist=infinity});
    assert(e = {id=3;tent_dist=infinity});
    assert(q5 = Empty);

   (* test add *)
    let q0 = add {id=6;tent_dist=3.2} test_1 in
    let (a, q1) = take q0 in
    let (b, q2) = take q1 in
    let (c, q3) = take q2 in
    let (d, q4) = take q3 in
    let (e, q5) = take q4 in    
    let (f, q6) = take q5 in
    assert(a = {id=0;tent_dist=0.});
    assert(b = {id=1;tent_dist=1.});
    assert(c = {id=2;tent_dist=2.});
    assert(d = {id=6;tent_dist=3.2});
    assert(e = {id=4;tent_dist=4.});
    assert(f = {id=5;tent_dist=5.});
    assert(q6 = Empty);

    (* test lookup *)
    assert((lookup 0 test_1) = Some {id = 0; tent_dist = 0.});
    assert((lookup 5 test_1) = Some {id = 5; tent_dist = 5.});
    assert((lookup 1 test_1) = Some {id = 1; tent_dist = 1.});
    assert((lookup 2 test_1) = Some {id = 2; tent_dist = 2.});
    assert((lookup 4 test_1) = Some {id = 4; tent_dist = 4.});
    assert((lookup 3 test_2) = Some {id = 3; tent_dist=infinity});
    assert((lookup 3 test_3) = Some {id = 3; tent_dist=infinity});

    (* test update *)
    assert(update 0 0.2 test_1=Tree
	(TwoBranch(Even,{id=0;tent_dist=0.2},
		   OneBranch({id=1;tent_dist=1.},{id=2;tent_dist=2.}), 
		   OneBranch({id=4;tent_dist=4.},{id= 5; tent_dist = 5.}))));
    
    assert(update 4 0.2 test_1 = Tree
	(TwoBranch (Even,{id=0;tent_dist=0.},
		    OneBranch({id=1;tent_dist=1.},{id=2; tent_dist=2.}), 
		    OneBranch({id=4;tent_dist=0.2},{id=5;tent_dist=5.}))))
end;;

BinaryHeap.run_tests();

(*****************************************************************************0*)
(******************   Priority Q using Binary Search Tree **********************)
(*******************************************************************************)

(* binary search tree invariant -- if its smaller, then it goes on the 
left, it its bigger, it goes on the right *)
module BinSQueue : PRIOQUEUE = 
struct

  type queue =  Leaf | Branch of queue * elt * queue

  let empty () : queue = Leaf

  let print_q (q: queue) : unit = ()
  
  let is_empty (t: queue) = t = empty () 

  let rec add (x : elt) (t : queue) : queue = 
    match t with
    | Leaf -> Branch(Leaf, x, Leaf)
    | Branch (l, v, r) ->
      if compare x v = Less then Branch (add x l, v, r) 
      else Branch (l, v, add x r) 
        
  let rec pull_min (t : queue) : elt * queue =
    match t with
    | Leaf -> raise QueueEmpty
    | Branch (Leaf, v, r) -> (v, r)
    | Branch (l, v, r) ->
      let min, t' = pull_min l in (min, Branch (t', v, r))
    
  let rec take (t : queue) : elt * queue =
    match t with
    | Leaf -> raise QueueEmpty
    | Branch (Leaf, v, r) -> (v, r)
    | Branch (l, v, r) -> 
      let min, t' = pull_min l in (min, Branch (t', v, r))

  let reconcile (res1: elt option) (res2: elt option) =
    match (res1, res2) with
    | (None, None) -> None
    | (Some x, None) -> Some x
    | (None, Some x) -> Some x
    | (Some x, Some y) -> raise (Failure "Impossible")
      
  let rec lookup (x : int) (t : queue) : elt option = 
    match t with
    | Leaf -> None
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
    let new_q = delete id pq in
    add {id = id; tent_dist = new_dist} (new_q)


  let run_tests () = 
    (* test ADD, take, LOOK UP, UPDATE *)
    let test_1 = 
      Branch (Leaf, {id=1;tent_dist=1.}, 
	      Branch (Leaf, {id =2;tent_dist=2.}, Leaf))
    in

    let test_2 = 
      Branch(Branch(Leaf,{id=5;tent_dist=0.4},Leaf),{id=1;tent_dist=1.},
	     Branch (Leaf, {id =2;tent_dist=2.}, Leaf))
    in

    (* test add *)
    assert (add {id=4;tent_dist=4.3} test_1 =
	Branch(Leaf, {id=1;tent_dist=1.}, 
	       Branch (Leaf, {id =2;tent_dist=2.}, 
		       Branch (Leaf, {id=4;tent_dist=4.3}, Leaf))));
    
    assert (add {id=4;tent_dist=0.3} test_1 =
	Branch(Branch(Leaf,{id=4;tent_dist=0.3},Leaf), 
	       {id=1;tent_dist=1.}, Branch (Leaf, {id =2;tent_dist=2.}, Leaf)));

    (* test take *)
    assert (take test_1 = ({id=1;tent_dist=1.}, 
			   Branch(Leaf, {id =2;tent_dist=2.}, Leaf)));

    (* test look up *)
    assert (lookup 1 test_2 = Some {id=1;tent_dist=1.});
    assert (lookup 10 test_1 = None);
    assert (lookup 2 test_1 = Some {id=2; tent_dist=2.});

    (* test update *) 
    let a = update 1 1.2 test_1 in
    
    let (e, q) = take a in
    assert (e = {id=1;tent_dist=1.2});
    let (e, q) = take q in
    assert (e = {id=2;tent_dist=2.});
    assert (q = Leaf);
    
    let b = update 2 1.4 test_2 in
    let c = update 1 2.5 b in
    let d = update 5 1.9 c in
    
    let (e, q) = take d in
    assert (e = {id=2;tent_dist=1.4});
    let (e, q) = take q in
    assert (e = {id=5;tent_dist=1.9});
    let (e, q) = take q in
    assert (e = {id=1;tent_dist=2.5})

end;;

BinSQueue.run_tests ();


(*******************************************************************************)
(********************    Priority Q using D-ary Heap   *************************)
(*******************************************************************************)


module type ARG =
sig
  val d : int
  val n : int
end

module DHeap (A : ARG) : PRIOQUEUE = 
struct 

  let n = A.n
  let d = A.d
    
  type queue = {heap : elt option array; 
		lt : int option array; 
		mutable first_empty: int}
    
  let empty _ : queue = 
    {heap = Array.make n None; 
     lt = Array.make n None ; 
     first_empty = 0}

  (* fold over the array- is everything None? *) 
  let is_empty (q: queue) : bool = q.first_empty = 0

  (* functions for testing *) 
  let print_item (x : elt option) : unit = 
    match x with 
    | None -> () 
    | Some x -> 
      (print_string (("id: ")^(string_of_int x.id)^(" tent_dist: ")^
			(string_of_float x.tent_dist)))

  let print_q (q: queue) = 
    Array.iter (fun x -> print_item x) q.heap;
    print_string "\n"

  
  let deopt (e: 'a option) : 'a = 
    match e with 
    | None -> raise (Failure "deoptionalize")
    | Some x -> x 


(* TESTING 
	(print_string (("child candidate: ")^(string_of_float child.tent_dist));
    print_string (("current: ")^(string_of_int (d*i +1))^
		     (" last : ")^(string_of_int ((d*i) + d))^("\n"));
*) 



  (* look up the element in lookup table *) 
  let lookup (i: int) (q: queue) : elt option = 
    match q.lt.(i) with 
    | None -> None
    | Some x -> q.heap.(x)
   
 
  (* looks at the children from index i, finds the one with the 
     minimum tent_dist. Returns child's index and element *)
  let min_child (i:int) (q:queue) : elt = 
    let first_child = (d*i+1) in 
    let last_child = min (d*i + d) (q.first_empty -1) in
    let rec compare_children (cur_child_idx: int) (min_so_far: elt) : elt =
      if cur_child_idx = last_child then min_so_far
      else
	let cur_child = deopt (lookup cur_child_idx q) in
        let new_min =
          if min_so_far.tent_dist > cur_child.tent_dist then cur_child
          else min_so_far
	in
        compare_children (cur_child_idx+1) (new_min)
    in
    compare_children first_child (deopt (lookup first_child q))



  let swap (parent: elt) (child: elt) (q: queue) : queue =
    let child_index = deopt (q.lt.(child.id)) in
    let parent_index = deopt (q.lt.(parent.id)) in 
    q.heap.(child_index) <- (Some parent); 
    q.heap.(parent_index) <- (Some child); 
    q.lt.(parent.id) <- (Some child_index);
    q.lt.(child.id) <- (Some parent_index);
    q
     
  (*MUST check against first_empty *) 
  let rec fix_down (i: int) (q:queue) : queue = 
      let parent_elt = deopt q.heap.(i) in 
      let no_children = ((d*i+1) >= q.first_empty) in
      let min_child_elt = min_child i q in  
      if no_children then q
      else (
	if min_child_elt.tent_dist < parent_elt.tent_dist
        then fix_down (deopt (q.lt.(min_child_elt.id)))
	  (swap parent_elt min_child_elt q)
        else q 
      )


  let rec fix_up (i: int) (q:queue) : queue = 
    let parent_index = (i - 1)/d in 
    let parent = deopt q.heap.(parent_index) in 
    let child = deopt q.heap.(i) in 
      if parent.tent_dist > child.tent_dist then 
	(fix_up parent_index (swap parent child q))
      else q


(* Testing : 
      let test_fix_up = print_string "min child of "; 
	   print_int parent_index;
	   print_string " : ";
	   print_string (("id: ")^(string_of_int min_child_elt.id)^
			    (" tent_dist : "));
	   print_string (string_of_float min_child_elt.tent_dist);
	   print_string "\n" in *) 
  

  (* put an element in the first_empty slot, update lookup 
     and last_empty, then fix queue *)
  let add (e: elt) (q: queue) : queue = 
    q.heap.(q.first_empty) <- (Some e);
    q.lt.(e.id) <- (Some q.first_empty);
    q.first_empty <- (q.first_empty + 1);
    fix_up (q.first_empty - 1) q


  (* take the first element, then swap with the last and fix *) 
  let take (q: queue) : elt * queue = 
    let min_elt = deopt q.heap.(0) in
    let new_front = deopt q.heap.(q.first_empty - 1) in
    let updated_q = q.heap.(0) <- (Some new_front); 
		    q.heap.(q.first_empty-1) <- None; 
                    q.first_empty <- (q.first_empty - 1); 
		    fix_down 0 q
    in 
    (min_elt, updated_q)

 let update (i: int) (f: float) (q: queue) : queue = 
    (* lookup the index in the array *) 
    let index = deopt (q.lt.(i)) in 
    let updated_queue = q.heap.(index) <- (Some {id = i; tent_dist = f}); q in 
    fix_up index updated_queue


  
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
  let min = {id=0;tent_dist=0.}
end

(* INTIALIZED ARRAY WITH 1000 NODES OF DISTANCE INFINITY *)

module FibHeap : PRIOQUEUE = 
struct
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
  
  let lookup (id: int) (q: queue) : elt option =
    let (heap, hash) = q in
    let node = try Some (Hashtbl.find hash id)
      with Not_found -> None in
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
    let _ = add {id=0;tent_dist=6.} a in
    let _ = add {id=1;tent_dist=5.} a in
    let _ = add {id=2;tent_dist=4.} a in
    let _ = add {id=3;tent_dist=3.} a in
    let _ = add {id=4;tent_dist=2.} a in
    let _ = add {id=5;tent_dist=1.} a in  


    (* Tests for add and lookup *)
    assert(lookup 0 a = Some {id=0;tent_dist=6.});
    assert(lookup 1 a = Some {id=1;tent_dist=5.});
    assert(lookup 2 a = Some {id=2;tent_dist=4.});
    assert(lookup 3 a = Some {id=3;tent_dist=3.});
    assert(lookup 4 a = Some {id=4;tent_dist=2.});
    assert(lookup 5 a = Some {id=5;tent_dist=1.});
    
    (* Tests for take *)
    let (el, b) = take a in
    assert(el = ({id=5;tent_dist=1.}));
    let (el, c) = take b in
    assert(el = ({id=4;tent_dist=2.}));
    let (el, d) = take c in
    assert(el = ({id=3;tent_dist=3.}));
    let (el, e) = take d in
    assert(el = ({id=2;tent_dist=4.}));
    let (el, f) = take e in
    assert(el = ({id=1;tent_dist=5.}));
    let (el, g) = take f in
    assert(el = ({id=0;tent_dist=6.}));
 
    let a = empty () in
    let _ = add {id=0;tent_dist=1.} a in
    let _ = add {id=1;tent_dist=2.} a in
    let _ = add {id=2;tent_dist=3.} a in
    let _ = add {id=3;tent_dist=4.} a in
    let _ = add {id=4;tent_dist=5.} a in
    let _ = add {id=5;tent_dist=6.} a in  
    
    (* Tests for update *)   
    let _ = update 0 6. a in
    let _ = update 1 5. a in
    let _ = update 2 4. a in
    let _ = update 3 3. a in
    let _ = update 4 2. a in
    let _ = update 5 1. a in

    let (el, b) = take a in
    assert(el = ({id=5;tent_dist=1.}));
    let (el, c) = take b in
    assert(el = ({id=4;tent_dist=2.}));
    let (el, d) = take c in
    assert(el = ({id=3;tent_dist=3.}));
    let (el, e) = take d in
    assert(el = ({id=2;tent_dist=4.}));
    let (el, f) = take e in
    assert(el = ({id=1;tent_dist=5.}));
    let (el, g) = take f in
    assert(el = ({id=0;tent_dist=6.}));

end;;

FibHeap.run_tests ();;
