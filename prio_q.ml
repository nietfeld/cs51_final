open Order
exception TODO

type elt = {id : int; mutable tent_dist : float };;

let compare x y = 
  if x.tent_dist < y.tent_dist then Less 
  else if x.tent_dist > y.tent_dist then Greater 
  else Eq
;;

module type PRIOQUEUE =
sig
  exception QueueEmpty
    
  (* The queue itself (stores things of type elt) *)
  type queue

  (* Returns an empty queue *)
  val empty :  unit -> queue

  (* Takes a queue, and returns whether or not it is empty *)
  val is_empty : queue -> bool

  (* Takes an element and a queue, and returns a new queue with the
   * element added *)
  val add : elt -> queue -> queue

  (* Pulls the highest priority element out of the passed-in queue,
   * also returning the queue with that element
   * removed. Can raise the QueueEmpty exception. *)
  val take : queue -> elt * queue
    
  (* Given an id, gives back the corresponding node with that id *)
  val lookup : int -> queue -> elt option

  (* this takes the id of the element to be updated and the new distance
   and returns the updated queue *)
  val update : int -> float -> queue -> queue

  val print_q : queue -> unit

  (* Run invariant checks on the implementation of this binary tree.
   * May raise Assert_failure exception *)
  val run_tests : unit -> unit
end

(*******************************************************************************)
(********************    Priority Q using Lists   ******************************)
(*******************************************************************************)


module ListQueue : PRIOQUEUE = 
struct
  exception QueueEmpty
  exception Impossible
    
  type queue = elt list

  let print_q _ = ()

  let empty () = []
  
  let is_empty (t: queue) = t = empty () 

  let rec add (e : elt) (q : queue) =
    match q with
    | [] -> [e]
    | hd::tl ->
      match compare e hd with
      | Less -> e::q
      | Greater | Eq -> hd::(add e tl)

  let print_queue (q: queue) : unit = 
    List.iter (fun x -> (print_string "\n id: "; print_string (string_of_int x.id); print_string " tent_dist: "; print_string (string_of_float x.tent_dist);)) q
    
  let rec take (q : queue) =
    print_string "Current:"; print_queue q; print_string "\n ******** \n";
    (match q with
    | [] -> ({id = 0; tent_dist = infinity}, []) (* why is this ID 0? *)
    | hd::tl -> hd, tl)

  let lookup (l_id: int) (q: queue) : elt option =	
    List.fold_right (fun a y -> if a.id = l_id then Some a else y)  q None (*(print_string "in list_queue lookup"; print_string (string_of_int l_id); raise Impossible) *) 
    

  let update (a: int) (new_dist: float) (q: queue) : queue =
    let new_queue = List.fold_left (fun x y -> if y.id = a then x else y::x) [] q in 
    add {id = a; tent_dist = new_dist} new_queue

  let delete (a: int) (q: queue) : queue = 
    List.fold_left (fun x y -> if y.id = a then x else y::x) [] q

  let run_tests () = 
    () 
 
end


(*******************************************************************************)
(********************    Priority Q using Binary Heap   ************************)
(*******************************************************************************)

module BinaryHeap : PRIOQUEUE =
struct
  
  exception QueueEmpty
  exception Impossible

  type balance = Even | Odd

  type tree =   TwoBranch of balance * elt * tree * tree
              | OneBranch of elt * elt
              | Leaf of elt

  (* A queue is either empty, or a tree *)
  type queue = Empty | Tree of tree

  let empty () = Empty

  let is_empty (q : queue) = q = Empty

  let hash = Hashtbl.create 10

  (* Adds element e to the queue q *)
  let add (e : elt) (q : queue) : queue =
    Hashtbl.add hash e.id e.tent_dist;
    let rec add_to_tree (e : elt) (t : tree) : tree =
      match t with
      (* If the tree is just a Leaf, then we end up with a OneBranch *)
      | Leaf e1 ->
        (match compare e e1 with
         | Eq | Greater -> OneBranch (e1, e)
         | Less -> OneBranch (e, e1))

      (* If the tree was a OneBranch, it will now be a TwoBranch *)
      | OneBranch(e1, e2) ->
        (match compare e e1 with
         | Eq | Greater -> TwoBranch (Even, e1, Leaf e2, Leaf e)
         | Less -> TwoBranch (Even, e, Leaf e2, Leaf e1))

      (* If the tree was even, then it will become an odd tree (and the element
       * is inserted to the left *)
      | TwoBranch(Even, e1, t1, t2) ->
        (match compare e e1 with
         | Eq | Greater -> TwoBranch(Odd, e1, add_to_tree e t1, t2)
         | Less -> TwoBranch(Odd, e, add_to_tree e1 t1, t2))

      (* If the tree was odd, then it will become an even tree (and the element
       * is inserted to the right *)
      | TwoBranch(Odd, e1, t1, t2) ->
        match compare e e1 with
        | Eq | Greater -> TwoBranch(Even, e1, t1, add_to_tree e t2)
        | Less -> TwoBranch(Even, e, t1, add_to_tree e1 t2)
    in
    (* If the queue is empty, then e is the only Leaf in the tree.
     * Else, insert it into the proper location in the pre-existing tree *)
    match q with
    | Empty -> Tree (Leaf e)
    | Tree t -> Tree (add_to_tree e t)

  (* Simply returns the top element of the tree t (i.e., just a single pattern
   * match in *)
  let get_top (t : tree) : elt =
    match t with
    | Leaf e
    | TwoBranch (_,e,_,_)
    | OneBranch (e,_) -> e

  (* Takes a tree, and if the top node is greater than its children, fixes
   * it. If fixing it results in a subtree where the node is greater than its
   * children, then you must (recursively) fix this tree too *)
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
    | Empty -> (*raise QueueEmpty*) Leaf {id = 0; tent_dist = 3.14436624}
    | Tree t -> t

  (* Takes a tree, and returns the item that was most recently inserted into
   * that tree, as well as the queue that results from removing that element.
   * Notice that a queue is returned (since removing an element from just a leaf
   * would result in an empty case, which is captured by the queue type
   *
   * By "item most recently inserted", we don't mean the
   * most recently inserted *value*, but rather the newest node that was
   * added to the bottom-level of the tree. If you follow the implementation
   * of add carefully, you'll see that the newest value may end up somewhere
   * in the middle of the tree, but there is always *some* value brought
   * down into a new node at the bottom of the tree. *This* is the node
   * that we want you to return.
   *)

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
      (print_string "TwoBranch (Even, "; print_elt e1; print_h t1; print_h t2; print_string ")")
    | TwoBranch (Odd, e1, t1, t2) -> 
      (print_string "TwoBranch (Odd, "; print_elt e1; print_h t1; print_h t2; print_string ")")
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
      (print_string "TwoBranch (Even, "; print_elt e1; print_t t1; print_t t2; print_string ")")
    | TwoBranch (Odd, e1, t1, t2) -> 
      (print_string "TwoBranch (Odd, "; print_elt e1; print_t t1; print_t t2; print_string ")")
	
  let rec get_last (t : tree) : elt * queue =
    print_string "Im in get_last";
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

  (* Implements the algorithm described in the writeup. You must finish this
   * implementation, as well as the implementations of get_last and fix, which
   * take uses *)
  let take (q : queue) : elt * queue =
    print_string "I'm in take yo \n";
    print_q q;
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

 (* now we'll need to fix it *) 
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
       (* might be prob here *)
	else (fix (TwoBranch (Even, v, (helper id new_dist l),  (helper id new_dist r))) )
      | TwoBranch (Odd, v, l, r) ->
	if v.id = id then TwoBranch (Odd, new_rec, l, r)
       (* might be prob here *)
	else (fix (TwoBranch  (Odd, v, (helper id new_dist l),  (helper id new_dist r))) )
    in
    match q with  
    | Empty -> raise (Failure "trying to update an empty q")
    | Tree t -> Tree (helper id new_dist t)


  let test_1 = Tree (TwoBranch (Even, {id = 0; tent_dist = 0.}, OneBranch ({id= 1; tent_dist=1.},{id=2; tent_dist=2.}), OneBranch( {id= 4; tent_dist=4.},  {id= 5; tent_dist = 5.})));;
  
  assert((lookup 0 test_1) = Some {id = 0; tent_dist = 0.});;
  print_string "PASSED FIRST";;
  assert((lookup 5 test_1) = Some {id = 5; tent_dist = 5.});;
  print_string "PASSED SECOND";;
  assert((lookup 1 test_1) = Some {id = 1; tent_dist = 1.});;
  assert((lookup 2 test_1) = Some {id = 2; tent_dist = 2.});;
  assert((lookup 4 test_1) = Some {id = 4; tent_dist = 4.});;

 
  let test_2 = Tree (TwoBranch (Even, {id= 2; tent_dist= 0.}, 
		     OneBranch ({id= 3; tent_dist=infinity}, {id= 1; tent_dist =infinity}), 
		     OneBranch ( {id= 4; tent_dist =infinity}, {id= 0; tent_dist =infinity} )));;

  assert((lookup 3 test_2) = Some{id= 3; tent_dist=infinity});;


  let test_3 = Tree(TwoBranch (Odd, {id= 4; tent_dist =3.}, OneBranch( {id= 3; tent_dist= infinity}, {id= 1; tent_dist = infinity}), Leaf {id= 0; tent_dist =infinity}));;
  
  let my_print =
   match (lookup 3 test_3) with
   | None -> print_string "not working"
   | Some a -> print_string ((string_of_int a.id)^(string_of_float a.tent_dist));;

  my_print;;
  assert((lookup 3 test_3) = Some{id= 3; tent_dist=infinity});;


  (*
    let test_get_top () =
    let x = E.generate () in
    let t = Leaf x in
    assert (get_top t = x);
    let y = E.generate_gt x () in
    let t = OneBranch (x, y) in
    assert (get_top t = x);
    let z = E.generate_gt y () in
    let t = TwoBranch (Even, x, Leaf y, Leaf z) in
    assert (get_top t = x);
    let w = E.generate_gt z () in
    let t = TwoBranch (Odd, x, OneBranch (y, w), Leaf z) in
    assert (get_top t = x);
    let q = E.generate_gt w () in
    let t = TwoBranch (Even, x, OneBranch (y, w), OneBranch (z, q)) in
    assert (get_top t = x)

    let test_fix () =
    let x = E.generate () in
    let t = Leaf x in
    assert (fix t = t);
    let y = E.generate_gt x () in
    let t = OneBranch (y, x) in
    assert (fix t = OneBranch (x, y));
    let t = OneBranch (x, y) in
    assert (fix t = OneBranch (x, y));
    let z = E.generate_gt y () in
    let t = TwoBranch (Even, x, Leaf y, Leaf z) in
    assert (fix t = t);
    let t = TwoBranch (Even, z, Leaf x, Leaf y) in
    assert (fix t = TwoBranch (Even, x, Leaf z, Leaf y));
    let t = TwoBranch (Even, y, Leaf z, Leaf x) in
    assert (fix t = TwoBranch (Even, x, Leaf z, Leaf y));
    let w = E.generate_gt z () in
    let t = TwoBranch (Odd, x, OneBranch (y, w), Leaf z) in
    assert (fix t = t);
    let t = TwoBranch (Odd, z, OneBranch (x, y), Leaf w) in
    assert (fix t = TwoBranch (Odd, x, OneBranch (y, z), Leaf w));
    let t = TwoBranch (Odd, w, OneBranch (y, z), Leaf x) in
    assert (fix t = TwoBranch (Odd, x, OneBranch (y, z), Leaf w));
    let t = TwoBranch (Odd, w, OneBranch (x, z), Leaf y) in
    assert (fix t = TwoBranch (Odd, x, OneBranch (z, w), Leaf y));
    let q = E.generate_gt w () in
    let t = TwoBranch (Even, x, OneBranch (y, w), OneBranch (z, q)) in
    assert (fix t = t);
    let t = TwoBranch (Even, y, OneBranch (x, w), OneBranch (z, q)) in
    assert (fix t = TwoBranch (Even, x, OneBranch (y, w), OneBranch (z, q)));
    let t = TwoBranch (Even, w, OneBranch (x, y), OneBranch (z, q)) in
    assert (fix t = TwoBranch (Even, x, OneBranch (y, w), OneBranch (z, q)));
    let t = TwoBranch (Even, w, OneBranch (z, q), OneBranch (x, y)) in
    assert (fix t = TwoBranch (Even, x, OneBranch (z, q), OneBranch (y, w)));
    let t = TwoBranch (Even, w, OneBranch (x, q), OneBranch (y, z)) in
    assert (fix t = TwoBranch (Even, x, OneBranch (w, q), OneBranch (y, z)));
    let t = TwoBranch (Even, q, OneBranch (y, z), OneBranch (x, w)) in
    assert (fix t = TwoBranch (Even, x, OneBranch (y, z), OneBranch (w, q)))

    let test_take () =
    let x = E.generate () in
    let y = E.generate_gt x () in
    let z = E.generate_gt y () in
    let w = E.generate_gt z () in
    let t = Tree (TwoBranch (Odd, x, OneBranch (y, w), Leaf z)) in
    assert (take t = (x, Tree (TwoBranch (Even, y, Leaf w, Leaf z))))
  *)

  let run_tests () = ()
 (*   test_get_top ();
    test_fix ();
    test_take () *)

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
    let node = fibnode_new e e.tent_dist in print_string "!";
    fibheap_insert heap node; Hashtbl.add hash e.id node; q
      
  let take (q: queue) : elt * queue =
    let (heap, hash) = q in
    let node = fibheap_extract_min heap in print_string "?" ;
    Hashtbl.remove hash node.key.id;
    ({id=node.key.id;tent_dist=node.data},q)
      
  let lookup (id: int) (q: queue) =
    let (heap, hash) = q in
    let node = Hashtbl.find hash id in
    Some {id=node.key.id;tent_dist=node.data}
      
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

    assert(print_q a = ());
    let (el, b) = take a in
    assert(el = ({id=0;tent_dist=1.}));
    assert(print_q b = ()); (*
    let (el, c) = take b in
    assert(el = ({id=1;tent_dist=2.}));
    let (el, d) = take c in
    assert(el = ({id=2;tent_dist=3.}));
    let (el, e) = take d in
    assert(el = ({id=3;tent_dist=4.}));
			   *)
			
end;;

FibHeap.run_tests ();;
