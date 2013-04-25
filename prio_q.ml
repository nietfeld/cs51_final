(* Here is where the module signature for the priority q module will go and all the functors that implement it *)
exception TODO


type order = Equal | Less | Greater 
(*
module type ELT =
sig
  type elt
  val compare : elt -> elt -> Order.order
  val to_string : elt -> string
end*)

type elt = {id : int; tent_dist : float};;
(*
let compare x y = 
  let (i1, f1) = x in
  let (i2, f2) = y in
  if f1 < f2 then Less else if f1 > f2 then Greater else Eq;;*)

let compare x y = 
  if x.id < y.id then Less else if x.id > y.id then Greater else raise (Failure "Compare sucks");;

(* this is the Module type that all of the below will return *)
(* do you think we need to add anything else to this? *)
module type PRIOQUEUE =
sig
  exception QueueEmpty

  (*module E : ELT
    
  (* What's being stored in the priority queue *)
  type elt = E.elt*)
    
  (* The queue itself (stores things of type elt) *)
  type queue

  (* Returns an empty queue *)
  val empty : queue

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
  val lookup : int -> queue -> elt

  (* Run invariant checks on the implementation of this binary tree.
   * May raise Assert_failure exception *)
  val run_tests : unit -> unit
end


(* this would just take some kind of q module *)
(* An example implementation of the COMPARABLE_AND_GENABLE signature *)

(* the float here is what represents the edge weight *)
(*
module IdCompare : ELT with type elt = (int*float) =
struct
  open Order
  type elt = (int*float)
  let compare x y = 
    let (i1, f1) = x in
    let (i2, f2) = y in
    if f1 < f2 then Less else if f1 > f2 then Greater else Eq

  let to_string p =
    let (x,y) = p in 
     (string_of_int x)^"*"^(string_of_float y)

 (* let generate () = (0, 0.0)
  let generate_gt x () = x + 1
  let generate_lt x () = x - 1
  let generate_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1) *)
end

*)

(*******************************************************************************)
(********************    Priority Q using Lists   ******************************)
(*******************************************************************************)


module ListQueue : PRIOQUEUE = 
struct
  exception QueueEmpty
  exception Impossible
    (*
  module E = IdCompare
   
  type elt = E.elt *)
  type queue = elt list

  let empty = []
  
  let is_empty (t: queue) = t = empty  

  let rec add (e : elt) (q : queue) =
    match q with
    | [] -> [e]
    | hd::tl ->
      match compare e hd with
      | Less -> e::q
      | Greater | Equal -> hd::(add e tl)

  let rec take (q : queue) =
    match q with
    | [] -> raise QueueEmpty (* might want to do something about this later *)
    | hd::tl -> hd, tl

  let rec lookup (l_id: int) (q: queue) : elt =
(*    match q with
    | [] -> raise NotPossible
    | ((id, dst) as e) :: tail -> if id = a then e else lookup a tail
*)	
    List.fold_right (fun a y -> if a.id = l_id then a else y) 
      q (raise Impossible)
      
  let run_tests () = ()
end



(*******************************************************************************)
(********************    Priority Q using Binary Heap   **************************)
(*******************************************************************************)
module BinaryHeap : PRIOQUEUE =
struct
  
  exception QueueEmpty
  exception Impossible
 
  (*module E = IdCompare
    
  type elt = E.elt *)

  (* Be sure to read the pset spec for hints and clarifications.
   *
   * Remember the invariants of the tree that make up your queue:
   * 1) A tree is ODD if its left subtree has 1 more node than its right
   * subtree. It is EVEN if its left and right subtrees have the same number of
   * nodes. The tree can never be in any other state. This is the WEAK
   * invariant, and should never be false.
   *
   * 2) All nodes in the subtrees of a node should be *greater* than (or equal
   * to) the value of that node. This, combined with the previous invariant,
   * makes a STRONG invariant. Any tree that a user passes in to your module
   * and receives back from it should satisfy this invariant.  However, in the
   * process of, say, adding a node to the tree, the tree may intermittently
   * not satisfy the order invariant. If so, you *must* fix the tree before
   * returning it to the user.  Fill in the rest of the module below!
   *)
  (* A node in the tree is either even or odd *)
  type balance = Even | Odd

  (* A tree is either just a single element, has one branch (the first elt in
   * the tuple is the element at this node, and the second elt is the element
   * down the branch), or has two branches (with the node being even or odd) *)
  type tree =   TwoBranch of balance * elt * tree * tree
              | OneBranch of elt * elt
              | Leaf of elt

  (* A queue is either empty, or a tree *)
  type queue = Empty | Tree of tree

  let empty = Empty

  let is_empty (q : queue) = q = Empty

  (* Adds element e to the queue q *)
  let add (e : elt) (q : queue) : queue =
    (* Given a tree, where e will be inserted is deterministic based on the
     * invariants. If we encounter a node in the tree where its value is greater
     * than the element being inserted, then we place the new elt in that spot
     * and propagate what used to be at that spot down toward where the new
     * element would have been inserted *)
    let rec add_to_tree (e : elt) (t : tree) : tree =
      match t with
      (* If the tree is just a Leaf, then we end up with a OneBranch *)
      | Leaf e1 ->
        (match compare e e1 with
         | Equal | Greater -> OneBranch (e1, e)
         | Less -> OneBranch (e, e1))

      (* If the tree was a OneBranch, it will now be a TwoBranch *)
      | OneBranch(e1, e2) ->
        (match compare e e1 with
         | Equal | Greater -> TwoBranch (Even, e1, Leaf e2, Leaf e)
         | Less -> TwoBranch (Even, e, Leaf e2, Leaf e1))

      (* If the tree was even, then it will become an odd tree (and the element
       * is inserted to the left *)
      | TwoBranch(Even, e1, t1, t2) ->
        (match compare e e1 with
         | Equal | Greater -> TwoBranch(Odd, e1, add_to_tree e t1, t2)
         | Less -> TwoBranch(Odd, e, add_to_tree e1 t1, t2))

      (* If the tree was odd, then it will become an even tree (and the element
       * is inserted to the right *)
      | TwoBranch(Odd, e1, t1, t2) ->
        match compare e e1 with
        | Equal | Greater -> TwoBranch(Even, e1, t1, add_to_tree e t2)
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
    | Less | Equal ->
      (match compare e1 e2 with
      | Less | Equal -> Neither
      | Greater -> Left)
    | Greater ->
      match compare e1 e3 with
      | Less | Equal -> Neither
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
      | Less | Equal -> t
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

  (* Implements the algorithm described in the writeup. You must finish this
   * implementation, as well as the implementations of get_last and fix, which
   * take uses *)
  let rec take (q : queue) : elt * queue =
    match extract_tree q with
    (* If the tree is just a Leaf, then return the value of that leaf, and the
     * new queue is now empty *)
    | Leaf e -> e, Empty

    (* If the tree is a OneBranch, then the new queue is just a Leaf *)
    | OneBranch (e1, e2) -> e1, Tree (Leaf e2)

    (* Removing an item from an even tree results in an odd tree. This
     * implementation replaces the root node with the most recently inserted
     * item, and then fixes the tree that results if it is violating the
     * strong invariant *)
    | TwoBranch (Even, e, t1, t2) ->
      let (last, q2') = get_last t2 in
      (match q2' with
       (* If one branch of the tree was just a leaf, we now have just
        * a OneBranch *)
       | Empty -> (e, Tree (fix (OneBranch (last, get_top t1))))
       | Tree t2' -> (e, Tree (fix (TwoBranch (Odd, last, t1, t2')))))
    (* Implement the odd case! *)
    | TwoBranch (Odd, e, t1, t2) ->
      let last, q1' = get_last t1 in
      match q1' with
      | Empty -> raise (Failure "The weak invariant has been broken")
      | Tree t1' -> e, Tree (fix (TwoBranch (Even, last, t1', t2)))

  let lookup (id: int) (q: queue) : elt =
    let rec optedlookup (a : int) (t : tree) : elt option = 
      match t with
      | Leaf x -> if a = x.id then Some x else None
      | OneBranch (x, y) ->
	(if x.id = a then Some x 
	 else if y.id = a then Some y
	 else None)
      | TwoBranch (b, x, lt, rt) ->
	if a = x.id then Some x
	else if a > x.id then optedlookup a rt
	else optedlookup a lt
    in
    match q with
    | Empty -> raise QueueEmpty
    | Tree t ->
      match optedlookup id t with
      | None -> raise Impossible
      | Some e -> e
	  
	    
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
(********************    Priority Q using D-ary Heap   **************************)
(*******************************************************************************)




(*******************************************************************************)
(********************    Priority Q using Fib Heap    **************************)
(*******************************************************************************)


(*

(* make the actual modules -- something along these lines *)
module IntTree = BinSTree(IntCompare)

(* Please read the entirety of "testing.ml" for an explanation of how
 * testing works.
 *)
let _ = IntTree.run_tests ()
*)
