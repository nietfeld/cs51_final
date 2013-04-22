(* from http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora125.html *)

(* this is a very exposed version of the graph implementation,
so we'll just make it into a modulde with a get_neighbors function *)

(* start *)


(* Here is where the module signature for the priority q module will go and all the functors that implement it *)
exception TODO

(* type Node = or edge??? or which definition do we need here ? *)

type order = Equal | Less | Greater

(* this is the Module type that all of the below will return *)
(* do you think we need to add anything else to this? *)
module type PRIOQUEUE =
sig
  exception QueueEmpty

  (* What's being stored in the priority queue *)
  type elt

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

  (************** WE NEED **************************************)
  (* UPDATE THE VALUE ASSOCIATED WITH A KEY *) 
  (* val update : elt -> queue -> queue *) 
 
  (* Run invariant checks on the implementation of this binary tree.
   * May raise Assert_failure exception *)
  val run_tests : unit -> unit
end


(* would this still be com and gen and would it take an int compare or
what exactly would it take? *)
module type COMPARABLE_AND_GENABLE =
sig
  type t
  val compare : t -> t -> order
  val to_string : t -> string

  (* See the testing.ml for an explanation of
   * what these "generate*" functions do, and why we included them in
   * this signature. *)
  (* Generate a value of type t *)
  val generate: unit -> t
  (* Generate a value of type t that is greater than the argument. *)
  val generate_gt: t -> unit -> t
  (* Generate a value of type t that is less than the argument. *)
  val generate_lt: t -> unit -> t
  (* Generate a value of type t that is between argument 1 and argument 2.
   * Returns None if there is no value between argument 1 and argument 2. *)
  val generate_between: t -> t -> unit -> t option
end


(* this would just take some kind of q module *)
(* An example implementation of the COMPARABLE_AND_GENABLE signature *)
module NodeCompare : COMPARABLE_AND_GENABLE with type t=int =
struct
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Equal
  let to_string = string_of_int

  let generate () = 0
  let generate_gt x () = x + 1
  let generate_lt x () = x - 1
  let generate_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end



(*******************************************************************************)
(********************    Priority Q using Lists   ******************************)
(*******************************************************************************)

module ListQueue (C : COMPARABLE_AND_GENABLE) : PRIOQUEUE with type elt = C.t =
struct
  exception QueueEmpty

  type elt = C.t
  type queue = elt list

  let empty = []
  
  let is_empty (t: queue) = t = empty  

  let rec add (e : elt) (q : queue) =
    match q with
    | [] -> [e]
    | hd::tl ->
      match C.compare e hd with
      | Less -> e::q
      | Greater | Equal -> hd::(add e tl)

  (* ADD UPDATE *) 
  (* FIND THE KEY, FIX THE VALUE *) 
  (* REORDER STUFF IN QUEUE *) 


  let rec take (q : queue) =
    match q with
    | [] -> raise QueueEmpty (* might want to do something about this later *)
    | hd::tl -> hd, tl

  let run_tests () = ()
end



(*******************************************************************************)
(********************    Priority Q using Binary Heap   **************************)
(*******************************************************************************)module BinaryHeap(C : COMPARABLE_AND_GENABLE) : PRIOQUEUE with type elt = C.t =
struct

  exception QueueEmpty

  type elt = C.t

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
        (match C.compare e e1 with
         | Equal | Greater -> OneBranch (e1, e)
         | Less -> OneBranch (e, e1))

      (* If the tree was a OneBranch, it will now be a TwoBranch *)
      | OneBranch(e1, e2) ->
        (match C.compare e e1 with
         | Equal | Greater -> TwoBranch (Even, e1, Leaf e2, Leaf e)
         | Less -> TwoBranch (Even, e, Leaf e2, Leaf e1))

      (* If the tree was even, then it will become an odd tree (and the element
       * is inserted to the left *)
      | TwoBranch(Even, e1, t1, t2) ->
        (match C.compare e e1 with
         | Equal | Greater -> TwoBranch(Odd, e1, add_to_tree e t1, t2)
         | Less -> TwoBranch(Odd, e, add_to_tree e1 t1, t2))

      (* If the tree was odd, then it will become an even tree (and the element
       * is inserted to the right *)
      | TwoBranch(Odd, e1, t1, t2) ->
        match C.compare e e1 with
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
    match C.compare e2 e3 with
    | Less | Equal ->
      (match C.compare e1 e2 with
      | Less | Equal -> Neither
      | Greater -> Left)
    | Greater ->
      match C.compare e1 e3 with
      | Less | Equal -> Neither
      | Greater -> Right

  let swap (e : elt) (t : tree) =
    match t with
    | Leaf _ -> Leaf e
    | OneBranch (_,e1) -> OneBranch (e,e1)
    | TwoBranch (b,_,t1,t2) -> TwoBranch (b,e,t1,t2)

  (*************************** NEED UPDATE **********************)
  (* Find, update, fix tree *) 

  let rec fix (t : tree) : tree =
    match t with
    | Leaf e -> t
    | OneBranch (e1,e2) ->
      (match C.compare e1 e2 with
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


  let test_get_top () =
    let x = C.generate () in
    let t = Leaf x in
    assert (get_top t = x);
    let y = C.generate_gt x () in
    let t = OneBranch (x, y) in
    assert (get_top t = x);
    let z = C.generate_gt y () in
    let t = TwoBranch (Even, x, Leaf y, Leaf z) in
    assert (get_top t = x);
    let w = C.generate_gt z () in
    let t = TwoBranch (Odd, x, OneBranch (y, w), Leaf z) in
    assert (get_top t = x);
    let q = C.generate_gt w () in
    let t = TwoBranch (Even, x, OneBranch (y, w), OneBranch (z, q)) in
    assert (get_top t = x)

  let test_fix () =
    let x = C.generate () in
    let t = Leaf x in
    assert (fix t = t);
    let y = C.generate_gt x () in
    let t = OneBranch (y, x) in
    assert (fix t = OneBranch (x, y));
    let t = OneBranch (x, y) in
    assert (fix t = OneBranch (x, y));
    let z = C.generate_gt y () in
    let t = TwoBranch (Even, x, Leaf y, Leaf z) in
    assert (fix t = t);
    let t = TwoBranch (Even, z, Leaf x, Leaf y) in
    assert (fix t = TwoBranch (Even, x, Leaf z, Leaf y));
    let t = TwoBranch (Even, y, Leaf z, Leaf x) in
    assert (fix t = TwoBranch (Even, x, Leaf z, Leaf y));
    let w = C.generate_gt z () in
    let t = TwoBranch (Odd, x, OneBranch (y, w), Leaf z) in
    assert (fix t = t);
    let t = TwoBranch (Odd, z, OneBranch (x, y), Leaf w) in
    assert (fix t = TwoBranch (Odd, x, OneBranch (y, z), Leaf w));
    let t = TwoBranch (Odd, w, OneBranch (y, z), Leaf x) in
    assert (fix t = TwoBranch (Odd, x, OneBranch (y, z), Leaf w));
    let t = TwoBranch (Odd, w, OneBranch (x, z), Leaf y) in
    assert (fix t = TwoBranch (Odd, x, OneBranch (z, w), Leaf y));
    let q = C.generate_gt w () in
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
    let x = C.generate () in
    let y = C.generate_gt x () in
    let z = C.generate_gt y () in
    let w = C.generate_gt z () in
    let t = Tree (TwoBranch (Odd, x, OneBranch (y, w), Leaf z)) in
    assert (take t = (x, Tree (TwoBranch (Even, y, Leaf w, Leaf z))))


  let run_tests () =
    test_get_top ();
    test_fix ();
    test_take ()

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


(* end *)



open Array
exception Not_found
(* for the prioqs, we'll have some initializers *)


(* Nan is infinitity *)
 type  cost  = Nan | Cost of float;;
 type  adj_mat =  cost array array;;
 type 'a graph = { mutable ind : int;  (* ind is how many nodes have been added*)
                   size : int; (* max number of nodes *)
                   nodes : 'a array;  (* the actual array of nodes *)
                   m : adj_mat};; (* the costs between any two nodes *)


 let create_graph n s = 
   { ind = 0; size = s; nodes = Array.create s n; 
     m = Array.create_matrix s s Nan } ;;

(* this should be the return type
 val create_graph : 'a -> int -> 'a graph = <fun> *)

(*The function belongs_to checks if the node n is contained in the graph g. *)

 let belongs_to n g = 
   let rec aux i =
     (i < g.size) & ((g.nodes.(i) = n) or (aux (i+1)))
   in aux 0;;
(* val belongs_to : 'a -> 'a graph -> bool = <fun> *)


(*The function index returns the index of the node n in the graph g. If the node does not exist, a Not_found exception is thrown.*)
 let index n g = 
   let rec aux i = 
     if i >= g.size then raise (Failure "Not_found")
     else if g.nodes.(i) = n then i 
     else aux (i+1)
   in aux 0 ;;
 (* val index : 'a -> 'a graph -> int = <fun> *)


(*The next two functions are for adding nodes and edges of cost c to graphs.*)

let add_node n g = 
   if g.ind = g.size then failwith "the graph is full"
   else if belongs_to n g then failwith "the node already exists"
   (* we might not want this to fail but just not include it and keep
      going *)
   else (g.nodes.(g.ind) <- n; g.ind <- g.ind + 1) ;;

(* val add_node : 'a -> 'a graph -> unit = <fun> *)

let add_edge e1 e2 c g = 
  try
    let x = index e1 g and y = index e2 g in 
    g.m.(x).(y) <- Cost c 
  with Not_found -> failwith "node does not exist" ;;

(*    val add_edge : 'a -> 'a -> float -> 'a graph -> unit = <fun> *)


(*      Now it is easy to create a complete weighted directed graph starting with a list of nodes and edges. The function test_aho constructs the graph of figure 13.8:

*)

let test_aho () = 
  let g = create_graph "nothing" 5 in 
  List.iter (fun x -> add_node x g) ["A"; "B"; "C"; "D"; "E"];
  List.iter (fun (a,b,c) -> add_edge a b c g) 
    ["A","B",10.;
     "A","D",30.;
     "A","E",100.0;
     "B","C",50.;
     "C","E",10.;
     "D","C",20.;
     "D","E",60.];
  for i=0 to g.ind -1 do g.m.(i).(i) <- Cost 0.0 done;
  g;;

(*val test_aho : unit -> string graph = <fun> *)


let a = test_aho();;

 (*
val a : string graph =
  {ind=5; size=5; nodes=[|"A"; "B"; "C"; "D"; "E"|];
   m=[|[|Cost 0; Cost 10; Nan; Cost 30; Cost ...|]; ...|]} *)



(* ----- DIJK CODE _____ *)

type comp_state = { paths : int array;
                     already_treated : bool array;
                     distances : cost array;
                     source : int; 
                     nn : int};; (* nn is the total number of the graph's nodes *)
(* considered having the prioq as a part of this record *)

let create_state () =  { paths = [||]; already_treated = [||]; distances = [||];
                         nn = 0; source = 0};;

(* cost functions *)

 let a_cost c = match c with Nan -> false | _-> true;;
(* val a_cost : cost -> bool = <fun> *)

let float_of_cost c = match c with 
   Nan -> failwith "float_of_cost"
 | Cost x -> x;;
(*val float_of_cost : cost -> float = <fun> *)

let add_cost  c1 c2 = match (c1,c2) with 
   Cost x, Cost y -> Cost (x+.y)
 | Nan, Cost y -> c2
 | Cost x, Nan -> c1 
 | Nan, Nan ->  c1;;
(* val add_cost : cost -> cost -> cost = <fun> *)

 let less_cost  c1 c2 = match (c1,c2) with 
   Cost x, Cost y -> x < y
 | Cost x, Nan -> true
 | _, _ ->  false;;
(* val less_cost : cost -> cost -> bool = <fun> *)

exception Found of int;;

(* this is where we need to replace with our prioq *)
(* this is likely going to be deletemin from the prioq *)
(* also here we will insert the edges that are connected to the 
current node ?????????????????????????????????????????/ *)


(************************ CHANGE TO DELETE-MIN FROM PRIOQUEUE **************)
let first_not_treated cs = 
  try 
    for i=0 to cs.nn-1 do 
      if not cs.already_treated.(i) then raise (Found i)
    done;
    (*raise Not_found ;*) (* ask Willie about this *)(* gives a warning - why *)
    0
  with Found i -> i ;;

(* val first_not_treated : comp_state -> int = <fun> *)


(**************** CHANGE TO UPDATING VALUE IN PRIOQUEU ****************)
let least_not_treated p cs =
  let ni = ref p  
  and nd = ref cs.distances.(p) in 
  for i=p+1 to cs.nn-1 do 
    if not cs.already_treated.(i) then 
      if less_cost cs.distances.(i)  !nd then 
        ( nd := cs.distances.(i);
          ni := i )
  done;
  !ni,!nd;;

 exception No_way;;

 let one_round cs g = 
   (*********** DELETE MIN, update prev and dist array  *****************)
   let p = first_not_treated cs  in 
   (*********** get neighbors from graph ***********)
   let np,nc = least_not_treated p cs in
   if not(a_cost nc  ) then raise No_way 
   else 
     begin
       cs.already_treated.(np) <- true;
       (* for each neighbor *) 
       for i = 0 to cs.nn -1 do
         if not cs.already_treated.(i) then 
           if a_cost g.m.(np).(i) then
	     (* update priority queue values *) 
             let ic = add_cost cs.distances.(np)  g.m.(np).(i) in 
             if less_cost ic cs.distances.(i)   
	     then (cs.paths.(i) <- np;
               cs.distances.(i) <- ic) 
       done; cs
     end;;

(* Need to finish implementing prioq *) 
module IntListQueue = (ListQueue(NodeCompare) :
                        PRIOQUEUE with type elt = NodeCompare.t)
module IntHeapQueue = (BinaryHeap(NodeCompare) :
                        PRIOQUEUE with type elt = NodeCompare.t)
let list_module = (module IntListQueue : PRIOQUEUE with type elt = NodeCompare.t)
let heap_module = (module IntHeapQueue : PRIOQUEUE with type elt = NodeCompare.t)


let dij s g (pq : (module PRIOQUEUE with type elt=NodeCompare.t)) = 
  let module P = (val (pq) : PRIOQUEUE with type elt = NodeCompare.t) in
  (*let prioq = P.empty in*)
  (* make a prioq with all nodes in graph set to infinity *) 
  if belongs_to s g then 
    begin
      let i = index s g in 
      let cs = { paths = Array.create g.ind (-1) ;
                 already_treated = Array.create g.ind false;
                 distances = Array.create g.ind Nan;
                 nn = g.ind;
                 source = i}  in
      cs.already_treated.(i) <- true; 
      (* make this recursive *) 
      for j=0 to g.ind-1 do 
        let c = g.m.(i).(j) in 
        cs.distances.(j) <- c;
        if a_cost c then cs.paths.(j) <- i 
      done;
      try
        for k = 0 to cs.nn-2 do 
          ignore(one_round cs g) 
        done;
        cs
      with No_way -> cs
    end
  else failwith "dij: node unknown";;
(* val dij : 'a -> 'a graph -> comp_state = <fun> *)


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

(*val display_state :
  (out_channel -> 'a -> unit) -> 'a graph * comp_state -> 'a -> unit = <fun> *)


let g = test_aho ();;
let r = dij "A" g list_module;;

display_state (fun x y -> Printf.printf "%s!" y) (a,r) "E";; 


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

let sort (m : (module PRIOQUEUE with type elt=NodeCompare.t)) (lst : int list) =
  let module P = (val (m) : PRIOQUEUE with type elt = NodeCompare.t) in

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
