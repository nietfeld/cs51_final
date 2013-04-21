(* from http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora125.html *)

(* this is a very exposed version of the graph implementation,
so we'll just make it into a modulde with a get_neighbors function *)

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


(* initializers for the prioq *)


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

let first_not_treated cs = 
  try 
    for i=0 to cs.nn-1 do 
      if not cs.already_treated.(i) then raise (Found i)
    done;
    (*raise Not_found ;*) (* ask Willie about this *)(* gives a warning - why *)
    0
  with Found i -> i ;;

(* val first_not_treated : comp_state -> int = <fun> *)


(* inserting into the prioq *)
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
(* val least_not_treated : int -> comp_state -> int * cost = <fun> *)


 exception No_way;;

 let one_round cs g = 
   let p = first_not_treated cs  in 
   let np,nc = least_not_treated p cs in
   if not(a_cost nc  ) then raise No_way 
   else 
     begin
       cs.already_treated.(np) <- true;
       for i = 0 to cs.nn -1 do
         if not cs.already_treated.(i) then 
           if a_cost g.m.(np).(i) then
             let ic = add_cost cs.distances.(np)  g.m.(np).(i) in 
             if less_cost ic cs.distances.(i)   
	     then (cs.paths.(i) <- np;
               cs.distances.(i) <- ic) 
       done; cs
     end;;
(*val one_round : comp_state -> 'a graph -> comp_state = <fun> *)


 let dij s g = 
   if belongs_to s g then 
     begin
       let i = index s g in 
       let cs = { paths = Array.create g.ind (-1) ;
                  already_treated = Array.create g.ind false;
                  distances = Array.create g.ind Nan;
                  nn = g.ind;
                  source = i}  in
       cs.already_treated.(i) <- true; 
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
let r = dij "A" g;;

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
