(* from http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora125.html *)

(* TODO:
    - take care of abstracting the dictionary implementation
    - make sure dij uses a prioq
*)
open Prio_q
open Array

exception Not_found

 type  cost  = Nan | Cost of float;; (* Nan is infinitity *)
 type  adj_mat =  cost array array;;
 type 'a graph = { mutable ind : int;  (* ind is how many nodes have been added*)
                   size : int; (* max number of nodes *)
                   nodes : 'a array;  (* the actual array of nodes *)
                   m : adj_mat};; (* the costs between any two nodes *)

 let create_graphf n s = 
   { ind = 0; size = s; nodes = Array.create s n; 
     m = Array.create_matrix s s Nan } ;;

(*The function belongs_to checks if the node n is contained in the graph g. *)
 let belongs_to n g = 
   let rec aux i =
     (i < g.size) & ((g.nodes.(i) = n) or (aux (i+1)))
   in aux 0;;

(*The function index returns the index of the node n in the graph g. If the node does not exist, a Not_found exception is thrown.*)
 let index n g = 
   let rec aux i = 
     if i >= g.size then raise (Failure "Not_found")
     else if g.nodes.(i) = n then i 
     else aux (i+1)
   in aux 0 ;;

(*The next two functions are for adding nodes and edges of cost c to graphs.*)
let add_node n g = 
   if g.ind = g.size then failwith "the graph is full"
   else if belongs_to n g then failwith "the node already exists"
   (* we might not want this to fail but just not include it and keep
      going *)
   else (g.nodes.(g.ind) <- n; g.ind <- g.ind + 1) ;;

let add_edge e1 e2 c g = 
  try
    let x = index e1 g and y = index e2 g in 
    g.m.(x).(y) <- Cost c 
  with Not_found -> failwith "node does not exist" ;;

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

let create_state () =  { paths = [||]; already_treated = [||]; distances = [||];
                         nn = 0; source = 0};;

(* cost functions *)

let a_cost c = match c with Nan -> false | _-> true;;

let float_of_cost c = match c with 
   Nan -> failwith "float_of_cost"
 | Cost x -> x;;

let add_cost  c1 c2 = match (c1,c2) with 
   Cost x, Cost y -> Cost (x+.y)
 | Nan, Cost y -> c2
 | Cost x, Nan -> c1 
 | Nan, Nan ->  c1;;

 let less_cost  c1 c2 = match (c1,c2) with 
   Cost x, Cost y -> x < y
 | Cost x, Nan -> true
 | _, _ ->  false;;

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
    (*raise Not_found ;*) (* ask about this *)(* gives a warning - why *)
    0
  with Found i -> i ;;

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

let g = test_aho ();;
let r = dij "A" g list_module;;

display_state (fun x y -> Printf.printf "%s!" y) (a,r) "E";; 
(* this is what should return *)

