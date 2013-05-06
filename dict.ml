module type DICT = 
sig
  type key   
  type value 
  type dict

  val empty : dict 
  val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a
  val lookup : dict -> key -> value option
  val member : dict -> key -> bool
  val insert : dict -> key -> value -> dict
  val remove : dict -> key -> dict
  val choose : dict -> (key * value * dict) option
  val string_of_key: key -> string
  val string_of_value : value -> string
  val string_of_dict : dict -> string
  val run_tests : unit -> unit
end

module type DICT_ARG =
sig
  type key
  type value
  val compare : key -> key -> Order.order
  val string_of_key : key -> string
  val string_of_value : value -> string

  val gen_key : unit -> key
  val gen_key_random : unit -> key
  val gen_key_gt : key -> unit -> key
  val gen_key_lt : key -> unit -> key
  val gen_key_between : key -> key -> unit -> key option
  val gen_value : unit -> value
  val gen_pair : unit -> key * value

end

(******************************************************************)
(* BTDict: a functor that implements our DICT signature           *)
(* using a balanced tree (2-3 trees)                              *)
(******************************************************************)

module BTDict (D:DICT_ARG) : (DICT with type key = D.key
with type value = D.value) =
struct
  open Order

  type key = D.key
  type value = D.value

  type pair = key * value

  type dict = 
    | Leaf
    | Two of dict * pair * dict
    | Three of dict * pair * dict * pair * dict

  type kicked =
    | Up of dict * pair * dict
    | Done of dict

  type hole =
    | Hole of pair option * dict
    | Absorbed of pair option * dict

  type direction2 =
    | Left2
    | Right2

  type direction3 =
    | Left3
    | Mid3
    | Right3
        
  let empty : dict = Leaf

  let rec fold (f: key -> value -> 'a -> 'a) (u: 'a) (d: dict) : 'a =
    match d with
    | Leaf -> u
    | Two (l, (k, v), r) -> f k v (fold f (fold f u r) l) 
    | Three(l, (k1, v1), m, (k2, v2), r) ->
      f k2 v2 (f k1 v1 (fold f (fold f (fold f u r) m) l))

  let string_of_key = D.string_of_key
  let string_of_value = D.string_of_value
  let rec string_of_dict (d: dict) : string =
    match d with
    | Leaf -> ""
    | Two (a, (x, y), c) -> string_of_dict a ^ "(" ^ string_of_key x ^ 
      "," ^ string_of_value y ^ ") " ^ string_of_dict c
    | Three (a, (x, y), c, (x1, y1), r) ->
      string_of_dict a ^ "(" ^ string_of_key x ^ 
      "," ^ string_of_value y ^ ") " ^ string_of_dict c ^ "(" ^
	string_of_key x1 ^  "," ^ string_of_value y1 ^ ") " ^
	string_of_dict r
      
  let rec string_of_tree (d: dict) : string = 
    match d with
      | Leaf -> "Leaf"
      | Two(left,(k,v),right) -> "Two(" ^ (string_of_tree left) 
        ^ ",(" ^ (string_of_key k) ^ "," ^ (string_of_value v) ^ "),"
        ^ (string_of_tree right) ^ ")"
      | Three(left,(k1,v1),middle,(k2,v2),right) -> 
        "Three(" ^ (string_of_tree left)
        ^ ",(" ^ (string_of_key k1) ^ "," ^ (string_of_value v1) ^ "),"
        ^ (string_of_tree middle) ^ ",(" ^ (string_of_key k2) ^ "," 
        ^ (string_of_value v2) ^ ")," ^ (string_of_tree right) ^ ")"

  let insert_upward_two (w: pair) (w_left: dict) (w_right: dict) 
      (x: pair) (x_other: dict) : kicked = 
    let (wk, _), (xk, _) = w, x in
    match D.compare wk xk with
    | Greater -> Done(Three(x_other, x, w_left, w, w_right))
    | Less | Eq -> Done(Three(w_left, w, w_right, x, x_other))    

  let insert_upward_three (w: pair) (w_left: dict) (w_right: dict)
      (x: pair) (y: pair) (other_left: dict) (other_right: dict) : kicked =
    let (wk, _), (xk, _), (yk, _) = w, x, y in
    match D.compare wk xk with
    | Less | Eq -> Up(Two(w_left,w,w_right),x,Two(other_left,y,other_right))
    | Greater ->
      match D.compare wk yk with
      | Less | Eq-> Up(Two(other_left,x,w_left),w,Two(w_right,y,other_right))
      | Greater -> Up(Two(other_left,x,other_right),y,Two(w_left,w,w_right))

  let rec insert_downward (d: dict) (k: key) (v: value) : kicked =
    match d with
      | Leaf -> Up(Leaf,(k,v),Leaf)
      | Two(left, (k2, v2),right) ->
	insert_downward_two (k,v) (k2,v2) left right
      | Three(left,(xk,xv),middle,(yk,yv),right) ->
    	insert_downward_three (k,v) (xk,xv) (yk,yv) left middle right
	  
  and insert_downward_two ((k,v): pair) ((k1,v1): pair) 
      (left: dict) (right: dict) : kicked = 
    match D.compare k k1 with
    | Greater ->
      let dright = insert_downward right k v in
      (match dright with
      | Up (l,w,r)-> insert_upward_two w l r (k1,v1) left  
      | Done d -> Done (Two(left,(k1,v1), d)))
    | Less ->
      let dleft = insert_downward left k v in
      (match dleft with
      | Up (l,w,r)-> insert_upward_two w l r (k1,v1) right  
      | Done d -> Done(Two (d,(k1,v1),right)))
    | Eq ->
      Done (Two(left, (k,v), right))
	
  and insert_downward_three ((k,v): pair) ((k1,v1): pair) ((k2,v2): pair) 
      (left: dict) (middle: dict) (right: dict) : kicked =
    match D.compare k k1 with
    | Eq -> Done (Three(left,(k,v),middle,(k2,v2),right))
    | Less ->
      let dleft = insert_downward left k v in
      (match dleft with
      | Up (l,w,r) -> insert_upward_three w l r (k1,v1) (k2,v2) middle right
      | Done d -> Done (Three(d,(k1,v1),middle,(k2,v2),right)))
    | Greater ->
      match D.compare k k2 with
      | Eq -> Done (Three(left,(k1,v1),middle,(k,v),right))
      | Less ->
	let dmiddle = insert_downward middle k v in
	(match dmiddle with
	| Up (l,w,r) -> insert_upward_three w l r (k1,v1) (k2,v2) left right
	| Done d -> Done(Three(left,(k1,v1),d,(k2,v2),right)))
      | Greater -> 
	let dright = insert_downward right k v in
	match dright with
	| Up (l,w,r) -> insert_upward_three w l r (k1,v1) (k2,v2) left middle
	| Done d -> Done (Three(left,(k1,v1),middle,(k2,v2),d))

  let insert (d: dict) (k: key) (v: value) : dict =
    match insert_downward d k v with
    | Up(l,(k1,v1),r) -> Two(l,(k1,v1),r)
    | Done x -> x
      
  let remove_upward_two (n: pair) (rem: pair option) 
      (left: dict) (right: dict) (dir: direction2) : hole =
    match dir,n,left,right with
      | Left2,x,l,Two(m,y,r) -> Hole(rem,Three(l,x,m,y,r))
      | Right2,y,Two(l,x,m),r -> Hole(rem,Three(l,x,m,y,r))
      | Left2,x,a,Three(b,y,c,z,d) ->
	Absorbed (rem, Two (Two (a,x,b), y, Two(c,z,d)))
      | Right2,z,Three(a,x,b,y,c),d ->
	Absorbed (rem, Two (Two (a,x,b), y, Two(c,z,d)))
      | Left2,_,_,_ | Right2,_,_,_ -> Absorbed(rem,Two(Leaf,n,Leaf))

  let remove_upward_three (n1: pair) (n2: pair) (rem: pair option)
      (left: dict) (middle: dict) (right: dict) (dir: direction3) : hole =
    match dir,n1,n2,left,middle,right with
      | Left3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,y,z,Two(a,x,b),c,d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,x,y,a,b,Two(c,z,d) -> Absorbed(rem,Two(a,x,Three(b,y,c,z,d)))
      | Right3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(a,x,Three(b,y,c,z,d)))
      | Left3,w,z,a,Three(b,x,c,y,d),e ->
	Absorbed(rem,Three(Two(a,w,b),x,Two(c,y,d),z,e))
      | Mid3,y,z,Three(a,w,b,x,c),d,e ->	
	Absorbed(rem,Three(Two(a,w,b),x,Two(c,y,d),z,e))
      | Mid3,w,x,a,b,Three(c,y,d,z,e) ->
	Absorbed(rem,Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Right3,w,z,a,Three(b,x,c,y,d),e ->
	Absorbed(rem,Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Left3,_,_,_,_,_ | Mid3,_,_,_,_,_ | Right3,_,_,_,_,_ ->
        Absorbed(rem,Three(Leaf,n1,Leaf,n2,Leaf))

  let rec remove_downward (d: dict) (k: key) : hole =
    match d with
      | Leaf -> Absorbed(None,d)
      | Two(Leaf,(k1,v1),Leaf) ->
        (match D.compare k k1 with
          | Eq -> Hole(Some(k1,v1),Leaf)
          | Less | Greater -> Absorbed(None,d)
        )
      | Three(Leaf,(k1,v1),Leaf,(k2,v2),Leaf) ->
        (match D.compare k k1, D.compare k k2 with
          | Eq, _ -> Absorbed(Some(k1,v1),Two(Leaf,(k2,v2),Leaf))
          | _, Eq -> Absorbed(Some(k2,v2),Two(Leaf,(k1,v1),Leaf))
          | _, _ -> Absorbed(None,d)
        )
      | Two(l,n,r) -> remove_downward_two k n l r
      | Three(l,n1,m,n2,r) -> remove_downward_three k n1 n2 l m r

  and remove_downward_two (k: key) ((k1,v1): pair) 
      (left: dict) (right: dict) : hole =
    match D.compare k k1 with
      | Eq ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,left)
          | Hole(Some n,new_right) -> 
            remove_upward_two n None left new_right Right2
          | Absorbed(None,_) -> Hole(None,left)
          | Absorbed(Some n,new_right) -> Absorbed(None,Two(left,n,new_right))
        )
      | Less -> 
        (match remove_downward left k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,(k1,v1),right))
        )
      | Greater ->
        (match remove_downward right k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem left t Right2
          | Absorbed(rem,t) -> Absorbed(rem,Two(left,(k1,v1),t))
        )

  and remove_downward_three (k: key) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : hole =
    match D.compare k k1, D.compare k k2 with
      | Eq, _ ->
        (match remove_min middle with
          | Hole(None,_) -> Hole(None,Two(left,(k2,v2),right))
          | Hole(Some n,new_middle) -> 
            remove_upward_three n (k2,v2) None left new_middle right Mid3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),right))
          | Absorbed(Some n,new_middle) -> 
            Absorbed(None,Three(left,n,new_middle,(k2,v2),right))
        )
      | _ , Eq ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,Two(left,(k1,v1),middle))
          | Hole(Some n,new_right) -> 
            remove_upward_three (k1,v1) n None left middle new_right Right3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),middle))
          | Absorbed(Some n,new_right) -> 
            Absorbed(None,Three(left,(k1,v1),middle,n,new_right))
        )
      | Less, _ ->
        (match remove_downward left k with
          | Hole(rem,t) -> 
            remove_upward_three (k1,v1) (k2,v2) rem t middle right Left3
          | Absorbed(rem,t) -> 
            Absorbed(rem,Three(t,(k1,v1),middle,(k2,v2),right))
        )
      | _, Greater ->
        (match remove_downward right k with
          | Hole(rem,t) -> 
            remove_upward_three (k1,v1) (k2,v2) rem left middle t Right3
          | Absorbed(rem,t) -> 
            Absorbed(rem,Three(left,(k1,v1),middle,(k2,v2),t))
        )
      | Greater, Less ->
        (match remove_downward middle k with
          | Hole(rem,t) -> 
            remove_upward_three (k1,v1) (k2,v2) rem left t right Mid3
          | Absorbed(rem,t) -> 
            Absorbed(rem,Three(left,(k1,v1),t,(k2,v2),right))
        )

  (* DO NOT EDIT THIS *)
  and remove_min (d: dict) : hole =
    match d with
      | Leaf -> Hole(None,Leaf)
      | Two(Leaf,n,_) -> Hole(Some n,Leaf)
      | Three(Leaf,n1,middle,n2,right) -> Absorbed(Some n1,Two(middle,n2,right))
      | Two(left,n,right) -> 
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_two n rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,n,right))
        )
      | Three(left,n1,middle,n2,right) ->
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_three n1 n2 rem t middle right Left3
          | Absorbed(rem,t) -> Absorbed(rem,Three(t,n1,middle,n2,right))
        )

  let remove (d: dict) (k: key) : dict =
    match remove_downward d k with
      | Hole(_,d') -> d'
      | Absorbed(_,d') -> d'

  let rec lookup (d: dict) (k: key) : value option =
    match d with
    | Leaf -> None
    | Two(l,(a,b),r) ->
      (match D.compare k a with
      | Eq -> Some b
      | Less -> lookup l k
      | Greater -> lookup r k)
    | Three(l,(a,b),m,(c,d),r) ->
      match D.compare k a with
      | Eq -> Some b
      | Less -> lookup l k
      | Greater ->
	match D.compare k c with
	| Eq -> Some d
	| Less -> lookup m k
	| Greater -> lookup r k
       
  let rec member (d: dict) (k: key) : bool =
    match d with
    | Leaf -> false
    | Two(l,(a,b),r) ->
      (match D.compare k a with
      | Eq -> true 
      | Less -> member l k 
      | Greater -> member r k)
    | Three(l,(a,b),m,(c,d),r) ->
      match D.compare k a with
      | Eq -> true
      | Less -> member l k
      | Greater ->
	match D.compare k c with
	| Eq -> true
	| Less -> member m k
	| Greater -> member r k
        
  let choose (d: dict) : (key * value * dict) option =
    match d with
    | Leaf -> None
    | Two(l, (k,v), r) -> Some (k, v, remove d k)
    | Three(l, (k,v), m, x, r) -> Some (k, v, remove d k)

  let rec balanced (d: dict) : bool =
    let rec helper (d: dict) (h: int) : bool * int =
      match d with
      | Leaf -> (true, h)
      | Two (l,p,r) -> 
	let (lb, lh) = helper l (h + 1) in
	let (rb, rh) = helper r (h + 1) in
	(lh = rh && lb && rb, lh)
      | Three (l,p,m,p2,r) -> 
	let (lb, lh) = helper l (h + 1) in
	let (mb, mh) = helper m (h + 1) in
	let (rb, rh) = helper r (h + 1) in
	(lh = mh && mh = rh && lb && mb && rb, lh)
    in
    let (b, height) = helper d 0 in
    b


  let insert_list (d: dict) (lst: (key * value) list) : dict = 
    List.fold_left (fun r (k,v) -> insert r k v) d lst

  let insert_list_reversed (d: dict) (lst: (key * value) list) : dict =
    List.fold_right (fun (k,v) r -> insert r k v) lst d

  let generate_pair_list (size: int) : (key * value) list =
    let rec helper (size: int) (current: key) : (key * value) list =
      if size <= 0 then []
      else 
        let new_current = D.gen_key_gt current () in
        (new_current, D.gen_value()) :: (helper (size - 1) new_current)
    in
    helper size (D.gen_key ())

   let rec generate_random_list (size: int) : (key * value) list =
    if size <= 0 then []
    else 
      (D.gen_key_random(), D.gen_value()) :: (generate_random_list (size - 1))

  let test_balance () =
    let d1 = Leaf in
    assert(balanced d1) ;

    let d2 = Two(Leaf,D.gen_pair(),Leaf) in
    assert(balanced d2) ;

    let d3 = Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf) in
    assert(balanced d3) ;

    let d4 = Three(Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),
                       D.gen_pair(),Two(Two(Leaf,D.gen_pair(),Leaf),
                                        D.gen_pair(),
                                        Two(Leaf,D.gen_pair(),Leaf))),
                   D.gen_pair(),
                   Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),D.gen_pair(),
                       Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf))),D.gen_pair(),
                   Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),D.gen_pair(),
                       Three(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                             Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                             Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf))))
    in
    assert(balanced d4) ;

    let d5 = Two(Leaf,D.gen_pair(),Two(Leaf,D.gen_pair(),Leaf)) in
    assert(not (balanced d5)) ;

    let d6 = Three(Leaf,D.gen_pair(),
                   Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),Leaf) in
    assert(not (balanced d6)) ;

    let d7 = Three(Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf),
                   D.gen_pair(),Leaf,D.gen_pair(),Two(Leaf,D.gen_pair(),Leaf))
    in
    assert(not (balanced d7)) ;
    ()

  let test_insert () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    assert (balanced d1);
    let pairs1 = generate_pair_list 27 in
    let d1 = insert_list empty pairs1 in
    assert (balanced d1);
    let pairs1 = generate_pair_list 28 in
    let d1 = insert_list empty pairs1 in
    assert (balanced d1);
    let pairs1 = generate_pair_list 29 in
    let d1 = insert_list empty pairs1 in
    assert (balanced d1);
    let pairs1 = generate_pair_list 30 in
    let d1 = insert_list empty pairs1 in
    assert (balanced d1);
    let pairs1 = generate_pair_list 31 in
    let d1 = insert_list empty pairs1 in
    assert (balanced d1);
    let pairs1 = generate_pair_list 32 in
    let d1 = insert_list empty pairs1 in
    assert (balanced d1);
    ()
     
  let test_remove () =
    let pairs1 = generate_pair_list 5 in
    let d1 = insert_list empty pairs1 in
    let d2 =
      match choose d1 with
      | None -> Leaf
      | Some (a, b, c) -> c
    in
    assert (balanced d2);
    let pairs1 = generate_pair_list 15 in
    let d1 = insert_list empty pairs1 in
    let d2 =
      match choose d1 with
      | None -> Leaf
      | Some (a, b, c) -> c
    in
    assert (balanced d2);
    let pairs1 = generate_pair_list 25 in
    let d1 = insert_list empty pairs1 in
    let d2 =
      match choose d1 with
      | None -> Leaf
      | Some (a, b, c) -> c
    in
    assert (balanced d2);
    () 

  let test_remove_nothing () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    let r2 = remove d1 (D.gen_key_lt (D.gen_key()) ()) in
    List.iter (fun (k,v) -> assert(lookup r2 k = Some v)) pairs1 ;
    assert(balanced r2) ;
    ()

  let test_remove_from_nothing () =
    let d1 = empty in
    let r1 = remove d1 (D.gen_key()) in
    assert(r1 = empty) ;
    assert(balanced r1) ;
    ()

  let test_remove_in_order () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.iter 
      (fun (k,v) -> 
        let r = remove d1 k in
        let _ = List.iter 
          (fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          ) pairs1 in
        assert(balanced r)
      ) pairs1 ;
    ()

  let test_remove_reverse_order () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list_reversed empty pairs1 in
    List.iter 
      (fun (k,v) -> 
        let r = remove d1 k in
        let _ = List.iter 
          (fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          ) pairs1 in
        assert(balanced r)
      ) pairs1 ;
    ()

  let test_remove_random_order () =
    let pairs5 = generate_random_list 100 in
    let d5 = insert_list empty pairs5 in
    let r5 = List.fold_right (fun (k,_) d -> remove d k) pairs5 d5 in
    List.iter (fun (k,_) -> assert(not (member r5 k))) pairs5 ;
    assert(r5 = empty) ;
    assert(balanced r5) ;
    () 

  let run_tests () = 
    test_balance () ; 
    test_insert () ;
    test_remove ();

    test_remove_nothing() ;
    test_remove_from_nothing() ;
    test_remove_in_order() ; 
    test_remove_reverse_order() ;
    test_remove_random_order() ; 
    ()

end;;

module Make (D:DICT_ARG) : (DICT with type key = D.key
  with type value = D.value) = 
   BTDict(D) 
