module type SET = 
sig
  type elt
  type set

  val empty : set
  val is_empty : set -> bool
  val insert : elt -> set -> set
  val singleton : elt -> set
  val union : set -> set -> set
  val intersect : set -> set -> set
  val remove : elt -> set -> set
  val member : set -> elt -> bool
  val choose : set -> (elt * set) option
  val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a
  val string_of_set : set -> string
  val string_of_elt : elt -> string
  val run_tests : unit -> unit
end

module type COMPARABLE = 
sig
  type t
  val compare : t -> t -> Order.order
  val string_of_t : t -> string
  val gen : unit -> t
  val gen_random : unit -> t
  val gen_gt : t -> unit -> t
  val gen_lt : t -> unit -> t
  val gen_between : t -> t -> unit -> t option
end

module IntComparable : COMPARABLE =
struct
  open Order
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Eq
  let string_of_t = string_of_int
  let gen () = 0
  let gen_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)
  let gen_gt x () = x + 1
  let gen_lt x () = x - 1
  let gen_between x y () = 
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end

module ListSet(C: COMPARABLE) : (SET with type elt = C.t) = 
struct
  open Order
  type elt = C.t 
  type set = elt list

  let empty = []

  let is_empty xs = 
    match xs with 
      | [] -> true
      | _ -> false

  let singleton x = [x]

  let rec insert x xs = 
    match xs with 
      | [] -> [x]
      | y::ys -> (match C.compare x y with 
          | Greater -> y::(insert x ys)
          | Eq -> xs
          | Less -> x::xs)

  let union xs ys = List.fold_right insert xs ys

  let rec remove y xs = 
    match xs with 
      | [] -> []
      | x::xs1 -> (match C.compare y x with 
          | Eq -> xs1
          | Less -> xs
          | Greater -> x::(remove y xs1))

  let rec intersect xs ys = 
    match xs, ys with 
      | [], _ -> []
      | _, [] -> []
      | xh::xt, yh::yt -> (match C.compare xh yh with 
          | Eq -> xh::(intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt)

  let rec member xs x = 
    match xs with 
      | [] -> false
      | y::ys -> (match C.compare x y with
          | Eq -> true
          | Greater -> member ys x
          | Less -> false)

  let choose xs = 
    match xs with 
      | [] -> None
      | x::rest -> Some (x,rest)
  let fold f e = List.fold_left (fun a x -> f x a) e 
    
  let string_of_elt = C.string_of_t
  let string_of_set (s: set) : string = 
    let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
    "set([" ^ (List.fold_left f "" s) ^ "])"

  let insert_list (d: set) (lst: elt list) : set = 
    List.fold_left (fun r k -> insert k r) d lst

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts ;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ;
    ()

  let test_union () =
    ()

  let test_intersect () =
    ()

  let test_member () =
    ()

  let test_choose () =
    ()

  let test_fold () =
    ()

  let test_is_empty () =
    ()

  let test_singleton () =
    ()

  let run_tests () = 
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()

end

module IntListSet = ListSet(IntComparable) ;;
IntListSet.run_tests();;


(******************************************************************)
(* DictSet: a functor that creates a SET by calling our           *)
(* Dict.Make functor                                              *)
(******************************************************************)

module DictSet(C : COMPARABLE) : (SET with type elt = C.t) = 
struct

  module D = Dict.Make

    (struct
      type key = C.t 
      type value = unit

      let compare = C.compare 

      let string_of_key = C.string_of_t
      let string_of_value = fun () -> ""

      let gen_key = C.gen
      let gen_key_random = C.gen_random
      let gen_key_gt = C.gen_gt
      let gen_key_lt = C.gen_lt
      let gen_key_between = C.gen_between
      let gen_value= fun () -> ()
      let gen_pair = fun () -> (gen_key (), gen_value ())
     end)

  type elt = D.key
  type set = D.dict

  let empty = D.empty

  let fold f = D.fold (fun k v a -> f k a)

  let member = D.member 

  let insert e s = D.insert s e ()

  let singleton e = insert e empty

  let remove a b = D.remove b a

  let choose x =
    match D.choose x with
    | None -> None
    | Some (k, v, d) -> Some (k, d)

  let union = fold insert

  let intersect one two = fold (fun x y ->
    if member one x then insert x y else y) empty two
        
  let is_empty x = x = empty

  let string_of_elt = D.string_of_key
  let string_of_set s = D.string_of_dict s

  let run_tests () = 
    ()
end

module IntDictSet = DictSet(IntComparable) ;;
IntDictSet.run_tests();;


(******************************************************************)
(* Make: a functor that creates a SET by calling our              *)
(* ListSet or DictSet functors                                    *)
(******************************************************************)
module Make(C : COMPARABLE) : (SET with type elt = C.t) = 
  DictSet (C)
