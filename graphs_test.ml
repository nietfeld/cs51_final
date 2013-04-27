open Graphs

module NamedGraph = 
struct
  include(Graph(struct
                  type node = string
                  let compare = Order.string_compare
                  let string_of_node = fun x -> x
                  let gen () = ""
                end))
  let from_edges (es: (string *  float * string) list) : graph =
    List.fold_left (fun g (src, wt, dst) -> add_edge g src dst wt) empty es
end

module TestGraph = 
struct 
  module G = NamedGraph

  let g = G.add_edge G.empty "a" "b" 3.;;
  let g2 = G.add_edge g "a" "c" 4.;;

  let deopt_len lo =
    match lo with
      | None -> 0
      | Some xs -> List.length xs;;

  let deopt_lst lo =
    match lo with
      | None -> []
      | Some xs -> xs;;

  let deopt_node no =
    match no with
      | None -> "None"
      | Some n -> n;;

  let _ = (
    assert (G.has_node g "a");
    assert (G.has_node g "b");
    assert (G.has_node g "c" = false);
    assert (G.has_node g2 "c");
    assert (G.has_node g2 "d" = false);

    assert (List.length (G.nodes G.empty) = 0) ;
    assert (List.length (G.nodes (G.add_node G.empty "a")) = 1) ;

    assert (List.length (G.nodes g) = 2) ;

    assert (List.length (G.nodes g2) = 3) ;

    assert (deopt_len (G.outgoing_edges g2 "a") = 2) ;
    assert (deopt_len (G.outgoing_edges g2 "b") = 0) ;
    assert (deopt_len (G.outgoing_edges g2 "c") = 0) ;
    assert (G.outgoing_edges g2 "d" = None) ;

    assert (deopt_len (G.neighbors g2 "a") = 2) ;
    assert (deopt_len (G.neighbors g2 "b") = 0) ;
    assert (deopt_len (G.neighbors g2 "c") = 0) ;
    assert (G.neighbors g2 "d" = None) ;

    assert (let t = deopt_lst (G.neighbors g2 "a") in
              t = [("b", 3.);("c",4.)] or t = [("c",4.);("b",3.)]) )

  let g3 = G.from_edges [("a",2.,"b");("a",3.,"c");("b",4.,"d");
		       ("d",5.,"e");("d",6.,"f");("h",8.,"e");
		       ("g",7.,"f");("h",9.,"g")] ;;

  assert(let a = deopt_lst (G.neighbors g3 "a") in 
	 a = [("b",2.);("c",3.)] or a = [("c",3.);("b",2.)]) ;
  assert(deopt_lst (G.neighbors g3 "b") = [("d",4.)]) ;
  assert(deopt_lst (G.neighbors g3 "c") = []) ;
  assert(let d = deopt_lst (G.neighbors g3 "d") in 
	 d = [("e",5.);("f",6.)] or d = [("f",6.);("e",5.)]) ;
  assert(deopt_lst (G.neighbors g3 "e") = []) ;
  assert(deopt_lst (G.neighbors g3 "f") = []) ;
  assert(deopt_lst (G.neighbors g3 "g") = [("f",7.)]) ;
  assert(let h = deopt_lst (G.neighbors g3 "h") in 
	 h = [("e",8.);("g",9.)] or h = [("g",9.);("e",8.)]) 

  let tester oelist : unit =
    match oelist with
    | None -> print_string ""
    | Some s -> print_string (List.fold_right (fun (dst, wt, src) y -> 
      dst ^ " & " ^ string_of_float wt ^ " & " ^ src ^ " ; " ^ y) s "")
;;
end
