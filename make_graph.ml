open Genlex

(* things for the parser *)
let keywords = [ "SIZE"; "NODE"; "EDGE"; "#"];;

let lex_line l = Genlex.make_lexer keywords (Stream.of_string l);;

let parse_line g s = match  s with parser
  | [< '(Genlex.Kwd  "SIZE"); '(Genlex.Int n) >] -> 
    g := create_graph "" n
  | [< '(Genlex.Kwd  "NODE"); '(Genlex.Ident name) >] -> 
    add_node name !g
  | [< '(Genlex.Kwd  "EDGE"); '(Genlex.Ident e1); 
       '(Genlex.Ident  e2); '(Genlex.Float c) >] -> 
    add_edge e1 e2 c !g
  | [< '(Genlex.Kwd  "#") >] -> ()
  | [<>] -> () ;;


(* actually creating the graph *)

(* our type definition for graph *)
(*
 edges : EdgeDict.dict ;
  num_nodes : int ;
index_to_node_map :
*)
let create_graph name = 
   let g = ref {edges= Edgedict.dict.empty; num_nodes=0; index_to_node_map = ???} in 
   let ic = open_in name in 
    try 
      print_string ("Loading "^name^": ");
      while true do 
       print_string "."; 
        let l = input_line ic in parse_line g (lex_line l)  
      done;
      !g
    with End_of_file -> print_newline(); close_in ic; !g ;;


(* construct the graph from the file *)
let b = create_graph "cs51_final/aho.dat" ;;
