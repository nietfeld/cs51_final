																																																												

											exe_time dij_quick (create_comp_graph a) ss ;;
																																																																																																							let triangle = [(5.,0.); (-3.,3.); (1.,0.); (-3.,-3.); (5.,0.)] in
																																																																																																																																																																let tr =  rotate triangle a in 
																																																																																																																																																																let ttr = translate tr (mx,my) in 
																																																																																																																																																																let tt = List.map (function (x,y) -> (int_of_float x, int_of_float y)) ttr 
																																																																																																																																																																in
																																																																																																																																																																Graphics.fill_poly (Array.of_list tt);;
																																																																																																																																																															      val display_arrow : float * float -> float -> unit = <fun>




																																																																																																																																																																The position of the text indicating the weight of an edge depends on the angle of the edge.

																																																																																																																																																																    # let display_label (mx,my) a lab = 
																																																																																																																																																																	let (sx,sy) = Graphics.text_size lab in
																																																																																																																																																																	let pos = [ float(-sx/2),float(-sy) ] in 
																																																																																																																																																																	let pr = rotate pos a in
																																																																																																																																																																	let pt = translate pr (mx,my) in 
																																																																																																																																																																	let px,py = List.hd pt in 
																																																																																																																																																																	let ox,oy = Graphics.current_point () in 
																																																																																																																																																																	Graphics.moveto ((int_of_float mx)-sx-6) 
																																																																																																																																																																	  ((int_of_float my) );
																																																																																																																																																																	Graphics.draw_string lab;
																																																																																																																																																																	Graphics.moveto ox oy;;
																																																																																																																																																																      val display_label : float * float -> float -> string -> unit = <fun>




																																																																																																																																																																	The preceding functions are now used by the function display_edge. Parameters are the graphical interface gg, the nodes i and j, and the color (col) to use.

																																																																																																																																																																	    # let display_edge gg col i j = 
																																																																																																																																																																		let g = gg.cg.g in  
																																																																																																																																																																		let x,y = gg.main.Awi.x,gg.main.Awi.y in 
																																																																																																																																																																		if a_cost g.m.(i).(j) then (
																																																																																																																																																																		  let (a1,b1) = gg.pos.(i)
																																																																																																																																																																		  and (a2,b2) = gg.pos.(j) in 
																																																																																																																																																																		  let x0,y0 = x+a1,y+b1   and x1,y1 = x+a2,y+b2 in
																																																																																																																																																																		  let rxm =  (float(x1-x0)) /. 2.  and rym =  (float(y1-y0)) /. 2. in
																																																																																																																																																																		  let xm = (float x0) +. rxm and ym = (float y0) +. rym in
																																																																																																																																																																		  Graphics.set_color col;
																																																																																																																																																																		  Graphics.moveto x0 y0;
																																																																																																																																																																		  Graphics.lineto x1 y1;
																																																																																																																																																																		  let a =  atan2 rym rxm in 
																																																																																																																																																																		  display_arrow (xm,ym) a;
																																																																																																																																																																		  display_label (xm,ym) a 
																																																																																																																																																																		    (string_of_float(float_of_cost g.m.(i).(j))));;
																																																																																																																																																																	      val display_edge : 'a gg -> Graphics.color -> int -> int -> unit = <fun>




																																																																																																																																																																		Displaying a Path

																																																																																																																																																																		To display a path, all edges along the path are displayed. The graphical display of a path towards a destination uses the same technique as the textual display.

																																																																																																																																																																		    # let rec display_shortest_path gg col dest = 
																																																																																																																																																																			let g = gg.cg.g in 
																																																																																																																																																																			if belongs_to dest g then 
																																																																																																																																																																			  let d = index dest g in
																																																																																																																																																																			  let rec aux is = 
																																																																																																																																																																			    if is = gg.state.source then ()
																																																																																																																																																																			    else (
																																																																																																																																																																			      let old = gg.state.paths.(is) in 
																																																																																																																																																																			      display_edge gg col old is;
																																																																																																																																																																			      aux old )
																																																																																																																																																																			  in 
																																																																																																																																																																			  if not(a_cost gg.state.distances.(d)) then Printf.printf "no way\n"
																																																																																																																																																																			  else aux d;;
																																																																																																																																																																		      val display_shortest_path : 'a gg -> Graphics.color -> 'a -> unit = <fun>




																																																																																																																																																																			Displaying a Graph

																																																																																																																																																																			The function display_gg displays a complete graph. If the destination node is not empty, the path between the source and the destination is traced.

																																																																																																																																																																			#  let display_gg gg () =  
																																																																																																																																																																			     Awi.display_rect gg.main ();
																																																																																																																																																																			     for i=0 to gg.cg.g.ind -1 do 
																																																																																																																																																																			       for j=0 to gg.cg.g.ind -1 do 
																																																																																																																																																																				 if i<> j then display_edge gg (Graphics.black) i j 
																																																																																																																																																																			       done
																																																																																																																																																																			     done;
																																																																																																																																																																			     if snd gg.dest != Awi.empty_component then 
																																																																																																																																																																			       display_shortest_path gg Graphics.red (fst gg.dest);;
																																																																																																																																																																			   val display_gg : 'a gg -> unit -> unit = <fun>




																																																																																																																																																																			     The Node Component

																																																																																																																																																																			     The nodes still need to be drawn. Since the user is allowed to choose the source and destination nodes, we define a component for nodes.

																																																																																																																																																																				 The user's main action is choosing the end nodes of the path to be found. Thus a node must be a component that reacts to mouse clicks, using its state to indicate if it has been chosen as a source or destination. We choose the button component, which reacts to mouse clicks.

																																																																																																																																																																				     Node Actions

																																																																																																																																																																				     It is necessary to indicate node selection. To show this, the background color of a node is changed by the function inverse.

																																																																																																																																																																				   # let inverse b = 
																																																																																																																																																																				       let gc = Awi.get_gc b in 
																																																																																																																																																																				       let fcol = Awi.get_gc_fcol gc 
																																																																																																																																																																				       and bcol = Awi.get_gc_bcol gc in 
																																																																																																																																																																				       Awi.set_gc_bcol gc fcol;
																																																																																																																																																																				       Awi.set_gc_fcol gc bcol;;
																																																																																																																																																																				     val inverse : Awi.component -> unit = <fun>




																																																																																																																																																																				       The function action_click effects this selection. It is called when a node is clicked on by the mouse. As parameters it takes the node associated with the button and the graph to modify the source or the destination of the search. When both nodes are selected, the function dij_quick finds a least cost path.
																																																																																																																																																																				     
																																																																																																																																																																				     # let action_click node gg b bs = 
																																																																																																																																																																					 let (s1,s) = gg.src
																																																																																																																																																																					 and (s2,d) = gg.dest in 
																																																																																																																																																																					 if  s == Awi.empty_component then (
																																																																																																																																																																					   gg.src  <- (node,b); inverse b )
																																																																																																																																																																					 else 
																																																																																																																																																																					   if d == Awi.empty_component then ( 
																																																																																																																																																																					     inverse b;
																																																																																																																																																																					     gg.dest <- (node,b); 
																																																																																																																																																																					     gg.state <- dij_quick s1 gg.cg;
																																																																																																																																																																					     display_shortest_path gg (Graphics.red) node
																																																																																																																																																																					   ) 
																																																																																																																																																																					   else (inverse s; inverse d;
																																																																																																																																																																						 gg.dest <- (s2,Awi.empty_component);
																																																																																																																																																																						 gg.src <- node,b; inverse b);;
																																																																																																																																																																				       val action_click : 'a -> 'a gg -> Awi.component -> 'b -> unit = <fun>




																																																																																																																																																																					 Creating an Interface

																																																																																																																																																																					 The main function to create an interface takes an interface graph and a list of options, creates the different components and associates them with the graph. The parameters are the graph (gg), its dimensions (gw and gh), a list of graph and node options (lopt) and a list of node border options (lopt2).
																																																																																																																																																																					     
																																																																																																																																																																					     # let main_gg gg gw gh lopt lopt2 = 
																																																																																																																																																																						 let gc = Awi.make_default_context () in 
																																																																																																																																																																						 Awi.set_gc gc lopt; 
     (* compute the maximal button size *)
     let vs = Array.map gg.to_string gg.cg.g.nodes in 
     let vsize = Array.map Graphics.text_size  vs in 
       let w = Array.fold_right (fun (x,y) -> max x)  vsize 0 
       and h = Array.fold_right (fun (x,y) -> max y)  vsize 0 in   
 (* create the main panel *)   
       gg.main <- Awi.create_panel true gw gh lopt;
       gg.main.Awi.display <- display_gg gg;
 (* create the buttons *)
         let vb_bs = 
           Array.map (fun x -> x,Awi.create_button (" "^(gg.to_string x)^" ") 
                        lopt)
                      gg.cg.g.nodes in 
           let f_act_b = Array.map (fun (x,(b,bs)) -> 
                                    let ac = action_click x  gg b
                                    in Awi.set_bs_action bs ac)  vb_bs in
           let bb = 
             Array.map (function (_,(b,_)) -> Awi.create_border b lopt2) vb_bs 
           in 
             Array.iteri 
               (fun i (b) -> let x,y = gg.pos.(i) in 
                               Awi.add_component gg.main  b 
                                 ["PosX",Awi.Iopt (x-w/2); 
                                  "PosY", Awi.Iopt (y-h/2)]) bb;
           ();;
val main_gg :
  'a gg ->
  int ->
  int -> (string * Awi.opt_val) list -> (string * Awi.opt_val) list -> unit =
  <fun>


The buttons are created automatically. They are positioned on the main window.

Testing the Interface

Everything is ready to create an interface now. We use a graph whose nodes are character strings to simplify the conversion functions. We construct the graph gg as follows:

# let id x = x;;
# let pos =  [| 200, 300; 80, 200 ; 100, 100; 200, 100;  260, 200 |];; 
# let gg = create_gg  (create_comp_graph (test_aho())) pos id id;;
# main_gg  gg 400 400 ["Background", Awi.Copt  (Graphics.rgb 130 130 130);
             "Foreground",Awi.Copt  Graphics.green] 
 [ "Relief", Awi.Sopt "Top";"Border_size", Awi.Iopt 2];;
*)
