(* load awi.cmo ?? where do we get that *)

type 'a gg = { mutable src : 'a * Awi.component; 
               mutable dest : 'a * Awi.component;
               pos : (int * int) array;
               cg : 'a comp_graph;
               mutable state : comp_state;
               mutable main : Awi.component;
               to_string : 'a -> string;
               from_string : string -> 'a } ;;

let create_gg cg vpos ts fs = 
  {src = cg.g.nodes.(0),Awi.empty_component;
   dest = cg.g.nodes.(0),Awi.empty_component;
   pos = vpos;
   cg = cg;
   state = create_state () ;
   main = Awi.empty_component;
   to_string = ts;
   from_string = fs};;

(* the part for drawing the edges *)
let rotate l a = 
  let ca = cos a  and sa = sin a in 
  List.map (function (x,y) -> ( x*.ca +. -.y*.sa, x*.sa +. y*.ca)) l;;

let translate l (tx,ty) = 
  List.map (function (x,y) -> (x +. tx, y +. ty)) l;;

 let display_arrow (mx,my) a = 
   let triangle = [(5.,0.); (-3.,3.); (1.,0.); (-3.,-3.); (5.,0.)] in
   let tr =  rotate triangle a in 
   let ttr = translate tr (mx,my) in 
   let tt = List.map (function (x,y) -> (int_of_float x, int_of_float y)) ttr 
   in
   Graphics.fill_poly (Array.of_list tt);;

(* the position of the text indicating the weight of an edge *)
 let display_label (mx,my) a lab = 
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
