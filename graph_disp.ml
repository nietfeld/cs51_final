open Graphics;;

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
