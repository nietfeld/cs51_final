 (* performance eval functions *)

 (* function to do the time calculations *)
let exe_time f g ss =
  let t0 = Sys.time() in
  Printf.printf "Start: \n" t0;
  List.iter (fun s -> ignore(f s g)) ss;
  let t1 = Sys.time() in
  Printf.printf "End \n" t1;
  Printf.printf "Duration = \n" (t1 -. t0) ;;


(* create a random list of 2000 nodes and measure the performacne of graph a *)
let ss = 
  let ss0 = ref [] in
  let i0 = int_of_char 'A' in
  let new_s i = Char.escaped (char_of_int (i0+i)) in
  for i=0 to 20000 do ss0 := (new_s (Random.int a.size))::!ss0 done;
  !ss0 ;;


(* not really sure at which point graph a got generated *)
Printf.printf"Function dij :\n"; exe_time dij a ss ;;
