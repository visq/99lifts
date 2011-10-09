open Graphics

(* levels *)
let one_level = 5 (* 58 *);;  (* simulation is more fun with fewer ticks per level *)

(* option type *)
type 'a option = None | Some of 'a

let is_some v =
  match v with
   |Some _ -> true
   |None   -> false;;

let default d v =
  match v with
   |Some x -> x
   |None -> d;;

(* keyboard input *)
let keyboard () =
  let status = wait_next_event [Poll] in
  if status.keypressed then 
    let key = read_key () in
    Some(key)
  else None;;

let button_down () = button_down ()

let draw_line col x0 x1 y0 y1 =
  set_color col;
  moveto x0 y0;
  lineto x1 y1;;

let y_pos_scale = 5;;
let x_base = 100;;
let y_base = 30;;
let y_end  = y_base + one_level * y_pos_scale * 16;;
let draw_lift_bg () =
  (* static background at x = x_base*)
  (* draw the axis line *)
  draw_line foreground (x_base - 5) (x_base + 5) y_base y_base;
  draw_line foreground x_base x_base y_base y_end;
  (* at each level, draw a circle *)
  for i=1 to 15 do draw_circle x_base (y_base + one_level * y_pos_scale * i) 5 done;
  ;;

let draw_lift_anim (color_lift,color_load) (lift_pos, load_pos) =
  (* lift *)
  set_color color_lift;
  draw_circle (x_base + 20) (y_base + y_pos_scale * lift_pos) 8;
  (* load *)
  set_color color_load;
  draw_circle (x_base + 10) (y_base + y_pos_scale * load_pos) 5;
  ;;

(* l is the current lift/load and ll is the previous one *)
let draw_lift_impl l ll =
  draw_lift_anim (background, background) ll;
  draw_lift_anim (red, blue) l;
  synchronize ();;

(* mouse input *)
let get_load_request() =
  let cx,cy   = mouse_pos() in 
  let pressed = button_down() in
  if pressed & (cx > x_base - 50) & (cx < x_base + 50) & (cy >= y_base - 10) & (cy <= y_end + 10) 
    then Some ((cy-y_base)/y_pos_scale)
    else None;;

(* initialization *)
open_graph "";;
auto_synchronize false;;
draw_lift_bg ();;
set_line_width 1;;
