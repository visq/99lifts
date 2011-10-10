open Graphics

(* levels *)
let num_levels = 15 ;;
let one_level = 5 (* 58 *);;                              (* simulation is more fun with fewer ticks per level *)
let end_pos   = one_level * (num_levels - 1) (* 864 *);;  (* simulation is more fun with fewer ticks per level *)

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
let y_end  = y_base + one_level * y_pos_scale * 15;;
let draw_lift_bg () =
  (* static background at x = x_base*)
  (* draw the axis line *)
  draw_line foreground x_base x_base y_base y_end;
  (* at each level, draw a horizontal line *)
  for i=1 to 15 do
    let y_off = (y_base + one_level * y_pos_scale * (i-1)) in 
    draw_line foreground (x_base - 5) (x_base + 5) y_off y_off
  done;
  ;;

let draw_lift_anim (color_lift, color_load, color_led) (lift_pos, load_pos, led_count) =
  (* lift *)
  set_color color_lift;
  draw_circle (x_base + 20) (y_base + y_pos_scale * lift_pos) 8;
  (* load *)
  set_color color_load;
  draw_circle (x_base + 10) (y_base + y_pos_scale * load_pos) 5;
  (* leds *)
  set_color color_led;
  for i=1 to led_count do draw_circle (x_base - 15) (y_base + one_level * y_pos_scale * (i-1)) 3 done;
  ;;
  
(* l is the current lift, load and led and ll is the previous one *)
let draw_lift_impl l (old_lift, old_load, old_leds) =
  draw_lift_anim (background, background, red) (old_lift, old_load, 15);
  draw_lift_anim (red, blue, green) l;
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
