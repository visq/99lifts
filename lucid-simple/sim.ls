(* lift: simulation *)
open Lift
open Graphics
open Data

(* utilities *)

(* a counter that counts modulo n *)
let node sample n =
  let rec cpt = 0 -> if pre cpt = n - 1 then 0 else pre cpt + 1
  and ok = cpt = 0 in
  ok

(* constants *)
let sensor_width   = 1
let end_pos        = one_level * 15 (* 864 *)
let stop_ticks     = 40
let ticks_per_imp  = 4       (* emit tick every 30 ms *)
let ticks_per_imp_stop = 50  (* emit every 300 ms (0-1 before 400ms) *) 

(* very simple environment simulation simulation 
   TODO: decent motor simulation *)
let node env motor_on dir initial_pos load_pos =
  ((t,b,l,i),pos) where
  rec automaton
     Off ->
      do until motor_on then On
    |On ->
      let clock runclock  = sample ticks_per_imp in
      do  emit impulse = () when runclock
      until (not motor_on) then Stop
    |Stop ->
      let rec ticks = 0 -> pre ticks + 1 in
      let clock stopclock = sample ticks_per_imp_stop in
      do  emit impulse = () when stopclock
      until (motor_on)                           then On
      until (not motor_on & ticks == stop_ticks) then Off
  end (* rec automaton *)
  and pos = if ?impulse
              then initial_pos -> pre pos + (if dir == Up then 1 else (-1))
              else initial_pos -> pre pos
  and i   = if ?impulse then true else false
  and t   = pos >= end_pos - sensor_width
  and b   = pos <= sensor_width
  and l   = abs (pos - load_pos) <= sensor_width

let node environment motor_dir motor_in initial_pos load_pos =
  ({sense_top=t;sense_bot=b;sense_load=l;sense_impulse=i;},pos)
  where ( (t,b,l,i), pos) = env motor_dir motor_in initial_pos load_pos 
  
(* make the interface for manual testing *)
(* all the inputs are given through the keyboard *)
let node interface key =
  let
      match key with
        None -> do done
      | Some(v) ->
          do match v with
               't' -> do emit top = () done
             | 'u' -> do emit up = () done
             | 'l' -> do emit load = () done
             | 'd' -> do emit down = () done
             | 'b' -> do emit bottom = () done
             | _ -> do done
             end
          done
      end in
  let top    = ? top
  and up     = ? up
  and load   = ? load
  and down   = ? down
  and bottom = ? bottom
  in {go_top=top;go_up=up;go_load=load;go_down=down;go_bot=bottom;}

let at_level l = one_level * (l+1)

(* plot lift *)
let node draw_lift st =
  draw_lift_impl st (st -> pre st)

let node main () =
  (* read the keyboard *)
  let key = None -> keyboard () in
  let btns = interface key in
  (* read mouse *)
  let load_req = get_load_request() in
  let load_pos = pos where
    rec pos = (match load_req with None -> last pos | Some p -> p end)
    and last pos = at_level 3
  in
  (* simulate *)
  let rec (sensors,lift_pos) = environment (false -> pre motor_on) (Down -> pre motor_dir) (at_level 2) load_pos
  and                          (motor_on, motor_dir, obs) = lift btns sensors in
  (* observation *)
  let (cmd,(cpos,abs),level) = obs in

  (* console output *)
  print_string "liftpos = ";
  print_int lift_pos;
  print_string ", ctrlpos = ";
  print_int cpos;
  if abs
    then print_string ""
    else print_string " r";
  print_string ",level = ";
  print_int (default (-1) level);
  match cmd with
     Stop -> print_string ", STOP"
    |Go Up -> print_string ", UP"
    |Go Down -> print_string ", DOWN"
  end;
  print_newline ();
  
  (* plot ouput *)
  draw_lift (lift_pos,load_pos)
