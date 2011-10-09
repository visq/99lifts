(*
Adapted from the java Lift benchmark in the JOP project (www.jopdesign.com)
Copyright (C) 2001-2008, Martin Schoeberl (martin@jopdesign.com)
              2011, Benedikt Huber (benedikt.huber@gmail.com)
*)

(* Differences to the Java version:
   - A simpler approximation is used for checkLevel, which does not require loops.
     We could use ocaml for the check_level loop, but this has not been implemented yet
   - For simplicity reasons, the low-level conversion from signals to bitmasks and the corresponding voter
     have been left out
*)

open Data

(* Synchronous Utility functions *)

(* [edge x] returns the rising edge *)
let node edge x = false -> not (pre x) & x

(* [signal c v] emits signal value v if the x is true *)
let node signal c v = o where
  rec clock clk = c and emit o = v when clk

(* types *)

(* direction *)
type dir         = Up   | Down

let reverse_dir dir = match dir with Up -> Down | Down -> Up end

(* relative or absolute position *)
let get_absolute_ticks (ticks,abs) = if abs then Some ticks else None
let get_pos_ticks (ticks,abs)      = ticks

(* motor command *)
type motor_command      = Stop | Go of dir

(* button inputs *)
type buttons =
  { go_top : bool
  ; go_up : bool
  ; go_load : bool
  ; go_down : bool
  ; go_bot : bool }

(* sensor inputs *)
type sensors =
  { sense_top : bool
  ; sense_bot : bool
  ; sense_load : bool
  ; sense_impulse: bool}


(* constants *)
let ms_per_tick = 10
let startup_ticks = 200 (* 500 *)  / ms_per_tick
let stop_ticks = 200    (* 500 *)  / ms_per_tick
let predicted_stop_ticks = 2
let start_pos = 0

(* Data computations are are not directly supported by lucid and would need to be implemented in ocaml *)
(* Therefore, we use a simpler approximation for now *)
let get_level pos =
  match pos with
     (_,false) -> None
    |(p,true)  -> Some (1 + (p+(one_level/2))/one_level)
  end

(* Check whether the current position is below the absolute position we last observed the load *)
let below_load pos load_pos =
  match (pos, load_pos) with
      (_, None) | ((_,false), _) -> false
      | ((p,true), Some lp) -> p < lp
  end

(* Command Logic:
     Keeps track of the status of the current action
     If there is no active action and the motor is idle, sets action depending on the button pressed (weak preemption)
     If there is an active action, check sensors and stop the lift if appropriate
     XXX: This node is a tad to large, maybe there is a sensible refactoring ?
*)
let node command btns sensors pos motor_busy = (cmd, reset_pos) where
  rec automaton
    Idle -> 
      do cmd = Stop
      unless (motor_busy) then Busy
      until (is_some search_pending) then GoLoad (default Down search_pending)
      until btns.go_top  then GoEnd Up
      until btns.go_bot  then GoEnd Down
      until btns.go_up   then GoPos (Up,   get_pos_ticks pos + one_level - predicted_stop_ticks)
      until btns.go_down then GoPos (Down, get_pos_ticks pos - one_level + predicted_stop_ticks)
      until btns.go_load then GoLoad (if below_load pos load_level then Up else Down)
    |Busy ->
      do cmd = Stop until (not motor_busy) then Idle
    |SensedEnd dir ->
      do match dir with
            Down -> do emit reset_pos = start_pos done
           |Up   -> do done
         end
      until true then Busy
    |GoEnd(dir) ->
      do cmd = Go dir until (sense_end dir) then SensedEnd dir
    |GoPos(dir,targetPos) ->
      let curpos = get_pos_ticks pos in
      do cmd = Go dir
      until (sense_end dir) then SensedEnd dir
      until (if dir == Up then curpos >= targetPos else curpos <= targetPos) then Busy
    |GoLoad(dir) ->
      do cmd = Go dir
      until sensors.sense_load then FoundLoad       (* found load *)
      until (sense_end dir) then SensedEndLoad dir  (* did not find load in this direction *)
    |FoundLoad ->
      do  load_level = (get_absolute_ticks pos) 
      and search_pending = None
      until true then Busy
    |SensedEndLoad dir ->
      do search_pending =
         match last search_pending with
            None ->   Some (reverse_dir dir)
           |Some _ -> None
         end
      until true then SensedEnd dir
  end (* rec automaton *)
  and last search_pending = None
  and last load_level = None
  and sense_end dir = if dir == Up then sensors.sense_top else sensors.sense_bot

(* Motor Control Logic: 
    on motor start, set direction, wait for startup, start motor and emit motor started
    on motor stop, stop motor, wait for stop, emit motor stopped
*)
let node motor_control cmd = (motor_on, motor_dir, motor_busy) where
  rec automaton
     Init ->
      (* using weak preemption in the initial state, all output values are initialized. *)
      (* therefore, partial assignments are ok in the other states (Manual 1.6.6) *)
      do (motor_on, motor_dir, motor_busy) = (false, Down, false) until true then Idle
    |Idle -> 
      do motor_busy = false
      until (cmd != Stop) then Start (match cmd with Go dir -> dir | Stop -> Down end)

    |Start(dir) ->
      let rec ticks = 0 -> pre ticks + 1 in
      do  (motor_on, motor_dir, motor_busy) = (false, dir, true)
      until (cmd == Stop) then Idle
      until (ticks == startup_ticks) then Running

    |Running ->
      do motor_on = true until (cmd = Stop) then Stop

    |Stop ->
      let rec ticks = 0 -> pre ticks + 1 in
      do  motor_on = false
      until (ticks == stop_ticks) then Idle

  end (* rec automaton *)

(* Position Logic:
    if a impuls_signal is present (rising edge on the impuls sensor and motor is active),
    then increment or decrement the position
    if a reset_signal is present (recalibration),
    then set the position
*)
let node position impuls_signal reset_signal motor_dir = (p,abs) where
  rec
  present
      reset_signal(v) -> do (p,abs) = (v, true) done
    | impuls_signal()   ->
          do match motor_dir with 
               Up   -> do (p,abs) = (last p + 1, last abs)  done
             | Down -> do (p,abs) = (last p - 1, last abs)  done
             end
          done
  end
  and last p   = 0
  and last abs = false

(* impulse signal from sensor and motor busy samples *)
let node impulse_signal impulse_sensor motor_active =
  signal (edge impulse_sensor & motor_active) ()

(* System:
     Sensors: Load Sensor, 2 position sensors, 1 rotation sensor, 5 buttons
     Output: motor on/off and motor direction
     Observed: command, position and level
     Controllers:
       command:  issues high-level commands depending on the sensor input
       motor:    controls the motor, processing the high-level commands
       position: keeps track of the position of the lift
*)
let node lift btns sensors = (motor_on, motor_dir, (cmd, pos, level)) where
  rec (cmd, reset_signal)               = command btns sensors ((0,false) -> pre pos) motor_busy
  and (motor_on, motor_dir, motor_busy) = motor_control (Stop -> pre cmd)
  and pos                               = position (impulse_signal sensors.sense_impulse motor_busy) reset_signal motor_dir
  and level                             = get_level pos

(* High-Level Position/Level Logic:
    Initially, the level is unknown (relative position)
    If we sense either top or bottom, the position is recalibrated
    As long as neither top nor bottom are sensed, we have a relative position
    If top or 
*)
(* Display Logic 
     if the absolute position is known, compute the current level
     otherwise the level is unknown (displayed as 0 in the original Lift implementation)
*)


(* Simulation drivers *)
let node command_demo b1 b2 b3 b4 b5 s1 s2 s3 pos pos_abs motor_state = (* Demo for lucys *)
  let (cmd,pos_sig) = command {go_top=b1;go_bot=b2;go_up=b3;go_down=b4;go_load=b5;} 
                              {sense_top=s1;sense_bot=s2;sense_load=s3;sense_impulse=false;}
                              (pos,pos_abs) motor_state in
  match cmd with
      Stop    -> 0
     |Go Up   -> 1
     |Go Down -> -1
  end
  
(* Impulse sensor signal on rising edge if motor is active *)
let node position_demo impulse_sensor motor_active bot_sensor is_up =  (* Demo for lucys *)
       position (impulse_signal impulse_sensor motor_active) (signal bot_sensor 0) (if is_up then Up else Down)

