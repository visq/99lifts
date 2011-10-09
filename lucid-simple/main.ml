(* simulation file *)
open Sim
let step, reset =
   let _m1080 = ref `Snil_ in
   let step = fun _x1079 -> main true true _x1079 false _m1080 in
   let reset = fun () -> _m1080 := `Snil_ in
   step, reset
let main _ = step ();;
(* simulation loop: sampled on 10 Hz *)
(* compiles with -custom unix.cma    *)
let periodic() =
   let _x = Unix.setitimer Unix.ITIMER_REAL
     {Unix.it_interval = 0.100000 ; Unix.it_value = 1.0 }
   in Sys.set_signal Sys.sigalrm (Sys.Signal_handle main);
   while true do Unix.sleep 1 done;;

 periodic();exit(0)
