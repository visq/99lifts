(* $Id: data.mli,v 1.2 2006/04/06 11:00:52 pouzet Exp $ *)
type 'a option = None | Some of 'a
val is_some : 'a option -> bool
val default : 'a -> 'a option -> 'a
val keyboard : unit -> char option
val one_level : int
val draw_lift_impl : (int * int) -> (int * int) -> unit
val get_cursor : unit -> (int * int)