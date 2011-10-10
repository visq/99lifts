type 'a option = None | Some of 'a
val is_some : 'a option -> bool
val default : 'a -> 'a option -> 'a
val keyboard : unit -> char option
val one_level : int
val end_pos : int
val draw_lift_impl : (int * int * int) -> (int * int * int) -> unit
val get_load_request : unit -> int option
