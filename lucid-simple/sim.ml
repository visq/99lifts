type ('a3, 'a2, 'a1)
       _state_1020 = {mutable _init977:'a3;
                      mutable _pre976:'a2; mutable _pre975:'a1;}
and ('a10, 'a9, 'a8, 'a7, 'a6, 'a5, 'a4, 'a3, 'a2, 'a1)
      _state_1039 = {mutable _1038:'a10;
                     mutable _pre978:'a9;
                     mutable _1037:'a8;
                     mutable _init982:'a7;
                     mutable _pre981:'a6;
                     mutable _pre980:'a5;
                     mutable _init927:'a4;
                     mutable _init926:'a3;
                     mutable _pre921:'a2; mutable _pre919:'a1;}
and ('a7, 'a6, 'a5, 'a4, 'a3, 'a2, 'a1)
      _state_1073 = {mutable _1072:'a7;
                     mutable _1071:'a6;
                     mutable _1070:'a5;
                     mutable _1069:'a4;
                     mutable _init993:'a3;
                     mutable _pre992:'a2; mutable _pre991:'a1;}
and state__917 =  | State__916Off | State__916On | State__916Stop
and ('a2, 'a1) _state_1047 = {mutable _1046:'a2; mutable _init984:'a1;}
and ('a2, 'a1) _state_1062 = {mutable _init988:'a2; mutable _pre987:'a1;}

let sample _cl68 _70__n _923 _self_1019 =
      let _self_1019 = match !_self_1019 with
                         | `St_1022(_self_1019) -> _self_1019
                         | _ ->
                             (let _1021 = {_init977 = true;
                                           _pre976 = 0; _pre975 = 0} in
                              _self_1019 := `St_1022(_1021); _1021)
                          in
      let _974 = ref false in
      let _72__ok = ref false in
      let _71__cpt = ref 0 in
      (if _cl68 then
        (_974 := (or) _923 _self_1019._init977;
         _71__cpt :=
           (if !_974 then 0 else
             (if (=) _self_1019._pre975 ((-) _70__n 1) then 0 else
               (+) _self_1019._pre976 1));
         _72__ok := (=) !_71__cpt 0));
      _self_1019._init977 <- (&) !_974 (not _cl68);
      (if _cl68 then
        (_self_1019._pre976 <- !_71__cpt; _self_1019._pre975 <- !_71__cpt));
      !_72__ok

let sensor_width = 1

let end_pos = ( * ) Data.one_level 15

let stop_ticks = 40

let ticks_per_imp = 4

let ticks_per_imp_stop = 50

let env
    _cl390
    _395__motor_on _396__dir _397__initial_pos _398__load_pos _924 _self_1036 =
      let _self_1036 = match !_self_1036 with
                         | `St_1041(_self_1036) -> _self_1036
                         | _ ->
                             (let _1040 = {_1038 = ref `Snil_;
                                           _pre978 = 0;
                                           _1037 = ref `Snil_;
                                           _init982 = true;
                                           _pre981 = 0;
                                           _pre980 = 0;
                                           _init927 = true;
                                           _init926 = true;
                                           _pre921 = false;
                                           _pre919 = State__916Off} in
                              _self_1036 := `St_1041(_1040); _1040)
                          in
      let _cl995__ = ref false in
      let _cl994__ = ref false in
      let _404__l = ref false in
      let _403__b = ref false in
      let _402__t = ref false in
      let _401__i = ref false in
      let _979 = ref false in
      let _400__pos = ref 0 in
      let _399__impulse = ref ((), false) in
      let _918 = ref State__916Off in
      (if _cl390 then
        (_979 := (or) _924 _self_1036._init982;
         _918 := (if !_979 then State__916Off else _self_1036._pre919);
         (match !_918 with
           | State__916Off ->
               ((match _395__motor_on with
                  | true ->
                      (_self_1036._pre921 <- true;
                       _self_1036._pre919 <- State__916On)
                  | _ ->
                      (_self_1036._pre921 <- false;
                       _self_1036._pre919 <- !_918)
                  );
                _self_1036._init927 <- (or) _924 _self_1036._init927;
                _self_1036._init926 <- (or) _924 _self_1036._init926)
           | State__916On ->
               (let _934 = (or) _924 _self_1036._init926 in
                let _935 = (or) _self_1036._pre921 _934 in
                (if _cl390 then _cl994__ := true);
                (let _405__runclock = sample
                                        !_cl994__
                                        ticks_per_imp _935 _self_1036._1037 in
                 (if (&&) _405__runclock true then
                   _399__impulse := ((), true));
                 (match not _395__motor_on with
                   | true ->
                       (_self_1036._pre921 <- true;
                        _self_1036._pre919 <- State__916Stop)
                   | _ ->
                       (_self_1036._pre921 <- false;
                        _self_1036._pre919 <- !_918)
                   );
                 _self_1036._init927 <- (or) _924 _self_1036._init927;
                 _self_1036._init926 <- false))
           | State__916Stop ->
               (let _940 = (or) _924 _self_1036._init927 in
                let _941 = (or) _self_1036._pre921 _940 in
                let _406__ticks = if _941 then 0 else
                                    (+) _self_1036._pre978 1 in
                (if _cl390 then _cl995__ := true);
                (let _407__stopclock = sample
                                         !_cl995__
                                         ticks_per_imp_stop
                                         _941 _self_1036._1038 in
                 (if (&&) _407__stopclock true then
                   _399__impulse := ((), true));
                 (match ((&)
                           (not _395__motor_on) ((==) _406__ticks stop_ticks),
                         _395__motor_on) with
                   | (_, true) ->
                       (_self_1036._pre921 <- true;
                        _self_1036._pre919 <- State__916On)
                   | (true, _) ->
                       (_self_1036._pre921 <- true;
                        _self_1036._pre919 <- State__916Off)
                   | _ ->
                       (_self_1036._pre921 <- false;
                        _self_1036._pre919 <- !_918)
                   );
                 _self_1036._pre978 <- _406__ticks;
                 _self_1036._init927 <- false;
                 _self_1036._init926 <- (or) _924 _self_1036._init926))
           );
         _401__i := (if snd !_399__impulse then true else false);
         _400__pos :=
           (if snd !_399__impulse then
             (if !_979 then _397__initial_pos else
               (+)
                 _self_1036._pre980
                 (if (==) _396__dir Lift.Up then 1 else (~-) 1))
             else (if !_979 then _397__initial_pos else _self_1036._pre981));
         _402__t := (>=) !_400__pos ((-) end_pos sensor_width);
         _403__b := (<=) !_400__pos sensor_width;
         _404__l := (<=) (abs ((-) !_400__pos _398__load_pos)) sensor_width));
      _self_1036._init982 <- (&) !_979 (not _cl390);
      (if _cl390 then
        (_self_1036._pre981 <- !_400__pos; _self_1036._pre980 <- !_400__pos));
      ((!_402__t, !_403__b, !_404__l, !_401__i), !_400__pos)

let environment
    _cl466
    _467__motor_dir
    _468__motor_in _469__initial_pos _470__load_pos _948 _self_1045 =
      let _self_1045 = match !_self_1045 with
                         | `St_1049(_self_1045) -> _self_1045
                         | _ ->
                             (let _1048 = {_1046 = ref `Snil_;
                                           _init984 = true} in
                              _self_1045 := `St_1049(_1048); _1048)
                          in
      let _983 = ref false in
      let _997 = ref ((false, false, false, false), 0) in
      let _cl996__ = ref false in
      (if _cl466 then
        (_983 := (or) _948 _self_1045._init984;
         _cl996__ := true;
         _997 :=
           env
             !_cl996__
             _467__motor_dir
             _468__motor_in
             _469__initial_pos _470__load_pos !_983 _self_1045._1046));
      (let ((_471__t, _472__b, _473__l, _474__i), _475__pos) = !_997 in
       _self_1045._init984 <- (&) !_983 (not _cl466);
       ({Lift.sense_top = _471__t;
         Lift.sense_bot = _472__b;
         Lift.sense_load = _473__l; Lift.sense_impulse = _474__i},
        _475__pos))

let interface _cl603 _604__key _949 _self_1053 =
      let _611__top = ref false in
      let _609__top = ref ((), false) in
      let _608__up = ref ((), false) in
      let _607__load = ref ((), false) in
      let _606__down = ref ((), false) in
      let _605__bottom = ref ((), false) in
      let _615__bottom = ref false in
      let _614__down = ref false in
      let _613__load = ref false in
      let _612__up = ref false in
      (if _cl603 then
        ((match _604__key with | Data.None -> ()
           | Data.Some(_610__v) ->
               (match _610__v with | 't' -> _609__top := ((), true)
                 | 'u' -> _608__up := ((), true)
                 | 'l' -> _607__load := ((), true)
                 | 'd' -> _606__down := ((), true)
                 | 'b' -> _605__bottom := ((), true) | _ -> () )
           );
         _611__top := snd !_609__top;
         _612__up := snd !_608__up;
         _613__load := snd !_607__load;
         _614__down := snd !_606__down; _615__bottom := snd !_605__bottom));
      {Lift.go_top = !_611__top;
       Lift.go_up = !_612__up;
       Lift.go_load = !_613__load;
       Lift.go_down = !_614__down; Lift.go_bot = !_615__bottom}

let at_level _cl635 _639__l =
      let _998 = ref 0 in
      (if _cl635 then _998 := ( * ) Data.one_level ((+) _639__l 1)); !_998

let draw_lift _cl668 _671__st _966 _self_1061 =
      let _self_1061 = match !_self_1061 with
                         | `St_1064(_self_1061) -> _self_1061
                         | _ ->
                             (let _1063 = {_init988 = true; _pre987 = (0, 0)} in
                              _self_1061 := `St_1064(_1063); _1063)
                          in
      let _985 = ref () in
      let _986 = ref false in
      (if _cl668 then
        (_986 := (or) _966 _self_1061._init988;
         _985 :=
           Data.draw_lift_impl
             _671__st (if !_986 then _671__st else _self_1061._pre987)));
      _self_1061._init988 <- (&) !_986 (not _cl668);
      (if _cl668 then _self_1061._pre987 <- _671__st); !_985

let main _cl901 _cl791 () _967 _self_1068 =
      let _self_1068 = match !_self_1068 with
                         | `St_1075(_self_1068) -> _self_1068
                         | _ ->
                             (let _1074 = {_1072 = ref `Snil_;
                                           _1071 = ref `Snil_;
                                           _1070 = ref `Snil_;
                                           _1069 = ref `Snil_;
                                           _init993 = true;
                                           _pre992 = Obj.magic ();
                                           _pre991 = false} in
                              _self_1068 := `St_1075(_1074); _1074)
                          in
      let _1014 = ref () in
      let _1013 = ref () in
      let _1012 = ref () in
      let _1011 = ref () in
      let _905__load_pos = ref 0 in
      let _1010 = ref () in
      let _1009 = ref () in
      let _903__btns = ref (Obj.magic ()) in
      let _1008 = ref () in
      let _902__key = ref (Obj.magic ()) in
      let _1007 = ref (Obj.magic (), (0, false), Obj.magic ()) in
      let _1006 = ref
                    (false,
                     Obj.magic (), (Obj.magic (), (0, false), Obj.magic ())) in
      let _cl1005__ = ref false in
      let _1004 = ref (Obj.magic (), 0) in
      let _cl1003__ = ref false in
      let _cl1002__ = ref false in
      let _cl1001__ = ref false in
      let _1000 = ref (0, 0) in
      let _cl999__ = ref false in
      let _990 = ref false in
      let _989 = ref () in
      let _915___1837 = ref () in
      let _cl1015__ = ref false in
      (if _cl901 then
        (_990 := (or) _967 _self_1068._init993;
         _902__key := (if !_990 then Data.None else Data.keyboard ());
         _cl999__ := true;
         _903__btns := interface !_cl999__ !_902__key !_990 _self_1068._1069));
      (let (_904__cx, _) = !_1000 in
       (if _cl901 then
         (_cl1001__ := true;
          _905__load_pos := at_level !_cl1001__ 3;
          _cl1002__ := true;
          _cl1003__ := true;
          _1004 :=
            environment
              !_cl1002__
              (if !_990 then false else _self_1068._pre991)
              (if !_990 then Lift.Down else _self_1068._pre992)
              (at_level !_cl1003__ 2) !_905__load_pos !_990 _self_1068._1070));
       (let (_906__sensors, _907__lift_pos) = !_1004 in
        (if _cl901 then
          (_cl1005__ := true;
           _1006 :=
             Lift.lift
               !_cl1005__ !_903__btns _906__sensors !_990 _self_1068._1071));
        (let (_908__motor_on, _909__motor_dir, _910__obs) = !_1006 in
         (if _cl901 then _1007 := _910__obs);
         (let (_911__cmd, (_912__cpos, _913__abs), _914__level) = !_1007 in
          (if _cl901 then _1008 := print_string "liftpos = ");
          (let _ = !_1008 in
           (if _cl901 then _1009 := print_int _907__lift_pos);
           (let _ = !_1009 in
            (if _cl901 then _1010 := print_string ", ctrlpos = ");
            (let _ = !_1010 in
             (if _cl901 then _1011 := print_int _912__cpos);
             (let _ = !_1011 in
              (if _cl901 then
                _1012 :=
                  (if _913__abs then print_string "" else print_string " r"));
              (let _ = !_1012 in
               (if _cl901 then _1013 := print_string ",level = ");
               (let _ = !_1013 in
                (if _cl901 then
                  _1014 := print_int (Data.default ((~-) 1) _914__level));
                (let _ = !_1014 in
                 (if _cl901 then
                   ((match _911__cmd with
                      | Lift.Stop -> _915___1837 := print_string ", STOP"
                      | Lift.Go(Lift.Up) ->
                          _915___1837 := print_string ", UP"
                      | Lift.Go(Lift.Down) ->
                          _915___1837 := print_string ", DOWN"
                      );
                    (let _ = !_915___1837 in
                     let _ = print_newline () in
                     _cl1015__ := true;
                     _989 :=
                       draw_lift
                         !_cl1015__
                         (_907__lift_pos, !_905__load_pos)
                         !_990 _self_1068._1072)));
                 _self_1068._init993 <- (&) !_990 (not _cl901);
                 (if _cl901 then
                   (_self_1068._pre992 <- _909__motor_dir;
                    _self_1068._pre991 <- _908__motor_on));
                 !_989)))))))))))