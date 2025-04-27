open! Core
open! Angstrom
open! Standard_product_3.Sp3_parser

let expect_test_f (module F : Parseable) s =
  let unconsumed_to_sexp (u : Angstrom.Buffered.unconsumed) =
    [%sexp
      { buf : string = Bigstringaf.substring u.buf ~off:u.off ~len:u.len
      ; off : int = u.off
      ; len : int = u.len
      }]
  in
  let state = Angstrom.Buffered.parse F.parse in
  let state = Angstrom.Buffered.feed state (`String s) in
  let state = Angstrom.Buffered.feed state `Eof in
  let sexp =
    match state with
    | Done (unconsumed, result) ->
      [%sexp
        { unconsumed : Sexp.t = unconsumed_to_sexp unconsumed
        ; result : F.t
        }]
    | Partial _ ->
      Sexp.Atom "Partial"
    | Fail (unconsumed, sl, s) ->
      [%sexp
        { unconsumed : Sexp.t = unconsumed_to_sexp unconsumed
        ; sl : string list = sl
        ; s : string = s
        }]
  in
  print_s sexp

let expect_test_fs (module P : Parseable) s =
  let unconsumed_to_sexp (u : Angstrom.Buffered.unconsumed) =
    [%sexp
      { buf : string = Bigstringaf.substring u.buf ~off:u.off ~len:u.len
      ; off : int = u.off
      ; len : int = u.len
      }]
  in
  let state = Angstrom.Buffered.parse (F.lines P.parse) in
  let state = Angstrom.Buffered.feed state (`String s) in
  let state = Angstrom.Buffered.feed state `Eof in
  let sexp =
    match state with
    | Done (unconsumed, result) ->
      [%sexp
        { unconsumed : Sexp.t = unconsumed_to_sexp unconsumed
        ; result : P.t list
        }]
    | Partial _ ->
      Sexp.Atom "Partial"
    | Fail (unconsumed, sl, s) ->
      [%sexp
        { unconsumed : Sexp.t = unconsumed_to_sexp unconsumed
        ; sl : string list = sl
        ; s : string = s
        }]
  in
  print_s sexp

let expect_test_p (module P : Parseable) s =
  parse_string ~consume:All P.parse s
  |> [%sexp_of: (P.t, string) result]
  |> print_s

let expect_test_ps (module P : Parseable) s =
  parse_string ~consume:All (F.lines P.parse) s
  |> [%sexp_of: (P.t list, string) result]
  |> print_s

let%expect_test "Version Line" =
  expect_test_p (module Line.Version)
    "#dP2025  4 15 18  0  0.00000000     576 ORBIT IGb20 FIT JGX ";
  [%expect {|
    (Ok
     ((version D) (pos_or_vel Pos) (year 2025) (month 4) (day_of_month 15)
      (hour 18) (minute 0) (second 0) (number_of_epochs 576) (data_used ORBIT)
      (coordinate_system IGb20) (orbit_used FIT) (agency "JGX ")))
    |}]

let%expect_test "Time info Line" =
  expect_test_p (module Line.Time_info)
    "## 2362 237600.00000000   300.00000000 60780 0.7500000000000";
  [%expect {|
    (Ok
     ((gps_week 2362) (seconds_of_week 237600) (epoch_interval 300)
      (modified_julian_day 60780) (fractional_day 0.75)))
    |}]

let%expect_test "Space vehicles Line" =
  expect_test_p (module Line.Space_vehicles)
    "+   76   G02G03G04G05G06G07G08G09G10G11G12G13G14G15G16G17G18";
  [%expect {|
    (Ok
     ((number (76))
      (ids
       (((kind Gps) (prn 2)) ((kind Gps) (prn 3)) ((kind Gps) (prn 4))
        ((kind Gps) (prn 5)) ((kind Gps) (prn 6)) ((kind Gps) (prn 7))
        ((kind Gps) (prn 8)) ((kind Gps) (prn 9)) ((kind Gps) (prn 10))
        ((kind Gps) (prn 11)) ((kind Gps) (prn 12)) ((kind Gps) (prn 13))
        ((kind Gps) (prn 14)) ((kind Gps) (prn 15)) ((kind Gps) (prn 16))
        ((kind Gps) (prn 17)) ((kind Gps) (prn 18))))))
    |}]

let%expect_test "Space vehicles Line" =
  expect_test_p (module Line.Space_vehicles)
    "+        R04R05R07R08R09R11R12R14R15R16R17R18R19R20R21R22R24";
  [%expect {|
    (Ok
     ((number ())
      (ids
       (((kind Glonass) (prn 4)) ((kind Glonass) (prn 5))
        ((kind Glonass) (prn 7)) ((kind Glonass) (prn 8))
        ((kind Glonass) (prn 9)) ((kind Glonass) (prn 11))
        ((kind Glonass) (prn 12)) ((kind Glonass) (prn 14))
        ((kind Glonass) (prn 15)) ((kind Glonass) (prn 16))
        ((kind Glonass) (prn 17)) ((kind Glonass) (prn 18))
        ((kind Glonass) (prn 19)) ((kind Glonass) (prn 20))
        ((kind Glonass) (prn 21)) ((kind Glonass) (prn 22))
        ((kind Glonass) (prn 24))))))
    |}]

let%expect_test "Space vehicles Line" =
  expect_test_p (module Line.Space_vehicles)
    "+        E26E27E29E30E31E33E34E36  0  0  0  0  0  0  0  0  0";
  [%expect {|
    (Ok
     ((number ())
      (ids
       (((kind Galileo) (prn 26)) ((kind Galileo) (prn 27))
        ((kind Galileo) (prn 29)) ((kind Galileo) (prn 30))
        ((kind Galileo) (prn 31)) ((kind Galileo) (prn 33))
        ((kind Galileo) (prn 34)) ((kind Galileo) (prn 36))))))
    |}]

let%expect_test "Accuracy" =
  expect_test_p (module Line.Accuracy)
    "++         3  2  2  2  2  2  3  2  2  2  2  2  2  2  2  2  2";
  [%expect {|
    (Ok
     ((accuracy_exponent
       ((3) (2) (2) (2) (2) (2) (3) (2) (2) (2) (2) (2) (2) (2) (2) (2) (2)))))
    |}]

let%expect_test "Accuracy" =
  expect_test_p (module Line.Accuracy)
    "++         2  2  2  3  2  2  3  2  0  0  0  0  0  0  0  0  0";
  [%expect {|
    (Ok
     ((accuracy_exponent
       ((2) (2) (2) (3) (2) (2) (3) (2) () () () () () () () () ()))))
    |}]

let%expect_test "Type and time" =
  expect_test_p (module Line.Type_and_time)
{|%c G  cc GPS ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc
%c cc cc ccc ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc|};
  [%expect {| (Ok ((file_type (Only Gps)) (time_system Gps))) |}]

let%expect_test "Base" =
  expect_test_p (module Line.Base)
{|%f  1.2500000  1.025000000  0.00000000000  0.000000000000000
%f  0.0000000  0.000000000  0.00000000000  0.000000000000000|};
  [%expect {| (Ok ((position_velocity 1.25) (clock 1.025))) |}]

let%expect_test "Int empty" =
  expect_test_p (module Line.Int_empty)
    {|%i    0    0    0    0      0      0      0      0         0|};
  [%expect {| (Ok ()) |}]

let%expect_test "Comment" =
  expect_test_p (module Line.Comment)
    "/* PROGRAM: MADOCA v.2.2.0, DATE: 2025/04/16 19:26:00 UTC                       ";
  [%expect {| (Ok ()) |}]

let%expect_test "Comment" =
  expect_test_ps (module Line.Comment)
    {|/* Center for Orbit Determination in Europe (CODE)          
/* Ultra-rapid GRE orbits starting year-day 25089 00 hour   
/* Observed/predicted: 24/24 hours (data used up to 089X)   
/* PCV:IGS20      OL/AL:FES2014b NONE     YN ORB:CoN CLK:BRD|};
  [%expect {| (Ok (() () () ())) |}]

let%expect_test "Epoch" =
  expect_test_p (module Line.Epoch)
    "*  2025  4 15 18  0  0.00000000";
  [%expect {|
    (Ok
     ((year 2025) (month 4) (day_of_month 15) (hour 18) (minute 0) (second 0)))
    |}]

let%expect_test "Position and clock" =
  expect_test_p (module Line.Position_and_clock)
    "PG21      0.000000      0.000000      0.000000 999999.999999                    ";
  [%expect {| (Ok ((space_vehicle_id ((kind Gps) (prn 21))) (data ()))) |}]

let%expect_test "Position and clock" =
  expect_test_p (module Line.Position_and_clock)
    "PG01  -1103.209931  15790.801024 -21325.471767    198.406633";
  [%expect {|
    (Ok
     ((space_vehicle_id ((kind Gps) (prn 1)))
      (data
       (((x -1103.209931) (y 15790.801024) (z -21325.471767) (clock (198.406633))
         (x_stddev ()) (y_stddev ()) (z_stddev ()) (clock_stddev ())
         (clock_event false) (clock_pred false) (maneuver false)
         (orbit_pred false))))))
    |}]

let%expect_test "Position and clock" =
  expect_test_p (module Line.Position_and_clock)
    "PG20  22037.667290  -3713.935328  14357.943543    365.018958 14 12 12 407       ";
  [%expect {|
    (Ok
     ((space_vehicle_id ((kind Gps) (prn 20)))
      (data
       (((x 22037.66729) (y -3713.935328) (z 14357.943543) (clock (365.018958))
         (x_stddev (14)) (y_stddev (12)) (z_stddev (12)) (clock_stddev (407))
         (clock_event false) (clock_pred false) (maneuver false)
         (orbit_pred false))))))
    |}]


let%expect_test "Position and clock" =
  expect_test_p (module Line.Position_and_clock)
    "PE10  20406.922352   9750.415733  19090.538833   -703.665576  9 21 20      P   P";
  [%expect {|
    (Ok
     ((space_vehicle_id ((kind Galileo) (prn 10)))
      (data
       (((x 20406.922352) (y 9750.415733) (z 19090.538833) (clock (-703.665576))
         (x_stddev (9)) (y_stddev (21)) (z_stddev (20)) (clock_stddev ())
         (clock_event false) (clock_pred true) (maneuver false)
         (orbit_pred true))))))
    |}]

let%expect_test "Position and clock" =
  expect_test_p (module Line.Position_and_clock)
    "PG01   1026.879290 -15811.938317 -21315.322068    238.653084               P   P";
  [%expect {|
    (Ok
     ((space_vehicle_id ((kind Gps) (prn 1)))
      (data
       (((x 1026.87929) (y -15811.938317) (z -21315.322068) (clock (238.653084))
         (x_stddev ()) (y_stddev ()) (z_stddev ()) (clock_stddev ())
         (clock_event false) (clock_pred true) (maneuver false)
         (orbit_pred true))))))
    |}]

let%expect_test "Epoch block" =
  expect_test_fs (module Line.Position_and_clock)
    {|PG01  -1103.209931  15790.801024 -21325.471767    198.406633
PG02 -10450.357472  17370.707261 -16482.947527   -207.393848
PG03   7117.316147  20056.052947 -16137.962133    681.020785|};
  [%expect {|
    ((unconsumed ((buf "") (off 182) (len 0)))
     (result
      (((space_vehicle_id ((kind Gps) (prn 1)))
        (data
         (((x -1103.209931) (y 15790.801024) (z -21325.471767)
           (clock (198.406633)) (x_stddev ()) (y_stddev ()) (z_stddev ())
           (clock_stddev ()) (clock_event false) (clock_pred false)
           (maneuver false) (orbit_pred false)))))
       ((space_vehicle_id ((kind Gps) (prn 2)))
        (data
         (((x -10450.357472) (y 17370.707261) (z -16482.947527)
           (clock (-207.393848)) (x_stddev ()) (y_stddev ()) (z_stddev ())
           (clock_stddev ()) (clock_event false) (clock_pred false)
           (maneuver false) (orbit_pred false)))))
       ((space_vehicle_id ((kind Gps) (prn 3)))
        (data
         (((x 7117.316147) (y 20056.052947) (z -16137.962133)
           (clock (681.020785)) (x_stddev ()) (y_stddev ()) (z_stddev ())
           (clock_stddev ()) (clock_event false) (clock_pred false)
           (maneuver false) (orbit_pred false))))))))
    |}]

let%expect_test "Position and clock" =
  expect_test_p (module Line.Velocity)
"VG18  17326.494425  16653.192208  16121.344746 999999.999999                    ";
  [%expect {|
    (Ok
     ((space_vehicle_id ((kind Gps) (prn 18)))
      (data
       ((x 17326.494425) (y 16653.192208) (z 16121.344746) (clock ())
        (x_stddev ()) (y_stddev ()) (z_stddev ()) (clock_stddev ())))))
    |}]


let%expect_test "Epoch block" =
  expect_test_f (module Epoch_block)
{|*  2025  3 30  0  0  0.00000000
PG01  -1103.209931  15790.801024 -21325.471767    198.406633
PG02 -10450.357472  17370.707261 -16482.947527   -207.393848
PG03   7117.316147  20056.052947 -16137.962133    681.020785|};
  [%expect {|
    ((unconsumed ((buf "") (off 214) (len 0)))
     (result
      (((year 2025) (month 3) (day_of_month 30) (hour 0) (minute 0) (second 0))
       ((((space_vehicle_id ((kind Gps) (prn 1)))
          (data
           (((x -1103.209931) (y 15790.801024) (z -21325.471767)
             (clock (198.406633)) (x_stddev ()) (y_stddev ()) (z_stddev ())
             (clock_stddev ()) (clock_event false) (clock_pred false)
             (maneuver false) (orbit_pred false)))))
         ())
        (((space_vehicle_id ((kind Gps) (prn 2)))
          (data
           (((x -10450.357472) (y 17370.707261) (z -16482.947527)
             (clock (-207.393848)) (x_stddev ()) (y_stddev ()) (z_stddev ())
             (clock_stddev ()) (clock_event false) (clock_pred false)
             (maneuver false) (orbit_pred false)))))
         ())
        (((space_vehicle_id ((kind Gps) (prn 3)))
          (data
           (((x 7117.316147) (y 20056.052947) (z -16137.962133)
             (clock (681.020785)) (x_stddev ()) (y_stddev ()) (z_stddev ())
             (clock_stddev ()) (clock_event false) (clock_pred false)
             (maneuver false) (orbit_pred false)))))
         ())))))
    |}]

let%expect_test "Header" =
  expect_test_f (module Header)
    {|#cP2025  4 26  6  0  0.00000000     576   u+U IGb20 FIT  GFZ
## 2363 540000.00000000   300.00000000 60791 0.2500000000000
+   77   E02E03E04E05E06E07E08E09E10E13E14E15E16E18E19E21E23
+        E24E25E26E27E29E30E31E33E34E36G01G02G03G04G06G07G08
+        G09G10G11G12G13G14G15G16G17G18G19G20G22G23G24G25G26
+        G27G28G29G30G31G32R02R03R04R05R07R08R09R11R12R14R15
+        R16R17R18R19R20R21R22R24R26 00 00 00 00 00 00 00 00
++         6  6  6  6  6  6  6  6  6  8  6  6  6  8  6  6  6
++         6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6
++         6  6  6  6  6  6  6  6  6  6  6  6  6  6  6 10  8
++         6  6  6  6  6  6  6  8  8  8  8  8  6  8  8  8  8
++         8  8  6  8  8  6  8  8  8  0  0  0  0  0  0  0  0
%c M  cc GPS ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc
%c cc cc ccc ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc
%f  1.2500000  1.025000000  0.00000000000  0.000000000000000
%f  0.0000000  0.000000000  0.00000000000  0.000000000000000
%i    0    0    0    0      0      0      0      0         0
%i    0    0    0    0      0      0      0      0         0
/* PCV:IGS20_2360 OL/AL:FES2014b NONE     YN CLK:CoN ORB:CoN
/*     GFZ Helmholtz Centre for Geosciences
/*                                                   
/*                                                   
|};
  [%expect {|
    ((unconsumed ((buf "\n") (off 1310) (len 1)))
     (result
      ((version
        ((version C) (pos_or_vel Pos) (year 2025) (month 4) (day_of_month 26)
         (hour 6) (minute 0) (second 0) (number_of_epochs 576)
         (data_used "  u+U") (coordinate_system IGb20) (orbit_used FIT)
         (agency " GFZ")))
       (time_info
        ((gps_week 2363) (seconds_of_week 540000) (epoch_interval 300)
         (modified_julian_day 60791) (fractional_day 0.25)))
       (space_vehicles
        (((number (77))
          (ids
           (((kind Galileo) (prn 2)) ((kind Galileo) (prn 3))
            ((kind Galileo) (prn 4)) ((kind Galileo) (prn 5))
            ((kind Galileo) (prn 6)) ((kind Galileo) (prn 7))
            ((kind Galileo) (prn 8)) ((kind Galileo) (prn 9))
            ((kind Galileo) (prn 10)) ((kind Galileo) (prn 13))
            ((kind Galileo) (prn 14)) ((kind Galileo) (prn 15))
            ((kind Galileo) (prn 16)) ((kind Galileo) (prn 18))
            ((kind Galileo) (prn 19)) ((kind Galileo) (prn 21))
            ((kind Galileo) (prn 23)))))
         ((number ())
          (ids
           (((kind Galileo) (prn 24)) ((kind Galileo) (prn 25))
            ((kind Galileo) (prn 26)) ((kind Galileo) (prn 27))
            ((kind Galileo) (prn 29)) ((kind Galileo) (prn 30))
            ((kind Galileo) (prn 31)) ((kind Galileo) (prn 33))
            ((kind Galileo) (prn 34)) ((kind Galileo) (prn 36))
            ((kind Gps) (prn 1)) ((kind Gps) (prn 2)) ((kind Gps) (prn 3))
            ((kind Gps) (prn 4)) ((kind Gps) (prn 6)) ((kind Gps) (prn 7))
            ((kind Gps) (prn 8)))))
         ((number ())
          (ids
           (((kind Gps) (prn 9)) ((kind Gps) (prn 10)) ((kind Gps) (prn 11))
            ((kind Gps) (prn 12)) ((kind Gps) (prn 13)) ((kind Gps) (prn 14))
            ((kind Gps) (prn 15)) ((kind Gps) (prn 16)) ((kind Gps) (prn 17))
            ((kind Gps) (prn 18)) ((kind Gps) (prn 19)) ((kind Gps) (prn 20))
            ((kind Gps) (prn 22)) ((kind Gps) (prn 23)) ((kind Gps) (prn 24))
            ((kind Gps) (prn 25)) ((kind Gps) (prn 26)))))
         ((number ())
          (ids
           (((kind Gps) (prn 27)) ((kind Gps) (prn 28)) ((kind Gps) (prn 29))
            ((kind Gps) (prn 30)) ((kind Gps) (prn 31)) ((kind Gps) (prn 32))
            ((kind Glonass) (prn 2)) ((kind Glonass) (prn 3))
            ((kind Glonass) (prn 4)) ((kind Glonass) (prn 5))
            ((kind Glonass) (prn 7)) ((kind Glonass) (prn 8))
            ((kind Glonass) (prn 9)) ((kind Glonass) (prn 11))
            ((kind Glonass) (prn 12)) ((kind Glonass) (prn 14))
            ((kind Glonass) (prn 15)))))
         ((number ())
          (ids
           (((kind Glonass) (prn 16)) ((kind Glonass) (prn 17))
            ((kind Glonass) (prn 18)) ((kind Glonass) (prn 19))
            ((kind Glonass) (prn 20)) ((kind Glonass) (prn 21))
            ((kind Glonass) (prn 22)) ((kind Glonass) (prn 24))
            ((kind Glonass) (prn 26)))))))
       (accuracy
        (((accuracy_exponent
           ((6) (6) (6) (6) (6) (6) (6) (6) (6) (8) (6) (6) (6) (8) (6) (6) (6))))
         ((accuracy_exponent
           ((6) (6) (6) (6) (6) (6) (6) (6) (6) (6) (6) (6) (6) (6) (6) (6) (6))))
         ((accuracy_exponent
           ((6) (6) (6) (6) (6) (6) (6) (6) (6) (6) (6) (6) (6) (6) (6) (10) (8))))
         ((accuracy_exponent
           ((6) (6) (6) (6) (6) (6) (6) (8) (8) (8) (8) (8) (6) (8) (8) (8) (8))))
         ((accuracy_exponent
           ((8) (8) (6) (8) (8) (6) (8) (8) (8) () () () () () () () ())))))
       (type_and_time ((file_type Mixed) (time_system Gps)))
       (base ((position_velocity 1.25) (clock 1.025))) (comments (() () () ())))))
    |}]

let%expect_test "Full file" =
  expect_test_f (module Full_file)
{|#cP2025  3 30  0  0  0.00000000     577 d+D   IGb20 EXT AIUB
## 2360      0.00000000   300.00000000 60764 0.0000000000000
+   80   G01G02G03G04G05G07G08G09G10G11G12G13G14G15G16G17G18
+        G19G20G21G22G23G24G25G26G27G28G29G30G31G32R01R02R03
+        R04R05R07R08R09R12R14R15R16R17R18R19R20R21R22R24R26
+        E02E03E04E05E06E07E08E09E10E11E12E13E14E15E16E18E19
+        E21E23E24E25E26E27E29E30E31E33E34E36  0  0  0  0  0
++         6  8  8  5  7  7  6  5  6  7  7  7  6  7  7  7  8
++         7  7  6  7  7  6  5  7  6  6  7  6  7  6  7  7  7
++         7  7  7  7  7  8  8  9  8  8  7  9  9  8  6  8  7
++         7  6  6  5  5  6  6  6  6  6  7  8  7  8  7  6  7
++         7  6  7  7  7  6  7  7  7  7  7  7  0  0  0  0  0
%c M  cc GPS ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc
%c cc cc ccc ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc
%f  1.2500000  1.025000000  0.00000000000  0.000000000000000
%f  0.0000000  0.000000000  0.00000000000  0.000000000000000
%i    0    0    0    0      0      0      0      0         0
%i    0    0    0    0      0      0      0      0         0
/* Center for Orbit Determination in Europe (CODE)          
/* Ultra-rapid GRE orbits starting year-day 25089 00 hour   
/* Observed/predicted: 24/24 hours (data used up to 089X)   
/* PCV:IGS20      OL/AL:FES2014b NONE     YN ORB:CoN CLK:BRD
*  2025  3 30  0  0  0.00000000
PG01  -1103.209931  15790.801024 -21325.471767    198.406633
PG02 -10450.357472  17370.707261 -16482.947527   -207.393848
PG03   7117.316147  20056.052947 -16137.962133    681.020785
EOF
|};
  [%expect {|
    ((unconsumed ((buf "") (off 1561) (len 0)))
     (result
      ((header
        ((version
          ((version C) (pos_or_vel Pos) (year 2025) (month 3) (day_of_month 30)
           (hour 0) (minute 0) (second 0) (number_of_epochs 577)
           (data_used "d+D  ") (coordinate_system IGb20) (orbit_used EXT)
           (agency AIUB)))
         (time_info
          ((gps_week 2360) (seconds_of_week 0) (epoch_interval 300)
           (modified_julian_day 60764) (fractional_day 0)))
         (space_vehicles
          (((number (80))
            (ids
             (((kind Gps) (prn 1)) ((kind Gps) (prn 2)) ((kind Gps) (prn 3))
              ((kind Gps) (prn 4)) ((kind Gps) (prn 5)) ((kind Gps) (prn 7))
              ((kind Gps) (prn 8)) ((kind Gps) (prn 9)) ((kind Gps) (prn 10))
              ((kind Gps) (prn 11)) ((kind Gps) (prn 12)) ((kind Gps) (prn 13))
              ((kind Gps) (prn 14)) ((kind Gps) (prn 15)) ((kind Gps) (prn 16))
              ((kind Gps) (prn 17)) ((kind Gps) (prn 18)))))
           ((number ())
            (ids
             (((kind Gps) (prn 19)) ((kind Gps) (prn 20)) ((kind Gps) (prn 21))
              ((kind Gps) (prn 22)) ((kind Gps) (prn 23)) ((kind Gps) (prn 24))
              ((kind Gps) (prn 25)) ((kind Gps) (prn 26)) ((kind Gps) (prn 27))
              ((kind Gps) (prn 28)) ((kind Gps) (prn 29)) ((kind Gps) (prn 30))
              ((kind Gps) (prn 31)) ((kind Gps) (prn 32))
              ((kind Glonass) (prn 1)) ((kind Glonass) (prn 2))
              ((kind Glonass) (prn 3)))))
           ((number ())
            (ids
             (((kind Glonass) (prn 4)) ((kind Glonass) (prn 5))
              ((kind Glonass) (prn 7)) ((kind Glonass) (prn 8))
              ((kind Glonass) (prn 9)) ((kind Glonass) (prn 12))
              ((kind Glonass) (prn 14)) ((kind Glonass) (prn 15))
              ((kind Glonass) (prn 16)) ((kind Glonass) (prn 17))
              ((kind Glonass) (prn 18)) ((kind Glonass) (prn 19))
              ((kind Glonass) (prn 20)) ((kind Glonass) (prn 21))
              ((kind Glonass) (prn 22)) ((kind Glonass) (prn 24))
              ((kind Glonass) (prn 26)))))
           ((number ())
            (ids
             (((kind Galileo) (prn 2)) ((kind Galileo) (prn 3))
              ((kind Galileo) (prn 4)) ((kind Galileo) (prn 5))
              ((kind Galileo) (prn 6)) ((kind Galileo) (prn 7))
              ((kind Galileo) (prn 8)) ((kind Galileo) (prn 9))
              ((kind Galileo) (prn 10)) ((kind Galileo) (prn 11))
              ((kind Galileo) (prn 12)) ((kind Galileo) (prn 13))
              ((kind Galileo) (prn 14)) ((kind Galileo) (prn 15))
              ((kind Galileo) (prn 16)) ((kind Galileo) (prn 18))
              ((kind Galileo) (prn 19)))))
           ((number ())
            (ids
             (((kind Galileo) (prn 21)) ((kind Galileo) (prn 23))
              ((kind Galileo) (prn 24)) ((kind Galileo) (prn 25))
              ((kind Galileo) (prn 26)) ((kind Galileo) (prn 27))
              ((kind Galileo) (prn 29)) ((kind Galileo) (prn 30))
              ((kind Galileo) (prn 31)) ((kind Galileo) (prn 33))
              ((kind Galileo) (prn 34)) ((kind Galileo) (prn 36)))))))
         (accuracy
          (((accuracy_exponent
             ((6) (8) (8) (5) (7) (7) (6) (5) (6) (7) (7) (7) (6) (7) (7)
              (7) (8))))
           ((accuracy_exponent
             ((7) (7) (6) (7) (7) (6) (5) (7) (6) (6) (7) (6) (7) (6) (7)
              (7) (7))))
           ((accuracy_exponent
             ((7) (7) (7) (7) (7) (8) (8) (9) (8) (8) (7) (9) (9) (8) (6)
              (8) (7))))
           ((accuracy_exponent
             ((7) (6) (6) (5) (5) (6) (6) (6) (6) (6) (7) (8) (7) (8) (7)
              (6) (7))))
           ((accuracy_exponent
             ((7) (6) (7) (7) (7) (6) (7) (7) (7) (7) (7) (7) () () () () ())))))
         (type_and_time ((file_type Mixed) (time_system Gps)))
         (base ((position_velocity 1.25) (clock 1.025)))
         (comments (() () () ()))))
       (epoch_blocks
        ((((year 2025) (month 3) (day_of_month 30) (hour 0) (minute 0)
           (second 0))
          ((((space_vehicle_id ((kind Gps) (prn 1)))
             (data
              (((x -1103.209931) (y 15790.801024) (z -21325.471767)
                (clock (198.406633)) (x_stddev ()) (y_stddev ()) (z_stddev ())
                (clock_stddev ()) (clock_event false) (clock_pred false)
                (maneuver false) (orbit_pred false)))))
            ())
           (((space_vehicle_id ((kind Gps) (prn 2)))
             (data
              (((x -10450.357472) (y 17370.707261) (z -16482.947527)
                (clock (-207.393848)) (x_stddev ()) (y_stddev ()) (z_stddev ())
                (clock_stddev ()) (clock_event false) (clock_pred false)
                (maneuver false) (orbit_pred false)))))
            ())
           (((space_vehicle_id ((kind Gps) (prn 3)))
             (data
              (((x 7117.316147) (y 20056.052947) (z -16137.962133)
                (clock (681.020785)) (x_stddev ()) (y_stddev ()) (z_stddev ())
                (clock_stddev ()) (clock_event false) (clock_pred false)
                (maneuver false) (orbit_pred false)))))
            ()))))))))
    |}]

let%expect_test "Processed file" =
  expect_test_f (module Processed_file)
    {|#dP2025  4 15 18  0  0.00000000     576 ORBIT IGb20 FIT JGX 
## 2362 237600.00000000   300.00000000 60780 0.7500000000000
+   76   G02G03G04G05G06G07G08G09G10G11G12G13G14G15G16G17G18
+        G19G20G21G22G23G24G25G26G27G28G29G30G31G32R01R02R03
+        R04R05R07R08R09R11R12R14R15R16R17R18R19R20R21R22R24
+        E02E03E04E05E06E07E08E09E10E11E12E13E15E19E21E24E25
+        E26E27E29E30E31E33E34E36  0  0  0  0  0  0  0  0  0
++         3  2  2  2  2  2  3  2  2  2  2  2  2  2  2  2  2
++         2  2 17  2  2  2  2  2  3  2  2  2  2  2 17  4  4
++         4  4  4  4  3  3  3  3  3  3  4  4  4  4  4  4  3
++         3  3  3  2  2  3  3  2  3 17  3  2  3  3  2  2  3
++         2  2  2  3  2  2  3  2  0  0  0  0  0  0  0  0  0
%c G  cc GPS ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc
%c cc cc ccc ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc
%f  1.2500000  1.025000000  0.00000000000  0.000000000000000
%f  0.0000000  0.000000000  0.00000000000  0.000000000000000
%i    0    0    0    0      0      0      0      0         0
%i    0    0    0    0      0      0      0      0         0
/* PROGRAM: MADOCA v.2.2.0, DATE: 2025/04/16 19:26:00 UTC                       
/* GSI and JAXA MGN URAPID                                                      
/* PCV:IGS20_2361 OL/AL:FES2014b NONE     NN CLK:CoM ORB:CoM                    
/*                                                                              
*  2025  4 15 18  0  0.00000000
PG02  20393.872619  16448.890331   6021.533601   -193.678381 11 15 16 408       
PG03  14720.343221   2094.001989  21822.302309    685.704153 13 18  9 408       
PG04  26146.413195   1169.320299   5004.661219    554.340033  8 13 15 408       
PG05  -9509.177294 -14235.560092 -20466.871746   -207.338896 15 17 13 408       
PG06  -2117.760416 -20992.709061  16113.165882   -306.510760 17 13 16 408       
PG07  15023.550437  -4830.815800 -20858.416348    -21.246216 15 17  9 408       
PG08  21940.757514   6550.547256 -13990.864107    509.035643 14 14 14 408       
PG09  24981.662143  -7496.903396  -5282.514459    631.820784 11 13 15 408       
PG10 -10172.959964  24338.239790    390.848002   -391.344804 15 11 18 408       
PG11 -10630.792008 -23493.199931   6530.703820   -727.888784 16  8 17 408       
PG12 -14983.036033  -3461.209039  21365.893549   -580.148972 14 18  9 408       
PG13 -13246.433399 -13303.283617 -19124.608743    700.432933 16 12 13 408       
PG14  12522.610311 -23086.256467  -2811.569394    631.050167 15 13 17 408       
PG15 -22640.793105  -3994.492679 -13937.478698    285.712089 12 14 14 408       
PG16   3654.058733  19589.167494 -17700.767606     -6.413653 16 16 15 408       
PG17  16155.109660 -12970.267276  17139.319904    327.019607 15 11 14 408       
PG18 -13413.264000   6821.976489 -21869.445135   -636.574693 16 17  6 408       
PG19   5545.461136 -14816.820052  21130.718888    619.077261 17  6 10 408       
PG20  -2752.462212 -23940.538946 -11204.470057    365.023539 16 14 17 408       
PG21      0.000000      0.000000      0.000000 999999.999999                    
PG22   9067.873490 -24682.843058   5149.231095   -102.289245 16  5 18 408       
PG23 -15995.330659  16159.816398 -13480.427490    460.063707 10 16 16 408       
PG24 -21326.678975 -12954.358805   9170.817180   -421.319480  7 16 15 408       
PG25 -17191.541238   9663.264946  17306.264844    490.343584  3 18 15 408       
PG26   -870.794118  24820.294326  -9048.460334   -187.683271 17 13 18 408       
PG27  10755.189023  13295.569811 -20752.706461    -27.715189 16 12 11 408       
PG28    -62.864197  19729.528117  17770.942862   -597.388230 16 13 14 408       
PG29 -25828.776332   4513.570305  -4714.436747   -517.484881 10 14 16 408       
PG30   6294.965475 -16031.227966 -19982.023016   -169.048900 16 15 11 408       
PG31   7266.577422  22935.850500  10599.871543   -210.148616 16  6 16 408       
PG32 -10617.852302  14983.347645  19359.698564   -476.668279 17  8 13 408       
PR01      0.000000      0.000000      0.000000 999999.999999                    
PR02 -10294.613059  -6476.174557 -22360.553959    -34.364402 18 17 13 408       
PR03 -22199.179027   5308.759454 -11277.956168    120.585040 11 14 17 408       
PR04 -21213.646675  13118.015863   5356.235273    286.589192 15 12 19 408       
PR05  -9009.358745  14180.403094  19203.790093    148.363106 20 14 18 408       
PR07  22297.858604  -5326.308862  11280.170874    104.285036 12 14 17 408       
PR08  21007.765827 -13399.554061  -5466.597493     -5.409556 16 12 19 408       
PR09   7307.721543  18942.873120 -15477.750218    249.412002 16 20 19 408       
PR11   9474.997417 -14756.218117 -18505.577165     22.203729 14 20 18 408       
PR12   1874.089801 -25415.874662  -1188.397548     53.120805 19  8 22 408       
PR14 -11055.582081  -1934.332660  22917.228089     28.094334 10 22  8 408       
PR15  -9176.832584  16364.577549  17305.522091     69.304246 15 20 19 408       
PR16  -1885.673082  25424.703327   1335.073119     35.030673 17  6 20 408       
PR17 -21368.435811  -7772.660813 -11596.379672    183.223950 13 18 19 408       
PR18 -12340.892644   3691.353916 -21996.308706    196.897759 17 16 13 408       
PR19    540.342304  12717.839185 -22109.643837    -47.816033 20 17 15 408       
PR20  17832.448040  16939.423639  -6843.586048   -128.695001 19 15 22 408       
PR21  21580.475358   9065.594194  10104.321028   -193.647776 12 18 19 408       
PR22  11604.230639  -4744.845787  22239.104910    -66.165846 17 16 12 408       
PR24 -18827.905640 -16007.656410   6358.471841   -100.593697 19 16 22 408       
PE02 -12341.612850  26687.645424  -3357.196156    212.396818 15  7 16 408       
PE03 -11052.522389  15824.575600  22447.285084   -185.692088 17 10 12 408       
PE04 -12069.258035 -15254.301896 -22301.711148   -208.320952 17 12 13 408       
PE05 -27172.888622   7077.538260   9390.873342   4844.155511 14 15 16 408       
PE06 -21486.424996 -11470.032504 -16827.805922   -106.259869 16 14 15 408       
PE07  27384.231525   6025.589783   9467.973755   -179.532587 12 15 16 408       
PE08  11366.903922  15318.725186  22633.611575   -449.622411 17 11 13 408       
PE09 -27384.969856  -6013.142020  -9518.042355   -851.849074 12 15 16 408       
PE10 -18280.999911 -22319.124759  -6621.216660   -703.224386 12 15 17 408       
PE11      0.000000      0.000000      0.000000 999999.999999                    
PE12 -13687.377274 -26117.149535   2423.289091  -1228.128347 14 11 17 408       
PE13  20403.533833  14815.801315  15517.990151    -33.052630  5 16 15 408       
PE15  13279.028372  26277.210655  -3137.746637   -168.144279 15 11 17 408       
PE19  12725.489417 -15242.301756 -21936.255734   1645.726610 17 10 13 408       
PE21  20366.125295 -15880.100468 -14469.123920   -658.615101  9 17 17 408       
PE24 -16626.448403  -4609.543722  24030.483532  -1380.665933 13 17  9 408       
PE25 -20422.369204  15454.583717  14839.028931     16.240368  9 17 17 408       
PE26  15401.597140  -5558.374352  24661.834170    -68.186923 15 17  6 408       
PE27  16556.681496   4648.575879 -24096.375982   -635.238987 13 17  9 408       
PE29  27034.919354  -7271.528097  -9615.536471    -87.688832 14 14 16 408       
PE30   3247.815851  22018.694911 -19523.889747  -1453.056061 16 15 14 408       
PE31  -3030.201651 -22167.390509  19382.689581    -63.009852 16 15 14 408       
PE33   1834.006555 -21873.294561  19857.486519     15.554073 16 14 15 408       
PE34  -1509.395223  22095.677173 -19643.837565   -175.293569 16 14 15 408       
PE36 -15475.153688   5043.772961 -24722.199332   -380.959273 15 17  7 408       
EOF
|};
  [%expect {|
    ((unconsumed ((buf "") (off 7614) (len 0)))
     (result
      ((raw_header
        ((version
          ((version D) (pos_or_vel Pos) (year 2025) (month 4) (day_of_month 15)
           (hour 18) (minute 0) (second 0) (number_of_epochs 576)
           (data_used ORBIT) (coordinate_system IGb20) (orbit_used FIT)
           (agency "JGX ")))
         (time_info
          ((gps_week 2362) (seconds_of_week 237600) (epoch_interval 300)
           (modified_julian_day 60780) (fractional_day 0.75)))
         (space_vehicles
          (((number (76))
            (ids
             (((kind Gps) (prn 2)) ((kind Gps) (prn 3)) ((kind Gps) (prn 4))
              ((kind Gps) (prn 5)) ((kind Gps) (prn 6)) ((kind Gps) (prn 7))
              ((kind Gps) (prn 8)) ((kind Gps) (prn 9)) ((kind Gps) (prn 10))
              ((kind Gps) (prn 11)) ((kind Gps) (prn 12)) ((kind Gps) (prn 13))
              ((kind Gps) (prn 14)) ((kind Gps) (prn 15)) ((kind Gps) (prn 16))
              ((kind Gps) (prn 17)) ((kind Gps) (prn 18)))))
           ((number ())
            (ids
             (((kind Gps) (prn 19)) ((kind Gps) (prn 20)) ((kind Gps) (prn 21))
              ((kind Gps) (prn 22)) ((kind Gps) (prn 23)) ((kind Gps) (prn 24))
              ((kind Gps) (prn 25)) ((kind Gps) (prn 26)) ((kind Gps) (prn 27))
              ((kind Gps) (prn 28)) ((kind Gps) (prn 29)) ((kind Gps) (prn 30))
              ((kind Gps) (prn 31)) ((kind Gps) (prn 32))
              ((kind Glonass) (prn 1)) ((kind Glonass) (prn 2))
              ((kind Glonass) (prn 3)))))
           ((number ())
            (ids
             (((kind Glonass) (prn 4)) ((kind Glonass) (prn 5))
              ((kind Glonass) (prn 7)) ((kind Glonass) (prn 8))
              ((kind Glonass) (prn 9)) ((kind Glonass) (prn 11))
              ((kind Glonass) (prn 12)) ((kind Glonass) (prn 14))
              ((kind Glonass) (prn 15)) ((kind Glonass) (prn 16))
              ((kind Glonass) (prn 17)) ((kind Glonass) (prn 18))
              ((kind Glonass) (prn 19)) ((kind Glonass) (prn 20))
              ((kind Glonass) (prn 21)) ((kind Glonass) (prn 22))
              ((kind Glonass) (prn 24)))))
           ((number ())
            (ids
             (((kind Galileo) (prn 2)) ((kind Galileo) (prn 3))
              ((kind Galileo) (prn 4)) ((kind Galileo) (prn 5))
              ((kind Galileo) (prn 6)) ((kind Galileo) (prn 7))
              ((kind Galileo) (prn 8)) ((kind Galileo) (prn 9))
              ((kind Galileo) (prn 10)) ((kind Galileo) (prn 11))
              ((kind Galileo) (prn 12)) ((kind Galileo) (prn 13))
              ((kind Galileo) (prn 15)) ((kind Galileo) (prn 19))
              ((kind Galileo) (prn 21)) ((kind Galileo) (prn 24))
              ((kind Galileo) (prn 25)))))
           ((number ())
            (ids
             (((kind Galileo) (prn 26)) ((kind Galileo) (prn 27))
              ((kind Galileo) (prn 29)) ((kind Galileo) (prn 30))
              ((kind Galileo) (prn 31)) ((kind Galileo) (prn 33))
              ((kind Galileo) (prn 34)) ((kind Galileo) (prn 36)))))))
         (accuracy
          (((accuracy_exponent
             ((3) (2) (2) (2) (2) (2) (3) (2) (2) (2) (2) (2) (2) (2) (2)
              (2) (2))))
           ((accuracy_exponent
             ((2) (2) (17) (2) (2) (2) (2) (2) (3) (2) (2) (2) (2) (2) (17)
              (4) (4))))
           ((accuracy_exponent
             ((4) (4) (4) (4) (3) (3) (3) (3) (3) (3) (4) (4) (4) (4) (4)
              (4) (3))))
           ((accuracy_exponent
             ((3) (3) (3) (2) (2) (3) (3) (2) (3) (17) (3) (2) (3) (3) (2)
              (2) (3))))
           ((accuracy_exponent
             ((2) (2) (2) (3) (2) (2) (3) (2) () () () () () () () () ())))))
         (type_and_time ((file_type (Only Gps)) (time_system Gps)))
         (base ((position_velocity 1.25) (clock 1.025)))
         (comments (() () () ()))))
       (space_vehicles
        (((kind Gps) (prn 2)) ((kind Gps) (prn 3)) ((kind Gps) (prn 4))
         ((kind Gps) (prn 5)) ((kind Gps) (prn 6)) ((kind Gps) (prn 7))
         ((kind Gps) (prn 8)) ((kind Gps) (prn 9)) ((kind Gps) (prn 10))
         ((kind Gps) (prn 11)) ((kind Gps) (prn 12)) ((kind Gps) (prn 13))
         ((kind Gps) (prn 14)) ((kind Gps) (prn 15)) ((kind Gps) (prn 16))
         ((kind Gps) (prn 17)) ((kind Gps) (prn 18)) ((kind Gps) (prn 19))
         ((kind Gps) (prn 20)) ((kind Gps) (prn 21)) ((kind Gps) (prn 22))
         ((kind Gps) (prn 23)) ((kind Gps) (prn 24)) ((kind Gps) (prn 25))
         ((kind Gps) (prn 26)) ((kind Gps) (prn 27)) ((kind Gps) (prn 28))
         ((kind Gps) (prn 29)) ((kind Gps) (prn 30)) ((kind Gps) (prn 31))
         ((kind Gps) (prn 32)) ((kind Glonass) (prn 1)) ((kind Glonass) (prn 2))
         ((kind Glonass) (prn 3)) ((kind Glonass) (prn 4))
         ((kind Glonass) (prn 5)) ((kind Glonass) (prn 7))
         ((kind Glonass) (prn 8)) ((kind Glonass) (prn 9))
         ((kind Glonass) (prn 11)) ((kind Glonass) (prn 12))
         ((kind Glonass) (prn 14)) ((kind Glonass) (prn 15))
         ((kind Glonass) (prn 16)) ((kind Glonass) (prn 17))
         ((kind Glonass) (prn 18)) ((kind Glonass) (prn 19))
         ((kind Glonass) (prn 20)) ((kind Glonass) (prn 21))
         ((kind Glonass) (prn 22)) ((kind Glonass) (prn 24))
         ((kind Galileo) (prn 2)) ((kind Galileo) (prn 3))
         ((kind Galileo) (prn 4)) ((kind Galileo) (prn 5))
         ((kind Galileo) (prn 6)) ((kind Galileo) (prn 7))
         ((kind Galileo) (prn 8)) ((kind Galileo) (prn 9))
         ((kind Galileo) (prn 10)) ((kind Galileo) (prn 11))
         ((kind Galileo) (prn 12)) ((kind Galileo) (prn 13))
         ((kind Galileo) (prn 15)) ((kind Galileo) (prn 19))
         ((kind Galileo) (prn 21)) ((kind Galileo) (prn 24))
         ((kind Galileo) (prn 25)) ((kind Galileo) (prn 26))
         ((kind Galileo) (prn 27)) ((kind Galileo) (prn 29))
         ((kind Galileo) (prn 30)) ((kind Galileo) (prn 31))
         ((kind Galileo) (prn 33)) ((kind Galileo) (prn 34))
         ((kind Galileo) (prn 36))))
       (accuracy
        (0.008 0.004 0.004 0.004 0.004 0.004 0.008 0.004 0.004 0.004 0.004 0.004
         0.004 0.004 0.004 0.004 0.004 0.004 0.004 131.072 0.004 0.004 0.004
         0.004 0.004 0.008 0.004 0.004 0.004 0.004 0.004 131.072 0.016 0.016
         0.016 0.016 0.016 0.016 0.008 0.008 0.008 0.008 0.008 0.008 0.016 0.016
         0.016 0.016 0.016 0.016 0.008 0.008 0.008 0.008 0.004 0.004 0.008 0.008
         0.004 0.008 131.072 0.008 0.004 0.008 0.008 0.004 0.004 0.008 0.004
         0.004 0.004 0.008 0.004 0.004 0.008 0.004))
       (epochs
        (((metadata
           ((year 2025) (month 4) (day_of_month 15) (hour 18) (minute 0)
            (second 0)))
          (x
           (20.393872619 14.720343221 26.146413195 -9.509177294 -2.117760416
            15.023550437 21.940757514 24.981662143 -10.172959964 -10.630792008
            -14.983036033 -13.246433399 12.522610311 -22.640793105 3.654058733
            16.15510966 -13.413264 5.545461136 -2.752462212 None 9.06787349
            -15.995330659 -21.326678975 -17.191541238 -0.870794118 10.755189023
            -0.062864197 -25.828776332 6.294965475 7.266577422 -10.617852302 None
            -10.294613059 -22.199179027 -21.213646675 -9.009358745 22.297858604
            21.007765827 7.307721543 9.474997417 1.874089801 -11.055582081
            -9.176832584 -1.885673082 -21.368435811 -12.340892644 0.540342304
            17.83244804 21.580475358 11.604230639 -18.82790564 -12.34161285
            -11.052522389 -12.069258035 -27.172888622 -21.486424996 27.384231525
            11.366903922 -27.384969856 -18.280999911 None -13.687377274
            20.403533833 13.279028372 12.725489417 20.366125295 -16.626448403
            -20.422369204 15.40159714 16.556681496 27.034919354 3.247815851
            -3.030201651 1.834006555 -1.509395223 -15.475153688))
          (y
           (16.448890331 2.094001989 1.169320299 -14.235560092 -20.992709061
            -4.8308158 6.550547256 -7.496903396 24.33823979 -23.493199931
            -3.461209039 -13.303283617 -23.086256467 -3.994492679 19.589167494
            -12.970267276 6.821976489 -14.816820052 -23.940538946 None
            -24.682843058 16.159816398 -12.954358805 9.663264946 24.820294326
            13.295569811 19.729528117 4.513570305 -16.031227966 22.9358505
            14.983347645 None -6.476174557 5.308759454 13.118015863 14.180403094
            -5.326308862 -13.399554061 18.94287312 -14.756218117 -25.415874662
            -1.93433266 16.364577549 25.424703327 -7.772660813 3.691353916
            12.717839185 16.939423639 9.065594194 -4.744845787 -16.00765641
            26.687645424 15.8245756 -15.254301896 7.07753826 -11.470032504
            6.025589783 15.318725186 -6.01314202 -22.319124759 None -26.117149535
            14.815801315 26.277210655 -15.242301756 -15.880100468 -4.609543722
            15.454583717 -5.558374352 4.648575879 -7.271528097 22.018694911
            -22.167390509 -21.873294561 22.095677173 5.043772961))
          (z
           (6.021533601 21.822302309 5.004661219 -20.466871746 16.113165882
            -20.858416348 -13.990864107 -5.282514459 0.390848002 6.53070382
            21.365893549 -19.124608743 -2.811569394 -13.937478698 -17.700767606
            17.139319904 -21.869445135 21.130718888 -11.204470057 None
            5.149231095 -13.48042749 9.17081718 17.306264844 -9.048460334
            -20.752706461 17.770942862 -4.714436747 -19.982023016 10.599871543
            19.359698564 None -22.360553959 -11.277956168 5.356235273
            19.203790093 11.280170874 -5.466597493 -15.477750218 -18.505577165
            -1.188397548 22.917228089 17.305522091 1.335073119 -11.596379672
            -21.996308706 -22.109643837 -6.843586048 10.104321028 22.23910491
            6.358471841 -3.357196156 22.447285084 -22.301711148 9.390873342
            -16.827805922 9.467973755 22.633611575 -9.518042355 -6.62121666 None
            2.423289091 15.517990151 -3.137746637 -21.936255734 -14.46912392
            24.030483532 14.839028931 24.66183417 -24.096375982 -9.615536471
            -19.523889747 19.382689581 19.857486519 -19.643837565 -24.722199332))
          (clock
           (-0.000193678381 0.000685704153 0.000554340033 -0.000207338896
            -0.00030651076 -2.1246216E-05 0.000509035643 0.000631820784
            -0.000391344804 -0.000727888784 -0.000580148972 0.000700432933
            0.000631050167 0.000285712089 -6.413653E-06 0.000327019607
            -0.000636574693 0.000619077261 0.000365023539 None -0.000102289245
            0.000460063707 -0.00042131948 0.000490343584 -0.000187683271
            -2.7715189E-05 -0.00059738823 -0.000517484881 -0.0001690489
            -0.000210148616 -0.000476668279 None -3.4364402E-05 0.00012058504
            0.000286589192 0.000148363106 0.000104285036 -5.409556E-06
            0.000249412002 2.2203729E-05 5.3120805E-05 2.8094334E-05
            6.9304246E-05 3.5030673E-05 0.00018322395 0.000196897759
            -4.7816033E-05 -0.000128695001 -0.000193647776 -6.6165846E-05
            -0.000100593697 0.000212396818 -0.000185692088 -0.000208320952
            0.004844155511 -0.000106259869 -0.000179532587 -0.000449622411
            -0.000851849074 -0.000703224386 None -0.001228128347 -3.305263E-05
            -0.000168144279 0.00164572661 -0.000658615101 -0.001380665933
            1.6240368E-05 -6.8186923E-05 -0.000635238987 -8.7688832E-05
            -0.001453056061 -6.3009852E-05 1.5554073E-05 -0.000175293569
            -0.000380959273))
          (x_stddev
           (0.011641532182693481 0.018189894035458565 0.0059604644775390625
            0.028421709430404007 0.044408920985006262 0.028421709430404007
            0.022737367544323206 0.011641532182693481 0.028421709430404007
            0.035527136788005009 0.022737367544323206 0.035527136788005009
            0.028421709430404007 0.014551915228366852 0.035527136788005009
            0.028421709430404007 0.035527136788005009 0.044408920985006262
            0.035527136788005009 None 0.035527136788005009 0.0093132257461547852
            0.00476837158203125 0.001953125 0.044408920985006262
            0.035527136788005009 0.035527136788005009 0.0093132257461547852
            0.035527136788005009 0.035527136788005009 0.044408920985006262 None
            0.055511151231257827 0.011641532182693481 0.028421709430404007
            0.086736173798840355 0.014551915228366852 0.035527136788005009
            0.035527136788005009 0.022737367544323206 0.069388939039072284
            0.0093132257461547852 0.028421709430404007 0.044408920985006262
            0.018189894035458565 0.044408920985006262 0.086736173798840355
            0.069388939039072284 0.014551915228366852 0.044408920985006262
            0.069388939039072284 0.028421709430404007 0.044408920985006262
            0.044408920985006262 0.022737367544323206 0.035527136788005009
            0.014551915228366852 0.044408920985006262 0.014551915228366852
            0.014551915228366852 None 0.022737367544323206 0.0030517578125
            0.028421709430404007 0.044408920985006262 0.0074505805969238281
            0.018189894035458565 0.0074505805969238281 0.028421709430404007
            0.018189894035458565 0.022737367544323206 0.035527136788005009
            0.035527136788005009 0.035527136788005009 0.035527136788005009
            0.028421709430404007))
          (y_stddev
           (0.028421709430404007 0.055511151231257827 0.018189894035458565
            0.044408920985006262 0.018189894035458565 0.044408920985006262
            0.022737367544323206 0.018189894035458565 0.011641532182693481
            0.0059604644775390625 0.055511151231257827 0.014551915228366852
            0.018189894035458565 0.022737367544323206 0.035527136788005009
            0.011641532182693481 0.044408920985006262 0.003814697265625
            0.022737367544323206 None 0.0030517578125 0.035527136788005009
            0.035527136788005009 0.055511151231257827 0.018189894035458565
            0.014551915228366852 0.018189894035458565 0.022737367544323206
            0.028421709430404007 0.003814697265625 0.0059604644775390625 None
            0.044408920985006262 0.022737367544323206 0.014551915228366852
            0.022737367544323206 0.022737367544323206 0.014551915228366852
            0.086736173798840355 0.086736173798840355 0.0059604644775390625
            0.13552527156068805 0.086736173798840355 0.003814697265625
            0.055511151231257827 0.035527136788005009 0.044408920985006262
            0.028421709430404007 0.055511151231257827 0.035527136788005009
            0.035527136788005009 0.00476837158203125 0.0093132257461547852
            0.014551915228366852 0.028421709430404007 0.022737367544323206
            0.028421709430404007 0.011641532182693481 0.028421709430404007
            0.028421709430404007 None 0.011641532182693481 0.035527136788005009
            0.011641532182693481 0.0093132257461547852 0.044408920985006262
            0.044408920985006262 0.044408920985006262 0.044408920985006262
            0.044408920985006262 0.022737367544323206 0.028421709430404007
            0.028421709430404007 0.022737367544323206 0.022737367544323206
            0.044408920985006262))
          (z_stddev
           (0.035527136788005009 0.0074505805969238281 0.028421709430404007
            0.018189894035458565 0.035527136788005009 0.0074505805969238281
            0.022737367544323206 0.028421709430404007 0.055511151231257827
            0.044408920985006262 0.0074505805969238281 0.018189894035458565
            0.044408920985006262 0.022737367544323206 0.028421709430404007
            0.022737367544323206 0.003814697265625 0.0093132257461547852
            0.044408920985006262 None 0.055511151231257827 0.035527136788005009
            0.028421709430404007 0.028421709430404007 0.055511151231257827
            0.011641532182693481 0.022737367544323206 0.035527136788005009
            0.011641532182693481 0.035527136788005009 0.018189894035458565 None
            0.018189894035458565 0.044408920985006262 0.069388939039072284
            0.055511151231257827 0.044408920985006262 0.069388939039072284
            0.069388939039072284 0.055511151231257827 0.13552527156068805
            0.0059604644775390625 0.069388939039072284 0.086736173798840355
            0.069388939039072284 0.018189894035458565 0.028421709430404007
            0.13552527156068805 0.069388939039072284 0.014551915228366852
            0.13552527156068805 0.035527136788005009 0.014551915228366852
            0.018189894035458565 0.035527136788005009 0.028421709430404007
            0.035527136788005009 0.018189894035458565 0.035527136788005009
            0.044408920985006262 None 0.044408920985006262 0.028421709430404007
            0.044408920985006262 0.018189894035458565 0.044408920985006262
            0.0074505805969238281 0.044408920985006262 0.003814697265625
            0.0074505805969238281 0.035527136788005009 0.022737367544323206
            0.022737367544323206 0.028421709430404007 0.028421709430404007
            0.00476837158203125))
          (clock_stddev
           (2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 None 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 None 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            None 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08 2.3732149737361174E-08 2.3732149737361174E-08
            2.3732149737361174E-08))
          (maneuver (Empty 76)) (orbit_pred (Empty 76)) (clock_event (Empty 76))
          (clock_pred (Empty 76)) (velocity ()))))
       (presence
        ((pos
          1111111111111111111011111111111011111111111111111111111111110111111111111111)
         (clock
          1111111111111111111011111111111011111111111111111111111111110111111111111111)
         (pos_stddev (Full 76)) (clock_stddev (Full 76)) (maneuver (Empty 76))
         (clock_event (Empty 76)) (velocity (Empty 76))
         (clock_velocity (Empty 76)) (velocity_stddev (Full 76))
         (clock_velocity_stddev (Full 76)))))))
    |}]

let%expect_test "Processed file" =
  expect_test_f (module Processed_file)
    {|#cP2025  4 26 18  0  0.00000000     577 d+D   IGb20 EXT AIUB
## 2363 583200.00000000   300.00000000 60791 0.7500000000000
+   80   G01G02G03G04G05G06G07G08G09G10G11G12G13G14G15G16G17
+        G18G19G20G22G23G24G26G27G28G29G30G31G32R02R03R04R05
+        R07R08R09R11R12R14R15R16R17R18R19R20R21R22R24R26R27
+        E02E03E04E05E06E07E08E09E10E11E12E13E14E15E16E18E19
+        E21E23E24E25E26E27E29E30E31E33E34E36  0  0  0  0  0
++         7  7  7  5  7  6  7  5  6  7  6  7  6  5  7  7  7
++         6  7  7  7  7  6  6  5  6  7  6  7  6  7  7  7  7
++         7  7  8  8  8  7  8  8  7  6  8  8  7  6  7  8  8
++         6  7  6  6  6  6  6  6  7  6  6  7  7  7  6  7  6
++         6  6  6  7  7  6  7  6  6  7  7  7  0  0  0  0  0
%c M  cc GPS ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc
%c cc cc ccc ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc
%f  1.2500000  1.025000000  0.00000000000  0.000000000000000
%f  0.0000000  0.000000000  0.00000000000  0.000000000000000
%i    0    0    0    0      0      0      0      0         0
%i    0    0    0    0      0      0      0      0         0
/* Center for Orbit Determination in Europe (CODE)          
/* Ultra-rapid GRE orbits starting year-day 25116 18 hour   
/* Observed/predicted: 16/32 hours (data used up to 117J)   
/* PCV:IGS20      OL/AL:FES2014b NONE     YN ORB:CoN CLK:BRD
*  2025  4 27 10  0  0.00000000
PG01    235.804082 -15613.737056 -21484.878594    238.648743
PG02  10487.194542 -17225.551881 -16612.076000   -184.188178
PG03  -6919.625534 -20185.884046 -16066.056265    688.282918
PG04   -205.738626 -25093.575230   8487.492367    559.253891
PG05  -3952.027413  15173.450524  21248.946963   -208.353540
PG06 -25731.217268   5526.056908  -4004.995003   -327.415601
PG07 -15948.018014  -9426.392166  19753.242843    -23.650206
PG08   4779.274875 -25837.572821   1192.895915    513.189363
PG09  -8653.755353 -18712.502592  16622.992614    642.973926
PG10  21895.387510   8077.276871 -12747.295606   -404.344814
PG11 -22716.713952  11770.922217   7050.546985   -722.514869
PG12   5208.189505  20868.348667 -15890.328345   -581.921715
PG13 -13492.372234  20957.992491   8560.040101    700.734796
PG14 -22396.870168 -10284.917500 -10179.988755    636.894558
PG15  -1097.531677  26448.730968    462.318603    289.920736
PG16  11518.385978  -9741.271962  21508.121734      2.416946
PG17 -14124.810864  -5081.053914 -21507.664181    308.522372
PG18  17385.153722   7718.711652  18666.120720   -632.313753
PG19 -16315.433975   4947.848663 -20370.859210    622.894619
PG20 -15709.532012   7220.134075  20221.699721    364.802538
PG22 -20208.498108  -4842.174939 -15987.900304   -102.760398
PG23  20286.179223  17333.245904    679.639946    466.894689
PG24  -4216.954589  17882.788452 -19527.068407   -414.286395
PG26  19081.920230  -2972.564250  18195.373399   -208.191923
PG27  12331.704077 -19625.483807  12240.866465    -26.989939
PG28  25245.850917  -4254.032620  -7093.743062   -603.372180
PG29   6250.967002  19905.835604  16304.953023   -513.277912
PG30 -23907.319478   -990.749905  11983.815263   -159.527335
PG31  24952.200005  -9399.490809   1912.944348   -208.998761
PG32  15215.696426   -252.295589 -21508.018788   -466.947284
PR02   1375.428278 -23378.199733  -9986.566720    -33.688093
PR03  -7339.839808 -23022.565341   8139.989019    121.297668
PR04 -11011.944404 -10148.625712  20681.163022    288.427719
PR05  -9232.506457   8191.085989  22341.586047    148.953238
PR07   7287.819889  23078.924858  -8081.578219    104.925869
PR08  11200.302830   9795.828356 -20661.314311     -5.233345
PR09  -2744.975142  11016.414286 -22818.848203    251.642532
PR11  23549.612629   9666.894929  -1440.028863     21.078699
PR12  19217.299478  -1212.172516  16759.938330     53.571402
PR14 -14617.527302 -13666.533929  15846.419464     28.173743
PR15 -23868.599981  -9006.336534   -360.339241     68.991077
PR16 -19255.468016   1062.289636 -16707.339633     35.355637
PR17  -2946.267475 -11070.459982 -22792.683509    186.621858
PR18 -15995.926368    879.066868 -19858.214971    198.842171
PR19 -21496.029779  10568.412362  -8772.504996    -49.194267
PR20 -14233.093291  17090.564361  12531.598007   -129.330316
PR21   1224.640743  12023.525848  22453.132512   -196.936186
PR22  16996.290484  -1299.852021  18993.084926    -65.444132
PR24  12809.369807 -17979.918545 -12781.758695   -102.180059
PR26  11091.152976  11978.920277 -19565.139240   -132.015602
PR27  -1905.791651  22806.532714  11294.902271   -169.268606
PE02 -22635.732924  -6217.129018 -18032.792433    215.232424
PE03 -17989.977897   3682.297790  23212.747558   -188.093184
PE04  14999.793693 -23058.432257 -10934.378316   -218.229562
PE05 -10342.641508 -17437.526868  21579.667140   4847.322484
PE06   9890.766129 -27843.987728  -1781.949220   -110.932946
PE07  -3570.006631  28502.764147  -7090.917864   -181.562347
PE08 -15214.871200  22621.597739  11510.214050   -456.034344
PE09   3671.691521 -28515.251667   7097.252744   -866.620921
PE10  13544.820712 -16534.004229 -20484.597001   -705.803734
PE11   1753.459606 -16596.061412 -24458.097230  -1989.925010
PE12  21887.091172 -14148.038865 -14018.618972  -1414.876484
PE13  -2380.843693  16590.257446  24400.570714    -33.568568
PE14 -17390.431583  -8081.539612 -21902.459355 999999.999999
PE15 -22452.375874  13847.774705  13445.785981   -170.340796
PE16 -14941.673897 -24264.864278   8042.142273    -34.723698
PE18  18965.601364 -12877.678031  17500.958745 999999.999999
PE19  17648.108317  -1984.818356 -23668.097328   1694.909257
PE21  20746.304834  21062.488269   1429.828967   -661.033248
PE23  22617.381035   5953.512996  18128.305613      1.882810
PE24  -6492.271790 -23853.191996  16250.793399  -1402.313084
PE25 -20527.189161 -21304.090205   -961.642170     16.623523
PE26  19032.337829   9491.325689  20586.241841    -57.156629
PE27   6459.000125  23795.396993 -16384.344738   -645.514475
PE29  10630.037888  17207.869351 -21608.051431    -91.040787
PE30 -11283.159933  12709.267080 -24248.810703  -1493.927773
PE31  11447.452076 -12454.088788  24293.370315    -64.719943
PE33  28937.476917  -2771.121821   5571.980278     15.250256
PE34 -28973.304055   3050.646347  -5240.350854   -177.567616
PE36 -18649.655362  -9601.982962 -20891.661852   -386.386305
*  2025  4 27 10  5  0.00000000
PG01   1026.879290 -15811.938317 -21315.322068    238.653084               P   P
PG02  11105.968048 -17437.745322 -15980.673653   -184.185766               P   P
PG03  -6433.991516 -19812.571818 -16722.772290    688.283921               P   P
PG04    -65.093765 -25376.373770   7595.174047    559.255085               P   P
PG05  -4615.741118  14682.941078  21458.769348   -208.353931               P   P
PG06 -25585.057116   5420.610526  -4953.277098   -327.421869               P   P
PG07 -15279.354664  -9696.356974  20145.775415    -23.650324               P   P
PG08   4877.349084 -25753.204418   2142.855978    513.191098               P   P
PG09  -8500.430253 -19323.213397  15993.062836    642.977635               P   P
PG10  22229.216294   8420.435422 -11946.530255   -404.348226               P   P
PG11 -22978.550713  11769.800075   6138.325065   -722.512794               P   P
PG12   4740.075999  20487.024787 -16530.930659   -581.922055               P   P
PG13 -13449.194788  20598.108199   9439.943943    700.734864               P   P
PG14 -22620.904351 -10594.414585  -9345.333970    636.895995               P   P
PG15  -1206.157927  26392.450136   1401.478937    289.921979               P   P
PG16  12112.651447  -9141.892005  21458.658355      2.419408               P   P
PG17 -13914.553533  -5901.200245 -21430.168087    308.516566               P   P
PG18  16717.674725   7977.222353  19154.567698   -632.312681               P   P
PG19 -16130.851753   4192.056668 -20700.538869    622.895740               P   P
PG20 -16349.616749   6811.399976  19847.902246    364.802470               P   P
PG22 -20595.434195  -5332.652316 -15330.699095   -102.760500               P   P
PG23  20218.202612  17350.454105   1641.538080    466.896836               P   P
PG24  -4882.180119  18143.371404 -19105.254818   -414.284591               P   P
PG26  19617.680955  -2526.812416  17703.198357   -208.197926               P   P
PG27  12329.832617 -19116.325280  13016.696188    -26.989524               P   P
PG28  25007.082110  -4076.244364  -7987.556117   -603.373936               P   P
PG29   6040.905696  20484.401868  15652.649397   -513.274823               P   P
PG30 -23501.019914  -1224.587719  12744.588662   -159.524776               P   P
PG31  25038.272702  -9341.619345    973.616051   -208.998224               P   P
PG32  15084.067943    576.257869 -21599.030494   -466.943114               P   P
PR02   1389.051937 -22941.786481 -10947.412204    -33.688731               P   P
PR03  -7442.711870 -23321.587203   7124.161837    121.297315               P   P
PR04 -11166.617961 -10972.153879  20170.344566    288.428447               P   P
PR05  -9351.892094   7275.996601  22606.839050    148.953834               P   P
PR07   7392.116030  23379.270296  -7066.850713    104.926764               P   P
PR08  11361.407390  10624.346256 -20160.294688     -5.233102               P   P
PR09  -3635.171760  10717.846922 -22838.973002    251.643647               P   P
PR11  23489.908497   9592.689283  -2507.965585     21.081217               P   P
PR12  19857.739710  -1008.249169  16009.207296     53.571738               P   P
PR14 -14001.680551 -13413.653016  16600.600859     28.173439               P   P
PR15 -23878.161342  -8962.608789    708.120176     68.991445               P   P
PR16 -19897.940999    864.495001 -15950.323955     35.355799               P   P
PR17  -2228.874359 -11666.541386 -22576.903894    186.622574               P   P
PR18 -15324.031116    394.126875 -20394.681932    198.842314               P   P
PR19 -21150.015198  10395.720090  -9764.424029    -49.194465               P   P
PR20 -14596.792707  17433.531027  11607.711995   -129.329972               P   P
PR21    515.806186  12607.050160  22158.053265   -196.938970               P   P
PR22  16349.639075   -843.803062  19578.458623    -65.443836               P   P
PR24  13176.131580 -18332.397276 -11877.476381   -102.180344               P   P
PR26  11253.795437  12756.823166 -18971.579810   -132.015906               P   P
PR27  -1937.227150  22322.859253  12218.290668   -169.268809               P   P
PE02 -23009.937456  -6597.029293 -17413.017945    215.233296               P   P
PE03 -17786.928568   3009.950271  23465.276560   -188.093895               P   P
PE04  14886.778755 -22735.757202 -11733.995770   -218.232521               P   P
PE05 -10170.623343 -18056.425465  21148.261807   4847.323438               P   P
PE06   9858.943815 -27783.637060  -2677.950548   -110.934412               P   P
PE07  -3609.149715  28700.493008  -6221.045496   -181.562961               P   P
PE08 -15093.024964  22283.917315  12302.852105   -456.036288               P   P
PE09   3705.083766 -28714.095320   6224.299553   -866.625343               P   P
PE10  14130.653745 -16703.682263 -19943.841367   -705.804510               P   P
PE11   2465.850998 -16772.317460 -24275.197002  -1989.825285               P   P
PE12  22277.888659 -14281.457763 -13245.478692  -1414.932139               P   P
PE13  -3091.528426  16768.496833  24198.578417    -33.568711               P   P
PE14 -17334.755190  -8758.817246 -21912.651338 999999.999999               P   P
PE15 -22825.089926  13981.175550  12657.262362   -170.341465               P   P
PE16 -14781.358202 -24065.822899   8891.012984    -34.724425               P   P
PE18  18760.540320 -12256.482050  17874.152758 999999.999999               P   P
PE19  17436.418124  -1300.614344 -23871.256162   1694.923927               P   P
PE21  20781.011516  21070.416872    526.045360   -661.033974               P   P
PE23  22997.319871   6332.972546  17511.243179      1.882405               P   P
PE24  -6159.785869 -23479.710635  16910.386479  -1402.319542               P   P
PE25 -20552.221843 -21301.646348    -57.354619     16.623659               P   P
PE26  18415.405479   9610.971205  21086.011819    -57.153362               P   P
PE27   6125.006067  23419.346688 -17041.223868   -645.517522               P   P
PE29  10453.218607  17827.133917 -21188.348582    -91.041819               P   P
PE30 -11781.188945  12176.461348 -24285.149222  -1493.939922               P   P
PE31  11948.258606 -11921.308005  24319.470941    -64.720433               P   P
PE33  28749.085295  -2779.249361   6470.719577     15.250200               P   P
PE34 -28794.746349   3060.808833  -6141.910730   -177.568322               P   P
PE36 -18022.868893  -9719.913342 -21381.131683   -386.387906               P   P
EOF
|};
  [%expect {|
    ((unconsumed ((buf "") (off 12770) (len 0)))
     (result
      ((raw_header
        ((version
          ((version C) (pos_or_vel Pos) (year 2025) (month 4) (day_of_month 26)
           (hour 18) (minute 0) (second 0) (number_of_epochs 577)
           (data_used "d+D  ") (coordinate_system IGb20) (orbit_used EXT)
           (agency AIUB)))
         (time_info
          ((gps_week 2363) (seconds_of_week 583200) (epoch_interval 300)
           (modified_julian_day 60791) (fractional_day 0.75)))
         (space_vehicles
          (((number (80))
            (ids
             (((kind Gps) (prn 1)) ((kind Gps) (prn 2)) ((kind Gps) (prn 3))
              ((kind Gps) (prn 4)) ((kind Gps) (prn 5)) ((kind Gps) (prn 6))
              ((kind Gps) (prn 7)) ((kind Gps) (prn 8)) ((kind Gps) (prn 9))
              ((kind Gps) (prn 10)) ((kind Gps) (prn 11)) ((kind Gps) (prn 12))
              ((kind Gps) (prn 13)) ((kind Gps) (prn 14)) ((kind Gps) (prn 15))
              ((kind Gps) (prn 16)) ((kind Gps) (prn 17)))))
           ((number ())
            (ids
             (((kind Gps) (prn 18)) ((kind Gps) (prn 19)) ((kind Gps) (prn 20))
              ((kind Gps) (prn 22)) ((kind Gps) (prn 23)) ((kind Gps) (prn 24))
              ((kind Gps) (prn 26)) ((kind Gps) (prn 27)) ((kind Gps) (prn 28))
              ((kind Gps) (prn 29)) ((kind Gps) (prn 30)) ((kind Gps) (prn 31))
              ((kind Gps) (prn 32)) ((kind Glonass) (prn 2))
              ((kind Glonass) (prn 3)) ((kind Glonass) (prn 4))
              ((kind Glonass) (prn 5)))))
           ((number ())
            (ids
             (((kind Glonass) (prn 7)) ((kind Glonass) (prn 8))
              ((kind Glonass) (prn 9)) ((kind Glonass) (prn 11))
              ((kind Glonass) (prn 12)) ((kind Glonass) (prn 14))
              ((kind Glonass) (prn 15)) ((kind Glonass) (prn 16))
              ((kind Glonass) (prn 17)) ((kind Glonass) (prn 18))
              ((kind Glonass) (prn 19)) ((kind Glonass) (prn 20))
              ((kind Glonass) (prn 21)) ((kind Glonass) (prn 22))
              ((kind Glonass) (prn 24)) ((kind Glonass) (prn 26))
              ((kind Glonass) (prn 27)))))
           ((number ())
            (ids
             (((kind Galileo) (prn 2)) ((kind Galileo) (prn 3))
              ((kind Galileo) (prn 4)) ((kind Galileo) (prn 5))
              ((kind Galileo) (prn 6)) ((kind Galileo) (prn 7))
              ((kind Galileo) (prn 8)) ((kind Galileo) (prn 9))
              ((kind Galileo) (prn 10)) ((kind Galileo) (prn 11))
              ((kind Galileo) (prn 12)) ((kind Galileo) (prn 13))
              ((kind Galileo) (prn 14)) ((kind Galileo) (prn 15))
              ((kind Galileo) (prn 16)) ((kind Galileo) (prn 18))
              ((kind Galileo) (prn 19)))))
           ((number ())
            (ids
             (((kind Galileo) (prn 21)) ((kind Galileo) (prn 23))
              ((kind Galileo) (prn 24)) ((kind Galileo) (prn 25))
              ((kind Galileo) (prn 26)) ((kind Galileo) (prn 27))
              ((kind Galileo) (prn 29)) ((kind Galileo) (prn 30))
              ((kind Galileo) (prn 31)) ((kind Galileo) (prn 33))
              ((kind Galileo) (prn 34)) ((kind Galileo) (prn 36)))))))
         (accuracy
          (((accuracy_exponent
             ((7) (7) (7) (5) (7) (6) (7) (5) (6) (7) (6) (7) (6) (5) (7)
              (7) (7))))
           ((accuracy_exponent
             ((6) (7) (7) (7) (7) (6) (6) (5) (6) (7) (6) (7) (6) (7) (7)
              (7) (7))))
           ((accuracy_exponent
             ((7) (7) (8) (8) (8) (7) (8) (8) (7) (6) (8) (8) (7) (6) (7)
              (8) (8))))
           ((accuracy_exponent
             ((6) (7) (6) (6) (6) (6) (6) (6) (7) (6) (6) (7) (7) (7) (6)
              (7) (6))))
           ((accuracy_exponent
             ((6) (6) (6) (7) (7) (6) (7) (6) (6) (7) (7) (7) () () () () ())))))
         (type_and_time ((file_type Mixed) (time_system Gps)))
         (base ((position_velocity 1.25) (clock 1.025)))
         (comments (() () () ()))))
       (space_vehicles
        (((kind Gps) (prn 1)) ((kind Gps) (prn 2)) ((kind Gps) (prn 3))
         ((kind Gps) (prn 4)) ((kind Gps) (prn 5)) ((kind Gps) (prn 6))
         ((kind Gps) (prn 7)) ((kind Gps) (prn 8)) ((kind Gps) (prn 9))
         ((kind Gps) (prn 10)) ((kind Gps) (prn 11)) ((kind Gps) (prn 12))
         ((kind Gps) (prn 13)) ((kind Gps) (prn 14)) ((kind Gps) (prn 15))
         ((kind Gps) (prn 16)) ((kind Gps) (prn 17)) ((kind Gps) (prn 18))
         ((kind Gps) (prn 19)) ((kind Gps) (prn 20)) ((kind Gps) (prn 22))
         ((kind Gps) (prn 23)) ((kind Gps) (prn 24)) ((kind Gps) (prn 26))
         ((kind Gps) (prn 27)) ((kind Gps) (prn 28)) ((kind Gps) (prn 29))
         ((kind Gps) (prn 30)) ((kind Gps) (prn 31)) ((kind Gps) (prn 32))
         ((kind Glonass) (prn 2)) ((kind Glonass) (prn 3))
         ((kind Glonass) (prn 4)) ((kind Glonass) (prn 5))
         ((kind Glonass) (prn 7)) ((kind Glonass) (prn 8))
         ((kind Glonass) (prn 9)) ((kind Glonass) (prn 11))
         ((kind Glonass) (prn 12)) ((kind Glonass) (prn 14))
         ((kind Glonass) (prn 15)) ((kind Glonass) (prn 16))
         ((kind Glonass) (prn 17)) ((kind Glonass) (prn 18))
         ((kind Glonass) (prn 19)) ((kind Glonass) (prn 20))
         ((kind Glonass) (prn 21)) ((kind Glonass) (prn 22))
         ((kind Glonass) (prn 24)) ((kind Glonass) (prn 26))
         ((kind Glonass) (prn 27)) ((kind Galileo) (prn 2))
         ((kind Galileo) (prn 3)) ((kind Galileo) (prn 4))
         ((kind Galileo) (prn 5)) ((kind Galileo) (prn 6))
         ((kind Galileo) (prn 7)) ((kind Galileo) (prn 8))
         ((kind Galileo) (prn 9)) ((kind Galileo) (prn 10))
         ((kind Galileo) (prn 11)) ((kind Galileo) (prn 12))
         ((kind Galileo) (prn 13)) ((kind Galileo) (prn 14))
         ((kind Galileo) (prn 15)) ((kind Galileo) (prn 16))
         ((kind Galileo) (prn 18)) ((kind Galileo) (prn 19))
         ((kind Galileo) (prn 21)) ((kind Galileo) (prn 23))
         ((kind Galileo) (prn 24)) ((kind Galileo) (prn 25))
         ((kind Galileo) (prn 26)) ((kind Galileo) (prn 27))
         ((kind Galileo) (prn 29)) ((kind Galileo) (prn 30))
         ((kind Galileo) (prn 31)) ((kind Galileo) (prn 33))
         ((kind Galileo) (prn 34)) ((kind Galileo) (prn 36))))
       (accuracy
        (0.128 0.128 0.128 0.032 0.128 0.064 0.128 0.032 0.064 0.128 0.064 0.128
         0.064 0.032 0.128 0.128 0.128 0.064 0.128 0.128 0.128 0.128 0.064 0.064
         0.032 0.064 0.128 0.064 0.128 0.064 0.128 0.128 0.128 0.128 0.128 0.128
         0.256 0.256 0.256 0.128 0.256 0.256 0.128 0.064 0.256 0.256 0.128 0.064
         0.128 0.256 0.256 0.064 0.128 0.064 0.064 0.064 0.064 0.064 0.064 0.128
         0.064 0.064 0.128 0.128 0.128 0.064 0.128 0.064 0.064 0.064 0.064 0.128
         0.128 0.064 0.128 0.064 0.064 0.128 0.128 0.128))
       (epochs
        (((metadata
           ((year 2025) (month 4) (day_of_month 27) (hour 10) (minute 0)
            (second 0)))
          (x
           (0.235804082 10.487194542 -6.919625534 -0.205738626 -3.952027413
            -25.731217268 -15.948018014 4.779274875 -8.653755353 21.89538751
            -22.716713952 5.208189505 -13.492372234 -22.396870168 -1.097531677
            11.518385978 -14.124810864 17.385153722 -16.315433975 -15.709532012
            -20.208498108 20.286179223 -4.216954589 19.08192023 12.331704077
            25.245850917 6.250967002 -23.907319478 24.952200005 15.215696426
            1.375428278 -7.339839808 -11.011944404 -9.232506457 7.287819889
            11.20030283 -2.744975142 23.549612629 19.217299478 -14.617527302
            -23.868599981 -19.255468016 -2.946267475 -15.995926368 -21.496029779
            -14.233093291 1.224640743 16.996290484 12.809369807 11.091152976
            -1.905791651 -22.635732924 -17.989977897 14.999793693 -10.342641508
            9.890766129 -3.570006631 -15.2148712 3.671691521 13.544820712
            1.753459606 21.887091172 -2.380843693 -17.390431583 -22.452375874
            -14.941673897 18.965601364 17.648108317 20.746304834 22.617381035
            -6.49227179 -20.527189161 19.032337829 6.459000125 10.630037888
            -11.283159933 11.447452076 28.937476917 -28.973304055 -18.649655362))
          (y
           (-15.613737056 -17.225551881 -20.185884046 -25.09357523 15.173450524
            5.526056908 -9.426392166 -25.837572821 -18.712502592 8.077276871
            11.770922217 20.868348667 20.957992491 -10.2849175 26.448730968
            -9.741271962 -5.081053914 7.718711652 4.947848663 7.220134075
            -4.842174939 17.333245904 17.882788452 -2.97256425 -19.625483807
            -4.25403262 19.905835604 -0.990749905 -9.399490809 -0.252295589
            -23.378199733 -23.022565341 -10.148625712 8.191085989 23.078924858
            9.795828356 11.016414286 9.666894929 -1.212172516 -13.666533929
            -9.006336534 1.062289636 -11.070459982 0.879066868 10.568412362
            17.090564361 12.023525848 -1.299852021 -17.979918545 11.978920277
            22.806532714 -6.217129018 3.68229779 -23.058432257 -17.437526868
            -27.843987728 28.502764147 22.621597739 -28.515251667 -16.534004229
            -16.596061412 -14.148038865 16.590257446 -8.081539612 13.847774705
            -24.264864278 -12.877678031 -1.984818356 21.062488269 5.953512996
            -23.853191996 -21.304090205 9.491325689 23.795396993 17.207869351
            12.70926708 -12.454088788 -2.771121821 3.050646347 -9.601982962))
          (z
           (-21.484878594 -16.612076 -16.066056265 8.487492367 21.248946963
            -4.004995003 19.753242843 1.192895915 16.622992614 -12.747295606
            7.050546985 -15.890328345 8.560040101 -10.179988755 0.462318603
            21.508121734 -21.507664181 18.66612072 -20.37085921 20.221699721
            -15.987900304 0.679639946 -19.527068407 18.195373399 12.240866465
            -7.093743062 16.304953023 11.983815263 1.912944348 -21.508018788
            -9.98656672 8.139989019 20.681163022 22.341586047 -8.081578219
            -20.661314311 -22.818848203 -1.440028863 16.75993833 15.846419464
            -0.360339241 -16.707339633 -22.792683509 -19.858214971 -8.772504996
            12.531598007 22.453132512 18.993084926 -12.781758695 -19.56513924
            11.294902271 -18.032792433 23.212747558 -10.934378316 21.57966714
            -1.78194922 -7.090917864 11.51021405 7.097252744 -20.484597001
            -24.45809723 -14.018618972 24.400570714 -21.902459355 13.445785981
            8.042142273 17.500958745 -23.668097328 1.429828967 18.128305613
            16.250793399 -0.96164217 20.586241841 -16.384344738 -21.608051431
            -24.248810703 24.293370315 5.571980278 -5.240350854 -20.891661852))
          (clock
           (0.000238648743 -0.000184188178 0.000688282918 0.000559253891
            -0.00020835354 -0.000327415601 -2.3650206E-05 0.000513189363
            0.000642973926 -0.000404344814 -0.000722514869 -0.000581921715
            0.000700734796 0.000636894558 0.000289920736 2.416946E-06
            0.000308522372 -0.000632313753 0.000622894619 0.000364802538
            -0.000102760398 0.000466894689 -0.000414286395 -0.000208191923
            -2.6989939E-05 -0.00060337218 -0.000513277912 -0.000159527335
            -0.000208998761 -0.000466947284 -3.3688093E-05 0.000121297668
            0.000288427719 0.000148953238 0.000104925869 -5.233345E-06
            0.000251642532 2.1078699E-05 5.3571402E-05 2.8173743E-05
            6.8991077E-05 3.5355637E-05 0.000186621858 0.000198842171
            -4.9194267E-05 -0.000129330316 -0.000196936186 -6.5444132E-05
            -0.000102180059 -0.000132015602 -0.000169268606 0.000215232424
            -0.000188093184 -0.000218229562 0.004847322484 -0.000110932946
            -0.000181562347 -0.000456034344 -0.000866620921 -0.000705803734
            -0.00198992501 -0.001414876484 -3.3568568E-05 None -0.000170340796
            -3.4723698E-05 None 0.001694909257 -0.000661033248 1.88281E-06
            -0.001402313084 1.6623523E-05 -5.7156629E-05 -0.000645514475
            -9.1040787E-05 -0.001493927773 -6.4719943E-05 1.5250256E-05
            -0.000177567616 -0.000386386305))
          (x_stddev (None 80)) (y_stddev (None 80)) (z_stddev (None 80))
          (clock_stddev (None 80)) (maneuver (Empty 80)) (orbit_pred (Empty 80))
          (clock_event (Empty 80)) (clock_pred (Empty 80)) (velocity ()))
         ((metadata
           ((year 2025) (month 4) (day_of_month 27) (hour 10) (minute 5)
            (second 0)))
          (x
           (1.02687929 11.105968048 -6.433991516 -0.065093765 -4.615741118
            -25.585057116 -15.279354664 4.877349084 -8.500430253 22.229216294
            -22.978550713 4.740075999 -13.449194788 -22.620904351 -1.206157927
            12.112651447 -13.914553533 16.717674725 -16.130851753 -16.349616749
            -20.595434195 20.218202612 -4.882180119 19.617680955 12.329832617
            25.00708211 6.040905696 -23.501019914 25.038272702 15.084067943
            1.389051937 -7.44271187 -11.166617961 -9.351892094 7.39211603
            11.36140739 -3.63517176 23.489908497 19.85773971 -14.001680551
            -23.878161342 -19.897940999 -2.228874359 -15.324031116 -21.150015198
            -14.596792707 0.515806186 16.349639075 13.17613158 11.253795437
            -1.93722715 -23.009937456 -17.786928568 14.886778755 -10.170623343
            9.858943815 -3.609149715 -15.093024964 3.705083766 14.130653745
            2.465850998 22.277888659 -3.091528426 -17.33475519 -22.825089926
            -14.781358202 18.76054032 17.436418124 20.781011516 22.997319871
            -6.159785869 -20.552221843 18.415405479 6.125006067 10.453218607
            -11.781188945 11.948258606 28.749085295 -28.794746349 -18.022868893))
          (y
           (-15.811938317 -17.437745322 -19.812571818 -25.37637377 14.682941078
            5.420610526 -9.696356974 -25.753204418 -19.323213397 8.420435422
            11.769800075 20.487024787 20.598108199 -10.594414585 26.392450136
            -9.141892005 -5.901200245 7.977222353 4.192056668 6.811399976
            -5.332652316 17.350454105 18.143371404 -2.526812416 -19.11632528
            -4.076244364 20.484401868 -1.224587719 -9.341619345 0.576257869
            -22.941786481 -23.321587203 -10.972153879 7.275996601 23.379270296
            10.624346256 10.717846922 9.592689283 -1.008249169 -13.413653016
            -8.962608789 0.864495001 -11.666541386 0.394126875 10.39572009
            17.433531027 12.60705016 -0.843803062 -18.332397276 12.756823166
            22.322859253 -6.597029293 3.009950271 -22.735757202 -18.056425465
            -27.78363706 28.700493008 22.283917315 -28.71409532 -16.703682263
            -16.77231746 -14.281457763 16.768496833 -8.758817246 13.98117555
            -24.065822899 -12.25648205 -1.300614344 21.070416872 6.332972546
            -23.479710635 -21.301646348 9.610971205 23.419346688 17.827133917
            12.176461348 -11.921308005 -2.779249361 3.060808833 -9.719913342))
          (z
           (-21.315322068 -15.980673653 -16.72277229 7.595174047 21.458769348
            -4.953277098 20.145775415 2.142855978 15.993062836 -11.946530255
            6.138325065 -16.530930659 9.439943943 -9.34533397 1.401478937
            21.458658355 -21.430168087 19.154567698 -20.700538869 19.847902246
            -15.330699095 1.64153808 -19.105254818 17.703198357 13.016696188
            -7.987556117 15.652649397 12.744588662 0.973616051 -21.599030494
            -10.947412204 7.124161837 20.170344566 22.60683905 -7.066850713
            -20.160294688 -22.838973002 -2.507965585 16.009207296 16.600600859
            0.708120176 -15.950323955 -22.576903894 -20.394681932 -9.764424029
            11.607711995 22.158053265 19.578458623 -11.877476381 -18.97157981
            12.218290668 -17.413017945 23.46527656 -11.73399577 21.148261807
            -2.677950548 -6.221045496 12.302852105 6.224299553 -19.943841367
            -24.275197002 -13.245478692 24.198578417 -21.912651338 12.657262362
            8.891012984 17.874152758 -23.871256162 0.52604536 17.511243179
            16.910386479 -0.057354619 21.086011819 -17.041223868 -21.188348582
            -24.285149222 24.319470941 6.470719577 -6.14191073 -21.381131683))
          (clock
           (0.000238653084 -0.000184185766 0.000688283921 0.000559255085
            -0.000208353931 -0.000327421869 -2.3650324E-05 0.000513191098
            0.000642977635 -0.000404348226 -0.000722512794 -0.000581922055
            0.000700734864 0.000636895995 0.000289921979 2.419408E-06
            0.000308516566 -0.000632312681 0.00062289574 0.00036480247
            -0.0001027605 0.000466896836 -0.000414284591 -0.000208197926
            -2.6989524E-05 -0.000603373936 -0.000513274823 -0.000159524776
            -0.000208998224 -0.000466943114 -3.3688731E-05 0.000121297315
            0.000288428447 0.000148953834 0.000104926764 -5.233102E-06
            0.000251643647 2.1081217E-05 5.3571738E-05 2.8173439E-05
            6.8991445E-05 3.5355799E-05 0.000186622574 0.000198842314
            -4.9194465E-05 -0.000129329972 -0.00019693897 -6.5443836E-05
            -0.000102180344 -0.000132015906 -0.000169268809 0.000215233296
            -0.000188093895 -0.000218232521 0.004847323438 -0.000110934412
            -0.000181562961 -0.000456036288 -0.000866625343 -0.00070580451
            -0.001989825285 -0.001414932139 -3.3568711E-05 None -0.000170341465
            -3.4724425E-05 None 0.001694923927 -0.000661033974 1.882405E-06
            -0.001402319542 1.6623659E-05 -5.7153362E-05 -0.000645517522
            -9.1041819E-05 -0.001493939922 -6.4720433E-05 1.52502E-05
            -0.000177568322 -0.000386387906))
          (x_stddev (None 80)) (y_stddev (None 80)) (z_stddev (None 80))
          (clock_stddev (None 80)) (maneuver (Empty 80)) (orbit_pred (Full 80))
          (clock_event (Empty 80)) (clock_pred (Full 80)) (velocity ()))))
       (presence
        ((pos (Full 80))
         (clock
          11111111111111111111111111111111111111111111111111111111111111101101111111111111)
         (pos_stddev (Empty 80)) (clock_stddev (Empty 80)) (maneuver (Empty 80))
         (clock_event (Empty 80)) (velocity (Empty 80))
         (clock_velocity (Empty 80)) (velocity_stddev (Full 80))
         (clock_velocity_stddev (Full 80)))))))
    |}]

let%expect_test "Processed file" =
  expect_test_f (module Processed_file)
    {|#dV2025  4 26  6  0  0.00000000     577 ORBIT IGS20 BHN ESOC                    
## 2363 540000.00000000   300.00000000 60791 0.2500000000000                    
+   51   G13G22G07G05G20G31G17G15G16G29G12G19G02G25G30G24G27                    
+        G06G09G03G32G26G08G10G04G18G23G14G11G28G01R09R26R11                    
+        R22R20R19R08R03R07R02R17R14R18R21R05R15R12R04R24R16                    
+          0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0                    
+          0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0                    
++         4  5  4  5  5  5  5  5  5  4  4  4  4  8  4  5  5                    
++         4  4  4  5  5  5  5  4  4  4  5  4  5  4  5  5  6                    
++         5  5  6  6  5  6  5  5  5  5  5  6  6  6  5  5  5                    
++         0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0                    
++         0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0                    
%c M  cc GPS ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc                    
%c cc cc ccc ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc                    
%f  0.0000000  0.000000000  0.00000000000  0.000000000000000                    
%f  0.0000000  0.000000000  0.00000000000  0.000000000000000                    
%i    0    0    0    0      0      0      0      0         0                    
%i    0    0    0    0      0      0      0      0         0                    
/*   EUROPEAN SPACE OPERATIONS CENTRE - DARMSTADT, GERMANY
/* ---------------------------------------------------------
/*  SP3 FILE GENERATED BY EPNS BAHN TOOL  (DETERMINATION)
/* PCV:ESA23_2357 OL/AL:EOT11A   NONE     YN ORB:CoN CLK:CoN                    
EOF
|};
  [%expect {|
    ((unconsumed ((buf "") (off 1721) (len 0)))
     (result
      ((raw_header
        ((version
          ((version D) (pos_or_vel Vel) (year 2025) (month 4) (day_of_month 26)
           (hour 6) (minute 0) (second 0) (number_of_epochs 577)
           (data_used ORBIT) (coordinate_system IGS20) (orbit_used BHN)
           (agency ESOC)))
         (time_info
          ((gps_week 2363) (seconds_of_week 540000) (epoch_interval 300)
           (modified_julian_day 60791) (fractional_day 0.25)))
         (space_vehicles
          (((number (51))
            (ids
             (((kind Gps) (prn 13)) ((kind Gps) (prn 22)) ((kind Gps) (prn 7))
              ((kind Gps) (prn 5)) ((kind Gps) (prn 20)) ((kind Gps) (prn 31))
              ((kind Gps) (prn 17)) ((kind Gps) (prn 15)) ((kind Gps) (prn 16))
              ((kind Gps) (prn 29)) ((kind Gps) (prn 12)) ((kind Gps) (prn 19))
              ((kind Gps) (prn 2)) ((kind Gps) (prn 25)) ((kind Gps) (prn 30))
              ((kind Gps) (prn 24)) ((kind Gps) (prn 27)))))
           ((number ())
            (ids
             (((kind Gps) (prn 6)) ((kind Gps) (prn 9)) ((kind Gps) (prn 3))
              ((kind Gps) (prn 32)) ((kind Gps) (prn 26)) ((kind Gps) (prn 8))
              ((kind Gps) (prn 10)) ((kind Gps) (prn 4)) ((kind Gps) (prn 18))
              ((kind Gps) (prn 23)) ((kind Gps) (prn 14)) ((kind Gps) (prn 11))
              ((kind Gps) (prn 28)) ((kind Gps) (prn 1)) ((kind Glonass) (prn 9))
              ((kind Glonass) (prn 26)) ((kind Glonass) (prn 11)))))
           ((number ())
            (ids
             (((kind Glonass) (prn 22)) ((kind Glonass) (prn 20))
              ((kind Glonass) (prn 19)) ((kind Glonass) (prn 8))
              ((kind Glonass) (prn 3)) ((kind Glonass) (prn 7))
              ((kind Glonass) (prn 2)) ((kind Glonass) (prn 17))
              ((kind Glonass) (prn 14)) ((kind Glonass) (prn 18))
              ((kind Glonass) (prn 21)) ((kind Glonass) (prn 5))
              ((kind Glonass) (prn 15)) ((kind Glonass) (prn 12))
              ((kind Glonass) (prn 4)) ((kind Glonass) (prn 24))
              ((kind Glonass) (prn 16)))))
           ((number ()) (ids ())) ((number ()) (ids ()))))
         (accuracy
          (((accuracy_exponent
             ((4) (5) (4) (5) (5) (5) (5) (5) (5) (4) (4) (4) (4) (8) (4)
              (5) (5))))
           ((accuracy_exponent
             ((4) (4) (4) (5) (5) (5) (5) (4) (4) (4) (5) (4) (5) (4) (5)
              (5) (6))))
           ((accuracy_exponent
             ((5) (5) (6) (6) (5) (6) (5) (5) (5) (5) (5) (6) (6) (6) (5)
              (5) (5))))
           ((accuracy_exponent
             (() () () () () () () () () () () () () () () () ())))
           ((accuracy_exponent
             (() () () () () () () () () () () () () () () () ())))))
         (type_and_time ((file_type Mixed) (time_system Gps)))
         (base ((position_velocity 0) (clock 0))) (comments (() () () ()))))
       (space_vehicles
        (((kind Gps) (prn 13)) ((kind Gps) (prn 22)) ((kind Gps) (prn 7))
         ((kind Gps) (prn 5)) ((kind Gps) (prn 20)) ((kind Gps) (prn 31))
         ((kind Gps) (prn 17)) ((kind Gps) (prn 15)) ((kind Gps) (prn 16))
         ((kind Gps) (prn 29)) ((kind Gps) (prn 12)) ((kind Gps) (prn 19))
         ((kind Gps) (prn 2)) ((kind Gps) (prn 25)) ((kind Gps) (prn 30))
         ((kind Gps) (prn 24)) ((kind Gps) (prn 27)) ((kind Gps) (prn 6))
         ((kind Gps) (prn 9)) ((kind Gps) (prn 3)) ((kind Gps) (prn 32))
         ((kind Gps) (prn 26)) ((kind Gps) (prn 8)) ((kind Gps) (prn 10))
         ((kind Gps) (prn 4)) ((kind Gps) (prn 18)) ((kind Gps) (prn 23))
         ((kind Gps) (prn 14)) ((kind Gps) (prn 11)) ((kind Gps) (prn 28))
         ((kind Gps) (prn 1)) ((kind Glonass) (prn 9)) ((kind Glonass) (prn 26))
         ((kind Glonass) (prn 11)) ((kind Glonass) (prn 22))
         ((kind Glonass) (prn 20)) ((kind Glonass) (prn 19))
         ((kind Glonass) (prn 8)) ((kind Glonass) (prn 3))
         ((kind Glonass) (prn 7)) ((kind Glonass) (prn 2))
         ((kind Glonass) (prn 17)) ((kind Glonass) (prn 14))
         ((kind Glonass) (prn 18)) ((kind Glonass) (prn 21))
         ((kind Glonass) (prn 5)) ((kind Glonass) (prn 15))
         ((kind Glonass) (prn 12)) ((kind Glonass) (prn 4))
         ((kind Glonass) (prn 24)) ((kind Glonass) (prn 16))))
       (accuracy
        (0.016 0.032 0.016 0.032 0.032 0.032 0.032 0.032 0.032 0.016 0.016 0.016
         0.016 0.256 0.016 0.032 0.032 0.016 0.016 0.016 0.032 0.032 0.032 0.032
         0.016 0.016 0.016 0.032 0.016 0.032 0.016 0.032 0.032 0.064 0.032 0.032
         0.064 0.064 0.032 0.064 0.032 0.032 0.032 0.032 0.032 0.064 0.064 0.064
         0.032 0.032 0.032))
       (epochs ())
       (presence
        ((pos (Full 51)) (clock (Full 51)) (pos_stddev (Full 51))
         (clock_stddev (Full 51)) (maneuver (Empty 51)) (clock_event (Empty 51))
         (velocity (Full 51)) (clock_velocity (Full 51))
         (velocity_stddev (Full 51)) (clock_velocity_stddev (Full 51)))))))
    |}]
