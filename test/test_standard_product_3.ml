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

let%expect_test "Epoch block" =
  expect_test_f (module Epoch_block)
    {|*  2025  4 26  6  0  0.00000000
PE02  -4213.196867 -23968.456807 -16850.297415    214.949828 16  9 14
PE03 -10020.938512 -14695.921440  23657.758768   -187.854902 13 13 11|};
  [%expect {|
    ((unconsumed ((buf "") (off 171) (len 0)))
     (result
      (((year 2025) (month 4) (day_of_month 26) (hour 6) (minute 0) (second 0))
       ((((space_vehicle_id ((kind Galileo) (prn 2)))
          (data
           (((x -4213.196867) (y -23968.456807) (z -16850.297415)
             (clock (214.949828)) (x_stddev (16)) (y_stddev (9)) (z_stddev (14))
             (clock_stddev ()) (clock_event false) (clock_pred false)
             (maneuver false) (orbit_pred false)))))
         ())
        (((space_vehicle_id ((kind Galileo) (prn 3)))
          (data
           (((x -10020.938512) (y -14695.92144) (z 23657.758768)
             (clock (-187.854902)) (x_stddev (13)) (y_stddev (13))
             (z_stddev (11)) (clock_stddev ()) (clock_event false)
             (clock_pred false) (maneuver false) (orbit_pred false)))))
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
       (epoch_count 1)
       (epochs
        (((metadata
           ((year 2025) (month 4) (day_of_month 15) (hour 18) (minute 0)
            (second 0)))
          (x
           (20393872.619 14720343.221 26146413.195 -9509177.294 -2117760.416
            15023550.437 21940757.514 24981662.143 -10172959.964 -10630792.008
            -14983036.033 -13246433.399 12522610.311 -22640793.105 3654058.733
            16155109.66 -13413264 5545461.136 -2752462.212 None 9067873.49
            -15995330.659 -21326678.975 -17191541.238 -870794.118 10755189.023
            -62864.197 -25828776.332 6294965.475 7266577.422 -10617852.302 None
            -10294613.059 -22199179.027 -21213646.675 -9009358.745 22297858.604
            21007765.827 7307721.543 9474997.417 1874089.801 -11055582.081
            -9176832.584 -1885673.082 -21368435.811 -12340892.644 540342.304
            17832448.04 21580475.358 11604230.639 -18827905.64 -12341612.85
            -11052522.389 -12069258.035 -27172888.622 -21486424.996 27384231.525
            11366903.922 -27384969.856 -18280999.911 None -13687377.274
            20403533.833 13279028.372 12725489.417 20366125.295 -16626448.403
            -20422369.204 15401597.14 16556681.496 27034919.354 3247815.851
            -3030201.651 1834006.555 -1509395.223 -15475153.688))
          (y
           (16448890.331 2094001.989 1169320.299 -14235560.092 -20992709.061
            -4830815.8 6550547.256 -7496903.396 24338239.79 -23493199.931
            -3461209.039 -13303283.617 -23086256.467 -3994492.679 19589167.494
            -12970267.276 6821976.489 -14816820.052 -23940538.946 None
            -24682843.058 16159816.398 -12954358.805 9663264.946 24820294.326
            13295569.811 19729528.117 4513570.305 -16031227.966 22935850.5
            14983347.645 None -6476174.557 5308759.454 13118015.863 14180403.094
            -5326308.862 -13399554.061 18942873.12 -14756218.117 -25415874.662
            -1934332.66 16364577.549 25424703.327 -7772660.813 3691353.916
            12717839.185 16939423.639 9065594.194 -4744845.787 -16007656.41
            26687645.424 15824575.6 -15254301.896 7077538.26 -11470032.504
            6025589.783 15318725.186 -6013142.02 -22319124.759 None -26117149.535
            14815801.315 26277210.655 -15242301.756 -15880100.468 -4609543.722
            15454583.717 -5558374.352 4648575.879 -7271528.097 22018694.911
            -22167390.509 -21873294.561 22095677.173 5043772.961))
          (z
           (6021533.601 21822302.309 5004661.219 -20466871.746 16113165.882
            -20858416.348 -13990864.107 -5282514.459 390848.002 6530703.82
            21365893.549 -19124608.743 -2811569.394 -13937478.698 -17700767.606
            17139319.904 -21869445.135 21130718.888 -11204470.057 None
            5149231.095 -13480427.49 9170817.18 17306264.844 -9048460.334
            -20752706.461 17770942.862 -4714436.747 -19982023.016 10599871.543
            19359698.564 None -22360553.959 -11277956.168 5356235.273
            19203790.093 11280170.874 -5466597.493 -15477750.218 -18505577.165
            -1188397.548 22917228.089 17305522.091 1335073.119 -11596379.672
            -21996308.706 -22109643.837 -6843586.048 10104321.028 22239104.91
            6358471.841 -3357196.156 22447285.084 -22301711.148 9390873.342
            -16827805.922 9467973.755 22633611.575 -9518042.355 -6621216.66 None
            2423289.091 15517990.151 -3137746.637 -21936255.734 -14469123.92
            24030483.532 14839028.931 24661834.17 -24096375.982 -9615536.471
            -19523889.747 19382689.581 19857486.519 -19643837.565 -24722199.332))
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
          (B0F
           1111111111111111111011111111111011111111111111111111111111110111111111111111))
         (clock
          (B0F
           1111111111111111111011111111111011111111111111111111111111110111111111111111))
         (pos_stddev (Full 76)) (clock_stddev (Full 76)) (maneuver (Empty 76))
         (clock_event (Empty 76)) (velocity (Empty 76))
         (clock_velocity (Empty 76)) (velocity_stddev (Empty 76))
         (clock_velocity_stddev (Empty 76)))))))
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
       (epoch_count 2)
       (epochs
        (((metadata
           ((year 2025) (month 4) (day_of_month 27) (hour 10) (minute 0)
            (second 0)))
          (x
           (235804.082 10487194.542 -6919625.534 -205738.626 -3952027.413
            -25731217.268 -15948018.014 4779274.875 -8653755.353 21895387.51
            -22716713.952 5208189.505 -13492372.234 -22396870.168 -1097531.677
            11518385.978 -14124810.864 17385153.722 -16315433.975 -15709532.012
            -20208498.108 20286179.223 -4216954.589 19081920.23 12331704.077
            25245850.917 6250967.002 -23907319.478 24952200.005 15215696.426
            1375428.278 -7339839.808 -11011944.404 -9232506.457 7287819.889
            11200302.83 -2744975.142 23549612.629 19217299.478 -14617527.302
            -23868599.981 -19255468.016 -2946267.475 -15995926.368 -21496029.779
            -14233093.291 1224640.743 16996290.484 12809369.807 11091152.976
            -1905791.651 -22635732.924 -17989977.897 14999793.693 -10342641.508
            9890766.129 -3570006.631 -15214871.2 3671691.521 13544820.712
            1753459.606 21887091.172 -2380843.693 -17390431.583 -22452375.874
            -14941673.897 18965601.364 17648108.317 20746304.834 22617381.035
            -6492271.79 -20527189.161 19032337.829 6459000.125 10630037.888
            -11283159.933 11447452.076 28937476.917 -28973304.055 -18649655.362))
          (y
           (-15613737.056 -17225551.881 -20185884.046 -25093575.23 15173450.524
            5526056.908 -9426392.166 -25837572.821 -18712502.592 8077276.871
            11770922.217 20868348.667 20957992.491 -10284917.5 26448730.968
            -9741271.962 -5081053.914 7718711.652 4947848.663 7220134.075
            -4842174.939 17333245.904 17882788.452 -2972564.25 -19625483.807
            -4254032.62 19905835.604 -990749.905 -9399490.809 -252295.589
            -23378199.733 -23022565.341 -10148625.712 8191085.989 23078924.858
            9795828.356 11016414.286 9666894.929 -1212172.516 -13666533.929
            -9006336.534 1062289.636 -11070459.982 879066.868 10568412.362
            17090564.361 12023525.848 -1299852.021 -17979918.545 11978920.277
            22806532.714 -6217129.018 3682297.79 -23058432.257 -17437526.868
            -27843987.728 28502764.147 22621597.739 -28515251.667 -16534004.229
            -16596061.412 -14148038.865 16590257.446 -8081539.612 13847774.705
            -24264864.278 -12877678.031 -1984818.356 21062488.269 5953512.996
            -23853191.996 -21304090.205 9491325.689 23795396.993 17207869.351
            12709267.08 -12454088.788 -2771121.821 3050646.347 -9601982.962))
          (z
           (-21484878.594 -16612076 -16066056.265 8487492.367 21248946.963
            -4004995.003 19753242.843 1192895.915 16622992.614 -12747295.606
            7050546.985 -15890328.345 8560040.101 -10179988.755 462318.603
            21508121.734 -21507664.181 18666120.72 -20370859.21 20221699.721
            -15987900.304 679639.946 -19527068.407 18195373.399 12240866.465
            -7093743.062 16304953.023 11983815.263 1912944.348 -21508018.788
            -9986566.72 8139989.019 20681163.022 22341586.047 -8081578.219
            -20661314.311 -22818848.203 -1440028.863 16759938.33 15846419.464
            -360339.241 -16707339.633 -22792683.509 -19858214.971 -8772504.996
            12531598.007 22453132.512 18993084.926 -12781758.695 -19565139.24
            11294902.271 -18032792.433 23212747.558 -10934378.316 21579667.14
            -1781949.22 -7090917.864 11510214.05 7097252.744 -20484597.001
            -24458097.23 -14018618.972 24400570.714 -21902459.355 13445785.981
            8042142.273 17500958.745 -23668097.328 1429828.967 18128305.613
            16250793.399 -961642.17 20586241.841 -16384344.738 -21608051.431
            -24248810.703 24293370.315 5571980.278 -5240350.854 -20891661.852))
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
           (1026879.29 11105968.048 -6433991.516 -65093.765 -4615741.118
            -25585057.116 -15279354.664 4877349.084 -8500430.253 22229216.294
            -22978550.713 4740075.999 -13449194.788 -22620904.351 -1206157.927
            12112651.447 -13914553.533 16717674.725 -16130851.753 -16349616.749
            -20595434.195 20218202.612 -4882180.119 19617680.955 12329832.617
            25007082.11 6040905.696 -23501019.914 25038272.702 15084067.943
            1389051.937 -7442711.87 -11166617.961 -9351892.094 7392116.03
            11361407.39 -3635171.76 23489908.497 19857739.71 -14001680.551
            -23878161.342 -19897940.999 -2228874.359 -15324031.116 -21150015.198
            -14596792.707 515806.186 16349639.075 13176131.58 11253795.437
            -1937227.15 -23009937.456 -17786928.568 14886778.755 -10170623.343
            9858943.815 -3609149.715 -15093024.964 3705083.766 14130653.745
            2465850.998 22277888.659 -3091528.426 -17334755.19 -22825089.926
            -14781358.202 18760540.32 17436418.124 20781011.516 22997319.871
            -6159785.869 -20552221.843 18415405.479 6125006.067 10453218.607
            -11781188.945 11948258.606 28749085.295 -28794746.349 -18022868.893))
          (y
           (-15811938.317 -17437745.322 -19812571.818 -25376373.77 14682941.078
            5420610.526 -9696356.974 -25753204.418 -19323213.397 8420435.422
            11769800.075 20487024.787 20598108.199 -10594414.585 26392450.136
            -9141892.005 -5901200.245 7977222.353 4192056.668 6811399.976
            -5332652.316 17350454.105 18143371.404 -2526812.416 -19116325.28
            -4076244.364 20484401.868 -1224587.719 -9341619.345 576257.869
            -22941786.481 -23321587.203 -10972153.879 7275996.601 23379270.296
            10624346.256 10717846.922 9592689.283 -1008249.169 -13413653.016
            -8962608.789 864495.001 -11666541.386 394126.875 10395720.09
            17433531.027 12607050.16 -843803.062 -18332397.276 12756823.166
            22322859.253 -6597029.293 3009950.271 -22735757.202 -18056425.465
            -27783637.06 28700493.008 22283917.315 -28714095.32 -16703682.263
            -16772317.46 -14281457.763 16768496.833 -8758817.246 13981175.55
            -24065822.899 -12256482.05 -1300614.344 21070416.872 6332972.546
            -23479710.635 -21301646.348 9610971.205 23419346.688 17827133.917
            12176461.348 -11921308.005 -2779249.361 3060808.833 -9719913.342))
          (z
           (-21315322.068 -15980673.653 -16722772.29 7595174.047 21458769.348
            -4953277.098 20145775.415 2142855.978 15993062.836 -11946530.255
            6138325.065 -16530930.659 9439943.943 -9345333.97 1401478.937
            21458658.355 -21430168.087 19154567.698 -20700538.869 19847902.246
            -15330699.095 1641538.08 -19105254.818 17703198.357 13016696.188
            -7987556.117 15652649.397 12744588.662 973616.051 -21599030.494
            -10947412.204 7124161.837 20170344.566 22606839.05 -7066850.713
            -20160294.688 -22838973.002 -2507965.585 16009207.296 16600600.859
            708120.176 -15950323.955 -22576903.894 -20394681.932 -9764424.029
            11607711.995 22158053.265 19578458.623 -11877476.381 -18971579.81
            12218290.668 -17413017.945 23465276.56 -11733995.77 21148261.807
            -2677950.548 -6221045.496 12302852.105 6224299.553 -19943841.367
            -24275197.002 -13245478.692 24198578.417 -21912651.338 12657262.362
            8891012.984 17874152.758 -23871256.162 526045.36 17511243.179
            16910386.479 -57354.619 21086011.819 -17041223.868 -21188348.582
            -24285149.222 24319470.941 6470719.577 -6141910.73 -21381131.683))
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
          (B0F
           11111111111111111111111111111111111111111111111111111111111111101101111111111111))
         (pos_stddev (Empty 80)) (clock_stddev (Empty 80)) (maneuver (Empty 80))
         (clock_event (Empty 80)) (velocity (Empty 80))
         (clock_velocity (Empty 80)) (velocity_stddev (Empty 80))
         (clock_velocity_stddev (Empty 80)))))))
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
       (epoch_count 0) (epochs ())
       (presence
        ((pos (Full 51)) (clock (Full 51)) (pos_stddev (Full 51))
         (clock_stddev (Full 51)) (maneuver (Empty 51)) (clock_event (Empty 51))
         (velocity (Full 51)) (clock_velocity (Full 51))
         (velocity_stddev (Full 51)) (clock_velocity_stddev (Full 51)))))))
    |}]

let%expect_test "Processed file" =
  expect_test_f (module Processed_file)
{|#dP2025  4 26  6  0  0.00000000     576 ORBIT IGS20 HLM IGS 
## 2363 540000.00000000   300.00000000 60791 0.2500000000000
+   81   E02E03E04E05E06E07E08E09E10E11E12E13E14E15E16E18E19
+        E21E23E24E25E26E27E29E30E31E33E34E36G01G02G03G04G05
+        G06G07G08G09G10G11G12G13G14G15G16G17G18G19G20G22G23
+        G24G25G26G27G28G29G30G31G32R02R03R04R05R07R08R09R11
+        R12R14R15R16R17R18R19R20R21R22R24R26R27  0  0  0  0
++         5  5  4  5  4  4  4  5  4  7  7  5  6  5  3  6  4
++         4  5  4  5  6  6  3  5  5  5  5  5  3  3  3  3  3
++         3  4  3  3  3  3  4  3  4  3  3  4  3  4  3  3  3
++         3  5  4  3  3  3  3  3  3  4  4  4  4  4  4  5  4
++         5  5  5  5  3  4  4  5  3  2  4  5  0  0  0  0  0
%c M  cc GPS ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc
%c cc cc ccc ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc
%f  1.2500000  1.025000000  0.00000000000  0.000000000000000
%f  0.0000000  0.000000000  0.00000000000  0.000000000000000
%i    0    0    0    0      0      0      0      0         0
%i    0    0    0    0      0      0      0      0         0
/* STRICTLY EXPERIMENTAL MULTI-GNSS COMBINATION
/* ULTRA RAPID ORBIT COMBINATION FROM WEIGHTED AVERAGE OF:
/* COD EMR ESA GFZ GRG WHU
/* REFERENCED TO GPS CLOCK AND TO WEIGHTED MEAN POLE:
/* PCV:IGS20_2290 OL/AL:FES2014b NONE     Y  ORB:CMB CLK:COD
*  2025  4 26  6  0  0.00000000
PE02  -4213.196867 -23968.456807 -16850.297415    214.949828 16  9 14
PE03 -10020.938512 -14695.921440  23657.758768   -187.854902 13 13 11
PE04  26675.982718   3236.666609 -12413.447403   -217.250645 10 18 15
PE05  12161.900248 -17269.056135  20750.780907   4847.008789  9 12 15
PE06  29184.491225  -3557.184694  -3450.943482   -110.465904  7 16 16
PE07 -27446.855749   9609.606663  -5459.771652   -181.352968 11 17 10
PE08 -26348.724656  -3622.432180  12976.496162   -455.385635  9 15  8
PE09  27499.697674  -9534.639616   5460.222148   -865.147135 10 15 15
PE10  21611.304314   5577.992738 -19449.229552   -705.550647  8 10 11
PE11  16535.381406  -4792.858187 -24088.741199  -2023.485404 23 12 19
PE12  22963.479160  13809.865210 -12555.541875  -1396.187922 19 20 23
PE13 -16811.893944   4232.909263  23995.306646    -33.513175 18 20 17
PE14 -22183.405342  -3172.092843 -10214.920942 999999.999999 17 21  9
PE15 -22931.052613 -14419.859790  11953.913255   -170.118537  8 11 19
PE16  14860.007637 -23733.217828   9623.217949    -34.484849  7 11 12
PE18  31442.591473  -6156.344772   1900.727643 999999.999999 15 20 18
PE19   8319.924621  15143.120626 -24020.882102   1690.010420  8 18 16
PE21  -9584.305975  28002.999777   -264.591634   -660.795680 16 21  9
PE23   4446.861306  23843.452019  16951.028172      1.968305 11 13 10
PE24  18114.583142 -15554.011809  17467.783157  -1400.155844  7 16 17
PE25   9888.884017 -27889.654694    732.701044     16.592896 13 18 14
PE26   -745.774650  20330.911278  21499.525619    -58.261549 21 25 18
PE27 -18074.597420  15494.836719 -17595.506116   -644.493255 14 19 20
PE29 -11833.673781  17416.230661 -20801.008723    -90.710730 10  8  6
PE30 -15941.252797  -5723.072228 -24288.702853  -1489.848427 13 20 17
PE31  15789.129952   5986.625307  24313.959117    -64.537449 16 18 15
PE33  15216.989285  24330.600761   7250.014126     15.280958 12 22 19
PE34 -15495.301659 -24252.691234  -6923.455127   -177.341104 14 22 16
PE36   1021.438090 -20019.640634 -21785.692244   -385.840057 12 10 14
PG01 -21135.760084 -13612.022821   8607.623825    237.147577  6  8  5
PG02 -20390.106859 -17215.653233  -1961.523751   -185.140402  7  8  8
PG03 -12379.024018  -9222.123128  21456.733939    688.054410  8  9  7
PG04 -23405.519880  -2345.782119  12506.734470    558.762682  7 11  8
PG05   7728.694150  19786.283006 -16080.781727   -208.251468  8  6  4
PG06  -2278.161284  16551.871984  20676.030160   -325.363831  7  6  4
PG07 -19287.103062   -231.217534 -17880.691174    -23.395734  9  9  8
PG08 -17033.661599  -8250.182049 -18961.921929    512.791003  8 11  8
PG09 -25520.834549   6923.640903   2746.956724    641.876482  9  8 10
PG10  10359.617607 -22884.457352  -7856.974881   -403.062498  9 11  7
PG11   7981.925553  21331.431966  13752.118488   -723.077538  6 10  8
PG12  12342.961430  10073.266753  20969.773628   -581.749108  7  8  8
PG13   6680.932970  13958.753783 -21850.734633    700.710973  6  4  6
PG14 -12635.752943  20717.974260 -10427.181231    636.311789  8  8  6
PG15  17920.729200   6505.442303 -19065.371640    289.498198  7 11 10
PG16  -1337.397581 -23708.966024 -11789.991519      1.535209 11  9  6
PG17 -20634.590534  13181.263798  10954.544405    310.357318  9  6  7
PG18  17807.240044  -1612.715409 -19688.652274   -632.759646  8  5  6
PG19 -12169.531684  15183.954598  17730.852936    622.516462  9  7  6
PG20   1831.591997  26279.891165  -3739.607527    364.824961  9  9  6
PG22  -9828.944972  24631.935028  -2943.484162   -102.725132  7  9  8
PG23  14904.443679 -10805.113712 -19006.002065    466.217570  8 10  5
PG24  22171.593748  14878.509687   1509.674799   -415.008364  4 10  8
PG25  15753.357260  -3212.489681  20738.083761 999999.999999 14 17 16
PG26   2354.360747 -26222.897500  -1149.624358   -206.166322 11 11  7
PG27  -4128.609209 -14901.945640 -21911.636662    -27.060006  9  9  1
PG28   4914.501532 -15241.567525  21183.183737   -602.799461 10  6  7
PG29  26121.984195  -3850.204773   3439.524281   -513.702792  4  8  7
PG30 -11125.184114  11068.824053 -21260.152252   -160.489029  9  7  9
PG31  -3612.256391 -19856.623699  16832.151787   -209.113675 10  6  8
PG32  16159.840583 -15718.117572  14130.043655   -467.933501  8 10  2
PR02 -20869.244540   8761.257119  11874.651602    -33.722160  6 10  9
PR03 -17837.744805  17222.815578  -6052.249390    121.277757  9  9  9
PR04  -5454.404696  15326.644430 -19615.540825    288.248688 11 11  8
PR05   9993.680901   5602.886509 -22772.033740    148.871863  8 10  5
PR07  17690.233975 -17233.954515   6335.198072    104.816202  8 10  7
PR08   4872.078420 -15344.037592  19841.158291     -5.260584 12 10 10
PR09  11331.692479   -904.956403  22854.108920    251.384894  8 10  6
PR11  -1896.064012 -25212.989328   3468.199888     21.197432  7  8  8
PR12  -9825.482978 -17888.885833 -15270.073920     53.530609  8 10 12
PR14  -5819.984450  17883.490709 -17207.960587     28.208872 11  6  6
PR15   2681.118650  25311.647386  -1572.494126     69.065503 10 13  8
PR16   9758.885140  17905.426136  15309.270964     35.312257 10  9  9
PR17 -10116.333561   6925.829826  22364.450258    186.261885 10  8  6
PR18   6422.722432  13189.169941  20862.150791    198.672404  8 10  6
PR19  18441.916889  14084.271580  10588.155562    -49.070851 14 14 12
PR20  22448.090646   5396.078285 -10796.129601   -129.282915  9 10 14
PR21  11816.651134  -5763.185639 -21869.187416   -196.603104  9  5  2
PR22  -7462.405997 -13934.584095 -20001.924667    -65.535524  8 10  8
PR24 -22654.965594  -3724.495206  11111.129704   -102.039403  6 11 11
PR26   6920.796330 -16217.399292  18468.945725   -132.012181 17 15  9
PR27  20445.316807  -7976.454175 -12974.368169   -169.291744
EOF
|};
  [%expect {|
    ((unconsumed ((buf "") (off 7044) (len 0)))
     (result
      ((raw_header
        ((version
          ((version D) (pos_or_vel Pos) (year 2025) (month 4) (day_of_month 26)
           (hour 6) (minute 0) (second 0) (number_of_epochs 576)
           (data_used ORBIT) (coordinate_system IGS20) (orbit_used HLM)
           (agency "IGS ")))
         (time_info
          ((gps_week 2363) (seconds_of_week 540000) (epoch_interval 300)
           (modified_julian_day 60791) (fractional_day 0.25)))
         (space_vehicles
          (((number (81))
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
              ((kind Galileo) (prn 34)) ((kind Galileo) (prn 36))
              ((kind Gps) (prn 1)) ((kind Gps) (prn 2)) ((kind Gps) (prn 3))
              ((kind Gps) (prn 4)) ((kind Gps) (prn 5)))))
           ((number ())
            (ids
             (((kind Gps) (prn 6)) ((kind Gps) (prn 7)) ((kind Gps) (prn 8))
              ((kind Gps) (prn 9)) ((kind Gps) (prn 10)) ((kind Gps) (prn 11))
              ((kind Gps) (prn 12)) ((kind Gps) (prn 13)) ((kind Gps) (prn 14))
              ((kind Gps) (prn 15)) ((kind Gps) (prn 16)) ((kind Gps) (prn 17))
              ((kind Gps) (prn 18)) ((kind Gps) (prn 19)) ((kind Gps) (prn 20))
              ((kind Gps) (prn 22)) ((kind Gps) (prn 23)))))
           ((number ())
            (ids
             (((kind Gps) (prn 24)) ((kind Gps) (prn 25)) ((kind Gps) (prn 26))
              ((kind Gps) (prn 27)) ((kind Gps) (prn 28)) ((kind Gps) (prn 29))
              ((kind Gps) (prn 30)) ((kind Gps) (prn 31)) ((kind Gps) (prn 32))
              ((kind Glonass) (prn 2)) ((kind Glonass) (prn 3))
              ((kind Glonass) (prn 4)) ((kind Glonass) (prn 5))
              ((kind Glonass) (prn 7)) ((kind Glonass) (prn 8))
              ((kind Glonass) (prn 9)) ((kind Glonass) (prn 11)))))
           ((number ())
            (ids
             (((kind Glonass) (prn 12)) ((kind Glonass) (prn 14))
              ((kind Glonass) (prn 15)) ((kind Glonass) (prn 16))
              ((kind Glonass) (prn 17)) ((kind Glonass) (prn 18))
              ((kind Glonass) (prn 19)) ((kind Glonass) (prn 20))
              ((kind Glonass) (prn 21)) ((kind Glonass) (prn 22))
              ((kind Glonass) (prn 24)) ((kind Glonass) (prn 26))
              ((kind Glonass) (prn 27)))))))
         (accuracy
          (((accuracy_exponent
             ((5) (5) (4) (5) (4) (4) (4) (5) (4) (7) (7) (5) (6) (5) (3)
              (6) (4))))
           ((accuracy_exponent
             ((4) (5) (4) (5) (6) (6) (3) (5) (5) (5) (5) (5) (3) (3) (3)
              (3) (3))))
           ((accuracy_exponent
             ((3) (4) (3) (3) (3) (3) (4) (3) (4) (3) (3) (4) (3) (4) (3)
              (3) (3))))
           ((accuracy_exponent
             ((3) (5) (4) (3) (3) (3) (3) (3) (3) (4) (4) (4) (4) (4) (4)
              (5) (4))))
           ((accuracy_exponent
             ((5) (5) (5) (5) (3) (4) (4) (5) (3) (2) (4) (5) () () () () ())))))
         (type_and_time ((file_type Mixed) (time_system Gps)))
         (base ((position_velocity 1.25) (clock 1.025)))
         (comments (() () () () ()))))
       (space_vehicles
        (((kind Galileo) (prn 2)) ((kind Galileo) (prn 3))
         ((kind Galileo) (prn 4)) ((kind Galileo) (prn 5))
         ((kind Galileo) (prn 6)) ((kind Galileo) (prn 7))
         ((kind Galileo) (prn 8)) ((kind Galileo) (prn 9))
         ((kind Galileo) (prn 10)) ((kind Galileo) (prn 11))
         ((kind Galileo) (prn 12)) ((kind Galileo) (prn 13))
         ((kind Galileo) (prn 14)) ((kind Galileo) (prn 15))
         ((kind Galileo) (prn 16)) ((kind Galileo) (prn 18))
         ((kind Galileo) (prn 19)) ((kind Galileo) (prn 21))
         ((kind Galileo) (prn 23)) ((kind Galileo) (prn 24))
         ((kind Galileo) (prn 25)) ((kind Galileo) (prn 26))
         ((kind Galileo) (prn 27)) ((kind Galileo) (prn 29))
         ((kind Galileo) (prn 30)) ((kind Galileo) (prn 31))
         ((kind Galileo) (prn 33)) ((kind Galileo) (prn 34))
         ((kind Galileo) (prn 36)) ((kind Gps) (prn 1)) ((kind Gps) (prn 2))
         ((kind Gps) (prn 3)) ((kind Gps) (prn 4)) ((kind Gps) (prn 5))
         ((kind Gps) (prn 6)) ((kind Gps) (prn 7)) ((kind Gps) (prn 8))
         ((kind Gps) (prn 9)) ((kind Gps) (prn 10)) ((kind Gps) (prn 11))
         ((kind Gps) (prn 12)) ((kind Gps) (prn 13)) ((kind Gps) (prn 14))
         ((kind Gps) (prn 15)) ((kind Gps) (prn 16)) ((kind Gps) (prn 17))
         ((kind Gps) (prn 18)) ((kind Gps) (prn 19)) ((kind Gps) (prn 20))
         ((kind Gps) (prn 22)) ((kind Gps) (prn 23)) ((kind Gps) (prn 24))
         ((kind Gps) (prn 25)) ((kind Gps) (prn 26)) ((kind Gps) (prn 27))
         ((kind Gps) (prn 28)) ((kind Gps) (prn 29)) ((kind Gps) (prn 30))
         ((kind Gps) (prn 31)) ((kind Gps) (prn 32)) ((kind Glonass) (prn 2))
         ((kind Glonass) (prn 3)) ((kind Glonass) (prn 4))
         ((kind Glonass) (prn 5)) ((kind Glonass) (prn 7))
         ((kind Glonass) (prn 8)) ((kind Glonass) (prn 9))
         ((kind Glonass) (prn 11)) ((kind Glonass) (prn 12))
         ((kind Glonass) (prn 14)) ((kind Glonass) (prn 15))
         ((kind Glonass) (prn 16)) ((kind Glonass) (prn 17))
         ((kind Glonass) (prn 18)) ((kind Glonass) (prn 19))
         ((kind Glonass) (prn 20)) ((kind Glonass) (prn 21))
         ((kind Glonass) (prn 22)) ((kind Glonass) (prn 24))
         ((kind Glonass) (prn 26)) ((kind Glonass) (prn 27))))
       (accuracy
        (0.032 0.032 0.016 0.032 0.016 0.016 0.016 0.032 0.016 0.128 0.128 0.032
         0.064 0.032 0.008 0.064 0.016 0.016 0.032 0.016 0.032 0.064 0.064 0.008
         0.032 0.032 0.032 0.032 0.032 0.008 0.008 0.008 0.008 0.008 0.008 0.016
         0.008 0.008 0.008 0.008 0.016 0.008 0.016 0.008 0.008 0.016 0.008 0.016
         0.008 0.008 0.008 0.008 0.032 0.016 0.008 0.008 0.008 0.008 0.008 0.008
         0.016 0.016 0.016 0.016 0.016 0.016 0.032 0.016 0.032 0.032 0.032 0.032
         0.008 0.016 0.016 0.032 0.008 0.004 0.016 0.032 None))
       (epoch_count 1)
       (epochs
        (((metadata
           ((year 2025) (month 4) (day_of_month 26) (hour 6) (minute 0)
            (second 0)))
          (x
           (-4213196.867 -10020938.512 26675982.718 12161900.248 29184491.225
            -27446855.749 -26348724.656 27499697.674 21611304.314 16535381.406
            22963479.16 -16811893.944 -22183405.342 -22931052.613 14860007.637
            31442591.473 8319924.621 -9584305.975 4446861.306 18114583.142
            9888884.017 -745774.65 -18074597.42 -11833673.781 -15941252.797
            15789129.952 15216989.285 -15495301.659 1021438.09 -21135760.084
            -20390106.859 -12379024.018 -23405519.88 7728694.15 -2278161.284
            -19287103.062 -17033661.599 -25520834.549 10359617.607 7981925.553
            12342961.43 6680932.97 -12635752.943 17920729.2 -1337397.581
            -20634590.534 17807240.044 -12169531.684 1831591.997 -9828944.972
            14904443.679 22171593.748 15753357.26 2354360.747 -4128609.209
            4914501.532 26121984.195 -11125184.114 -3612256.391 16159840.583
            -20869244.54 -17837744.805 -5454404.696 9993680.901 17690233.975
            4872078.42 11331692.479 -1896064.012 -9825482.978 -5819984.45
            2681118.65 9758885.14 -10116333.561 6422722.432 18441916.889
            22448090.646 11816651.134 -7462405.997 -22654965.594 6920796.33
            20445316.807))
          (y
           (-23968456.807 -14695921.44 3236666.609 -17269056.135 -3557184.694
            9609606.663 -3622432.18 -9534639.616 5577992.738 -4792858.187
            13809865.21 4232909.263 -3172092.843 -14419859.79 -23733217.828
            -6156344.772 15143120.626 28002999.777 23843452.019 -15554011.809
            -27889654.694 20330911.278 15494836.719 17416230.661 -5723072.228
            5986625.307 24330600.761 -24252691.234 -20019640.634 -13612022.821
            -17215653.233 -9222123.128 -2345782.119 19786283.006 16551871.984
            -231217.534 -8250182.049 6923640.903 -22884457.352 21331431.966
            10073266.753 13958753.783 20717974.26 6505442.303 -23708966.024
            13181263.798 -1612715.409 15183954.598 26279891.165 24631935.028
            -10805113.712 14878509.687 -3212489.681 -26222897.5 -14901945.64
            -15241567.525 -3850204.773 11068824.053 -19856623.699 -15718117.572
            8761257.119 17222815.578 15326644.43 5602886.509 -17233954.515
            -15344037.592 -904956.403 -25212989.328 -17888885.833 17883490.709
            25311647.386 17905426.136 6925829.826 13189169.941 14084271.58
            5396078.285 -5763185.639 -13934584.095 -3724495.206 -16217399.292
            -7976454.175))
          (z
           (-16850297.415 23657758.768 -12413447.403 20750780.907 -3450943.482
            -5459771.652 12976496.162 5460222.148 -19449229.552 -24088741.199
            -12555541.875 23995306.646 -10214920.942 11953913.255 9623217.949
            1900727.643 -24020882.102 -264591.634 16951028.172 17467783.157
            732701.044 21499525.619 -17595506.116 -20801008.723 -24288702.853
            24313959.117 7250014.126 -6923455.127 -21785692.244 8607623.825
            -1961523.751 21456733.939 12506734.47 -16080781.727 20676030.16
            -17880691.174 -18961921.929 2746956.724 -7856974.881 13752118.488
            20969773.628 -21850734.633 -10427181.231 -19065371.64 -11789991.519
            10954544.405 -19688652.274 17730852.936 -3739607.527 -2943484.162
            -19006002.065 1509674.799 20738083.761 -1149624.358 -21911636.662
            21183183.737 3439524.281 -21260152.252 16832151.787 14130043.655
            11874651.602 -6052249.39 -19615540.825 -22772033.74 6335198.072
            19841158.291 22854108.92 3468199.888 -15270073.92 -17207960.587
            -1572494.126 15309270.964 22364450.258 20862150.791 10588155.562
            -10796129.601 -21869187.416 -20001924.667 11111129.704 18468945.725
            -12974368.169))
          (clock
           (0.000214949828 -0.000187854902 -0.000217250645 0.004847008789
            -0.000110465904 -0.000181352968 -0.000455385635 -0.000865147135
            -0.000705550647 -0.002023485404 -0.001396187922 -3.3513175E-05 None
            -0.000170118537 -3.4484849E-05 None 0.00169001042 -0.00066079568
            1.968305E-06 -0.001400155844 1.6592896E-05 -5.8261549E-05
            -0.000644493255 -9.071073E-05 -0.001489848427 -6.4537449E-05
            1.5280958E-05 -0.000177341104 -0.000385840057 0.000237147577
            -0.000185140402 0.00068805441 0.000558762682 -0.000208251468
            -0.000325363831 -2.3395734E-05 0.000512791003 0.000641876482
            -0.000403062498 -0.000723077538 -0.000581749108 0.000700710973
            0.000636311789 0.000289498198 1.535209E-06 0.000310357318
            -0.000632759646 0.000622516462 0.000364824961 -0.000102725132
            0.00046621757 -0.000415008364 None -0.000206166322 -2.7060006E-05
            -0.000602799461 -0.000513702792 -0.000160489029 -0.000209113675
            -0.000467933501 -3.372216E-05 0.000121277757 0.000288248688
            0.000148871863 0.000104816202 -5.260584E-06 0.000251384894
            2.1197432E-05 5.3530609E-05 2.8208872E-05 6.9065503E-05 3.5312257E-05
            0.000186261885 0.000198672404 -4.9070851E-05 -0.000129282915
            -0.000196603104 -6.5535524E-05 -0.000102039403 -0.000132012181
            -0.000169291744))
          (x_stddev
           (0.035527136788005009 0.018189894035458565 0.0093132257461547852
            0.0074505805969238281 0.00476837158203125 0.011641532182693481
            0.0074505805969238281 0.0093132257461547852 0.0059604644775390625
            0.16940658945086007 0.069388939039072284 0.055511151231257827
            0.044408920985006262 0.0059604644775390625 0.00476837158203125
            0.028421709430404007 0.0059604644775390625 0.035527136788005009
            0.011641532182693481 0.00476837158203125 0.018189894035458565
            0.10842021724855044 0.022737367544323206 0.0093132257461547852
            0.018189894035458565 0.035527136788005009 0.014551915228366852
            0.022737367544323206 0.014551915228366852 0.003814697265625
            0.00476837158203125 0.0059604644775390625 0.00476837158203125
            0.0059604644775390625 0.00476837158203125 0.0074505805969238281
            0.0059604644775390625 0.0074505805969238281 0.0074505805969238281
            0.003814697265625 0.00476837158203125 0.003814697265625
            0.0059604644775390625 0.00476837158203125 0.011641532182693481
            0.0074505805969238281 0.0059604644775390625 0.0074505805969238281
            0.0074505805969238281 0.00476837158203125 0.0059604644775390625
            0.00244140625 0.022737367544323206 0.011641532182693481
            0.0074505805969238281 0.0093132257461547852 0.00244140625
            0.0074505805969238281 0.0093132257461547852 0.0059604644775390625
            0.003814697265625 0.0074505805969238281 0.011641532182693481
            0.0059604644775390625 0.0059604644775390625 0.014551915228366852
            0.0059604644775390625 0.00476837158203125 0.0059604644775390625
            0.011641532182693481 0.0093132257461547852 0.0093132257461547852
            0.0093132257461547852 0.0059604644775390625 0.022737367544323206
            0.0074505805969238281 0.0074505805969238281 0.0059604644775390625
            0.003814697265625 0.044408920985006262 None))
          (y_stddev
           (0.0074505805969238281 0.018189894035458565 0.055511151231257827
            0.014551915228366852 0.035527136788005009 0.044408920985006262
            0.028421709430404007 0.028421709430404007 0.0093132257461547852
            0.014551915228366852 0.086736173798840355 0.086736173798840355
            0.10842021724855044 0.011641532182693481 0.011641532182693481
            0.086736173798840355 0.055511151231257827 0.10842021724855044
            0.018189894035458565 0.035527136788005009 0.055511151231257827
            0.26469779601696886 0.069388939039072284 0.0059604644775390625
            0.086736173798840355 0.055511151231257827 0.13552527156068805
            0.13552527156068805 0.0093132257461547852 0.0059604644775390625
            0.0059604644775390625 0.0074505805969238281 0.011641532182693481
            0.003814697265625 0.003814697265625 0.0074505805969238281
            0.011641532182693481 0.0059604644775390625 0.011641532182693481
            0.0093132257461547852 0.0059604644775390625 0.00244140625
            0.0059604644775390625 0.011641532182693481 0.0074505805969238281
            0.003814697265625 0.0030517578125 0.00476837158203125
            0.0074505805969238281 0.0074505805969238281 0.0093132257461547852
            0.0093132257461547852 0.044408920985006262 0.011641532182693481
            0.0074505805969238281 0.003814697265625 0.0059604644775390625
            0.00476837158203125 0.003814697265625 0.0093132257461547852
            0.0093132257461547852 0.0074505805969238281 0.011641532182693481
            0.0093132257461547852 0.0093132257461547852 0.0093132257461547852
            0.0093132257461547852 0.0059604644775390625 0.0093132257461547852
            0.003814697265625 0.018189894035458565 0.0074505805969238281
            0.0059604644775390625 0.0093132257461547852 0.022737367544323206
            0.0093132257461547852 0.0030517578125 0.0093132257461547852
            0.011641532182693481 0.028421709430404007 None))
          (z_stddev
           (0.022737367544323206 0.011641532182693481 0.028421709430404007
            0.028421709430404007 0.035527136788005009 0.0093132257461547852
            0.0059604644775390625 0.028421709430404007 0.011641532182693481
            0.069388939039072284 0.16940658945086007 0.044408920985006262
            0.0074505805969238281 0.069388939039072284 0.014551915228366852
            0.055511151231257827 0.035527136788005009 0.0074505805969238281
            0.0093132257461547852 0.044408920985006262 0.022737367544323206
            0.055511151231257827 0.086736173798840355 0.003814697265625
            0.044408920985006262 0.028421709430404007 0.069388939039072284
            0.035527136788005009 0.022737367544323206 0.0030517578125
            0.0059604644775390625 0.00476837158203125 0.0059604644775390625
            0.00244140625 0.00244140625 0.0059604644775390625
            0.0059604644775390625 0.0093132257461547852 0.00476837158203125
            0.0059604644775390625 0.0059604644775390625 0.003814697265625
            0.003814697265625 0.0093132257461547852 0.003814697265625
            0.00476837158203125 0.003814697265625 0.003814697265625
            0.003814697265625 0.0059604644775390625 0.0030517578125
            0.0059604644775390625 0.035527136788005009 0.00476837158203125
            0.00125 0.00476837158203125 0.00476837158203125 0.0074505805969238281
            0.0059604644775390625 0.0015625 0.0074505805969238281
            0.0074505805969238281 0.0059604644775390625 0.0030517578125
            0.00476837158203125 0.0093132257461547852 0.003814697265625
            0.0059604644775390625 0.014551915228366852 0.003814697265625
            0.0059604644775390625 0.0074505805969238281 0.003814697265625
            0.003814697265625 0.014551915228366852 0.022737367544323206 0.0015625
            0.0059604644775390625 0.011641532182693481 0.0074505805969238281
            None))
          (clock_stddev (None 81)) (maneuver (Empty 81)) (orbit_pred (Empty 81))
          (clock_event (Empty 81)) (clock_pred (Empty 81)) (velocity ()))))
       (presence
        ((pos (Full 81))
         (clock
          (B0F
           111111111111011011111111111111111111111111111111111101111111111111111111111111111))
         (pos_stddev
          (B0F
           111111111111111111111111111111111111111111111111111111111111111111111111111111110))
         (clock_stddev (Empty 81)) (maneuver (Empty 81)) (clock_event (Empty 81))
         (velocity (Empty 81)) (clock_velocity (Empty 81))
         (velocity_stddev (Empty 81)) (clock_velocity_stddev (Empty 81)))))))
    |}]
