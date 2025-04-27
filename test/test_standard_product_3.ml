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
            (maneuver false) (orbit_pred false)))))))))
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
               (maneuver false) (orbit_pred false))))))))))))
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
            2.3732149737361174E-08))))))))
    |}]
