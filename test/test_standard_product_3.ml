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
