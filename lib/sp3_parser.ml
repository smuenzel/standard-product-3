open Core
open Angstrom

(* Reference: https://files.igs.org/pub/data/format/sp3d.pdf *)

module Version = struct
  type t =
    | C
    | D
  [@@deriving sexp]

  let parse =
    let* c = any_char in
    match c with
    | 'c' -> return C
    | 'd' -> return D
    | _ -> fail "not a version"
end

module Pos_or_vel = struct
  type t =
    | Pos
    | Vel
  [@@deriving sexp]

  let parse =
    let* c = any_char in
    match c with
    | 'P' -> return Pos
    | 'V' -> return Vel
    | _ -> fail "not a pos or vel"
end

module Char = struct
  include Char

  let is_blank = function
    | ' ' -> true
    | _ -> false

  let is_blank_or_digit c = is_blank c || is_digit c

end

module Fortran = struct
  let blank_to_zero s =
    if String.is_empty s then "0"
    else String.map s ~f:(function | ' ' -> '0' | c -> c)
    
  let f_empty =
    let+ () = end_of_input in
    Bigdecimal.zero

  let f_sign_only_exponent =
    let* sign = satisfy (function | '+' | '-' -> true | _ -> false) in
    let+ digits = take_while1 Char.is_digit in
    let i = Int.of_string digits in
    match sign with
    | '-' -> -i
    | _ -> i

  let f_exponent =
    let* _ = satisfy (function | 'E' | 'D' -> true | _ -> false) in
    let* () = skip_while Char.is_blank in
    let* sign = option '+' (satisfy (function | '+' | '-' -> true | _ -> false)) in
    let+ digits = take_while1 Char.is_digit in
    let i = Int.of_string digits in
    match sign with
    | '-' -> -i
    | _ -> i

  let f_only_exponent =
    let+ _ = f_exponent in
    Bigdecimal.zero

  let f_editing_number ~w =
    let* sign = option '+' (satisfy (function | '+' | '-' -> true | _ -> false)) in
    let* digits_or_blank = take_while1 Char.is_blank_or_digit in
    let* decimal = option false (let+ _ = char '.' in true) in
    let* follow_digits_raw = take_while Char.is_blank_or_digit in
    let+ exponent = option 0 (choice [ f_sign_only_exponent; f_exponent ]) in
    let digits = blank_to_zero digits_or_blank |> Int.of_string in 
    let follow_digits = blank_to_zero follow_digits_raw in
    let result =
      if decimal
      then begin
        let follow_int = Int.of_string follow_digits in
        let decimal =
          Bigdecimal.of_int follow_int
          |> Bigdecimal.scale_by ~power_of_ten:(~- (String.length follow_digits))
        in
        Bigdecimal.(+)
          (Bigdecimal.of_int digits)
          decimal
      end else begin
        assert (String.is_empty follow_digits_raw);
        Bigdecimal.of_int digits
        |> Bigdecimal.scale_by ~power_of_ten:(~- w)
      end
    in
    Bigdecimal.scale_by result ~power_of_ten:exponent
    |> match sign with
    | '-' -> Bigdecimal.neg
    | _ -> Fn.id

  let f_editing' ~w =
    let* () = skip_while Char.is_blank in
    choice
      [ f_empty
      ; f_only_exponent
      ; f_editing_number ~w
      ]

  let f_editing ~w ~d =
    assert (d <= w);
    (* The field has at least [w] characters, this is a little difficult to represent
       in Angstrom, so we create a subparser *)
    let* s = take w in
    match
      parse_string ~consume:All
        (f_editing' ~w)
        s
    with
    | Error s -> fail (Printf.sprintf "f_editing: %s" s)
    | Ok r -> return r

end

module F = struct

  let i n =
    let* v = take n in
    let s = String.strip v in
    try
      return (Int.of_string s)
    with
    | Failure _ -> fail "not an int"

  let i n =
    i n <?> "F.i"

  let f w d = Fortran.f_editing ~w ~d

  let a n = take n

  let a n =
    a n <?> "F.a"

  let blank = let+ _ = char ' ' <?> "F.blank" in ()

  let eol = let+ _ = char '\n' <?> "F.eol" in ()

  let line p =
    let* p = p in
    let+ () = eol in
    p

  let line p =
    line p <?> "F.line"

  let lines p =
    let+ l = sep_by1 eol p in
    l

  let lines p =
    lines p <?> "F.lines"
end

module Space_vehicle_id = struct
  module Kind = struct
    type t =
      | Gps
      | Glonass
      | Leo
      | Sbas
      | Galileo
      | Beidou
      | Irnss
      | Qzss
    [@@deriving sexp]

    let parse =
      let* c = any_char in
      match c with
      | 'G' -> return Gps
      | 'R' -> return Glonass
      | 'L' -> return Leo
      | 'S' -> return Sbas
      | 'E' -> return Galileo
      | 'C' -> return Beidou
      | 'I' -> return Irnss
      | 'J' -> return Qzss
      | _ -> fail "not a sv"

  end

  type t =
    { kind : Kind.t
    ; prn: int
    } [@@deriving sexp]

  let parse =
    let* kind = Kind.parse in
    let* prn_raw = F.i 2 in
    let prn =
      match kind with
      | Qzss -> prn_raw + 192
      | _ -> prn_raw
    in
    return { kind; prn }

  let parse_opt =
    choice
      [ string "  0" *> return None
      ; parse >>| Option.some
      ]
end

module File_type = struct
  type t =
    | Mixed
    | Only of Space_vehicle_id.Kind.t
  [@@deriving sexp]

  let parse = 
    choice
      [ string "M " *> return Mixed
      ; let* kind = Space_vehicle_id.Kind.parse in
        let+ _ = F.blank in
        Only kind
      ]
end

module Time_system = struct 
  type t =
    | Gps
    | Glonass
    | Galileo
    | Beidou
    | Tai
    | Utc
    | Irnss
    | Qzss
  [@@deriving sexp]

  let parse =
    let* s = take 3 in
    match s with
    | "GPS" -> return Gps
    | "GLO" -> return Glonass
    | "GAL" -> return Galileo
    | "BDT" -> return Beidou
    | "TAI" -> return Tai
    | "UTC" -> return Utc
    | "IRN" -> return Irnss
    | "QZS" -> return Qzss
    | _ -> fail "not a time system"
end

module Line = struct
  module Version = struct
    type t =
      { version : Version.t
      ; pos_or_vel : Pos_or_vel.t
      ; year : int
      ; month : int
      ; day_of_month : int
      ; hour : int
      ; minute : int
      ; second : Bigdecimal.t
      ; number_of_epochs : int
      ; data_used : string
      ; coordinate_system : string
      ; orbit_used : string
      ; agency : string
      } [@@deriving sexp]

    let parse : t Angstrom.t =
      let* _ = char '#' in
      let* version = Version.parse in
      let* pos_or_vel = Pos_or_vel.parse in
      let* year = F.i 4 in
      let* _ = F.blank in
      let* month = F.i 2 in
      let* _ = F.blank in
      let* day_of_month = F.i 2 in
      let* _ = F.blank in
      let* hour = F.i 2 in
      let* _ = F.blank in
      let* minute = F.i 2 in
      let* _ = F.blank in
      let* second = F.f 11 8 in
      let* _ = F.blank in
      let* number_of_epochs = F.i 7 in
      let* _ = F.blank in
      let* data_used = F.a 5 in
      let* _ = F.blank in
      let* coordinate_system = F.a 5 in
      let* _ = F.blank in
      let* orbit_used = F.a 3 in
      let* _ = F.blank in
      let+ agency = F.a 4 in
      {  version
      ; pos_or_vel
      ; year
      ; month
      ; day_of_month
      ; hour
      ; minute
      ; second
      ; number_of_epochs
      ; data_used
      ; coordinate_system
      ; orbit_used
      ; agency
      }

  end

  module Time_info = struct
    type t =
      { gps_week : int
      ; seconds_of_week : Bigdecimal.t
      ; epoch_interval : Bigdecimal.t
      ; modified_julian_day : int
      ; fractional_day : Bigdecimal.t
      } [@@deriving sexp]

    let parse =
      let* _ = char '#' in
      let* _ = char '#' in
      let* _ = F.blank in
      let* gps_week = F.i 4 in
      let* _ = F.blank in
      let* seconds_of_week = F.f 15 8 in
      let* _ = F.blank in
      let* epoch_interval = F.f 14 8 in
      let* _ = F.blank in
      let* modified_julian_day = F.i 5 in
      let* _ = F.blank in
      let+ fractional_day = F.f 15 13 in
      { gps_week; seconds_of_week; epoch_interval; modified_julian_day; fractional_day }
  end

  module Space_vehicles = struct
    type t =
      { number : int option
      ; ids : Space_vehicle_id.t list
      } [@@deriving sexp]

    let parse =
      let* _ = char '+' in
      let* _ = char ' ' in
      let* _ = F.blank in
      let* number =
        choice
          [ (let+ _ = count 3 F.blank in None)
          ; (let+ n = F.i 3 in Some n)
          ]
      in
      let* _ = count 3 F.blank in
      let+ ids = count 17 Space_vehicle_id.parse_opt in
      let ids = List.filter_opt ids in
      { number; ids }

    let parse =
      parse <?> "space_vehicles"

  end

  module Accuracy = struct
    type t =
      { accuracy_exponent : int option list
      } [@@deriving sexp]

    let parse =
      let* _ = char '+' in
      let* _ = char '+' in
      let* _ = count 7 F.blank in
      let+ accuracy_exponent = count 17 (F.i 3) in
      let accuracy_exponent =
        List.map ~f:(function | 0 -> None | x -> Some x) accuracy_exponent
      in
      { accuracy_exponent }
  end

  module Type_and_time = struct
    type t =
      { file_type : File_type.t
      ; time_system : Time_system.t
      } [@@deriving sexp]

    let parse =
      let* _ = char '%' in
      let* _ = char 'c' in
      let* _ = F.blank in
      let* file_type = File_type.parse in
      let* _ = F.blank in
      let* _ = count 2 (char 'c') in
      let* _ = F.blank in
      let* time_system = Time_system.parse in
      let remainder =
        let* _ = F.blank in
        let* _ = count 3 (char 'c') in
        let* _ = F.blank in
        let* _ = count 4 (char 'c') in
        let* _ = F.blank in
        let* _ = count 4 (char 'c') in
        let* _ = F.blank in
        let* _ = count 4 (char 'c') in
        let* _ = F.blank in
        let* _ = count 4 (char 'c') in
        let* _ = F.blank in
        let* _ = count 5 (char 'c') in
        let* _ = F.blank in
        let* _ = count 5 (char 'c') in
        let* _ = F.blank in
        let* _ = count 5 (char 'c') in
        let* _ = F.blank in
        let+ _ = count 5 (char 'c') in
        ()
      in
      let remainder = remainder <?> "remainder" in
      let* () = remainder in
      let line2 = 
        let* _ = char '\n' in
        let* _ = char '%' in
        let* _ = char 'c' in
        let* _ = F.blank in
        let* _ = count 2 (char 'c') in
        let* _ = F.blank in
        let* _ = count 2 (char 'c') in
        let* _ = F.blank in
        let* _ = count 3 (char 'c') in
        let+ () = remainder in
        ()
      in
      let line2 = line2 <?> "line2" in
      let+ () = line2 in
      { file_type; time_system }

    let parse = parse <?> "type_and_time"
  end

  module Base = struct
    type t =
      { position_velocity : Bigdecimal.t
      ; clock : Bigdecimal.t
      } [@@deriving sexp]

    let parse =
      let* _ = char '%' in
      let* _ = char 'f' in
      let* _ = F.blank in
      let* position_velocity = F.f 10 7 in
      let* _ = F.blank in
      let* clock = F.f 12 9 in
      let* _ = F.blank in
      let* _ = F.f 14 11 in
      let* _ = F.blank in
      let* _ = F.f 18 15 in
      let* _ = char '\n' in
      let* _ = char '%' in
      let* _ = char 'f' in
      let* _ = F.blank in
      let* _ = F.f 10 7 in
      let* _ = F.blank in
      let* _ = F.f 12 9 in
      let* _ = F.blank in
      let* _ = F.f 14 11 in
      let* _ = F.blank in
      let+ _ = F.f 18 15 in
      { position_velocity; clock }

    let parse = parse <?> "base"
  end

  module Int_empty = struct
    type t = unit [@@deriving sexp]

    let parse = 
      let* _ = char '%' in
      let* _ = char 'i' in
      let* _ = F.blank in
      let* _ = F.i 4 in
      let* _ = F.blank in
      let* _ = F.i 4 in
      let* _ = F.blank in
      let* _ = F.i 4 in
      let* _ = F.blank in
      let* _ = F.i 4 in
      let* _ = F.blank in
      let* _ = F.i 6 in
      let* _ = F.blank in
      let* _ = F.i 6 in
      let* _ = F.blank in
      let* _ = F.i 6 in
      let* _ = F.blank in
      let* _ = F.i 6 in
      let* _ = F.blank in
      let+ _ = F.i 9 in
      ()

    let parse = parse <?> "int_empty"
  end

  module Comment = struct
    type t = unit [@@deriving sexp]

    let parse : t Angstrom.t =
      let* _ = char '/' in
      let* _ = char '*' in
      skip_while (function | '\n' -> false | _ -> true)

    let parse = parse <?> "comment"
  end

  module Epoch = struct
    type t =
      { year : int
      ; month : int
      ; day_of_month : int
      ; hour : int
      ; minute : int
      ; second : Bigdecimal.t
      } [@@deriving sexp]

    let parse =
      let* _ = char '*' in
      let* _ = char ' ' in
      let* _ = F.blank in
      let* year = F.i 4 in
      let* _ = F.blank in
      let* month = F.i 2 in
      let* _ = F.blank in
      let* day_of_month = F.i 2 in
      let* _ = F.blank in
      let* hour = F.i 2 in
      let* _ = F.blank in
      let* minute = F.i 2 in
      let* _ = F.blank in
      let+ second = F.f 11 8 in
      { year; month; day_of_month; hour; minute; second }

    let parse = parse <?> "epoch"
  end

  module Position_and_clock = struct
    module Data = struct
      type t =
        { x : Bigdecimal.t
        ; y : Bigdecimal.t
        ; z : Bigdecimal.t
        ; clock : Bigdecimal.t option
        ; x_stddev : int option
        ; y_stddev : int option
        ; z_stddev : int option
        ; clock_stddev : int option
        ; clock_event : bool
        ; clock_pred : bool
        ; maneuver : bool
        ; orbit_pred : bool
        } [@@deriving sexp]
    end

    type t =
      { space_vehicle_id : Space_vehicle_id.t
      ; data : Data.t option
      } [@@deriving sexp]

    let clock_is_none clock =
      Bignum.equal (Bignum.truncate (Bigdecimal.to_bignum clock)) (Bignum.of_int 999999)

    let parse_prefix = 
      let* _ = char 'P' in
      let* space_vehicle_id = Space_vehicle_id.parse in
      let* x = F.f 14 6 in
      let* y = F.f 14 6 in
      let* z = F.f 14 6 in
      let+ clock = F.f 14 6 in
      let clock = if clock_is_none clock then None else Some clock in
      space_vehicle_id, x, y, z, clock

    let parse_prefix = parse_prefix <?> "prefix"

    let parse_stdev =
      let* x_stddev = F.i 2 in
      let* _ = F.blank in
      let* y_stddev = F.i 2 in
      let* _ = F.blank in
      let* z_stddev = F.i 2 in
      let* _ = F.blank in
      let+ clock_stddev =
        choice
          [ F.i 3 >>| Option.some
          ; count 3 F.blank >>| fun _ -> None
          ]
          <?> "clock_stddev"
      in
      Some x_stddev, Some y_stddev, Some z_stddev, clock_stddev

    let parse_stdev = parse_stdev <?> "stdev"

    let partial_data
        ~x ~y ~z ~clock
      =
      let* () = skip_while Char.is_blank in
      let* n = peek_char in
      match n with
      | Some '\n' | None -> 
        Some
          { Data.x
          ; y
          ; z
          ; clock
          ; x_stddev = None
          ; y_stddev = None
          ; z_stddev = None
          ; clock_stddev = None
          ; clock_event = false
          ; clock_pred = false
          ; maneuver = false
          ; orbit_pred = false
          }
        |> return
      | _ -> fail "not partial"

    let full_data 
        ~x ~y ~z ~clock
      =
      let* _ = F.blank in
      let* x_stddev, y_stddev, z_stddev, clock_stddev = parse_stdev in
      let* _ = F.blank in
      let* clock_event =
        let* c = any_char in
        match c with
        | 'E' -> return true
        | ' ' -> return false
        | _ -> fail "not an event"
      in
      let* clock_pred =
        let* c = any_char in
        match c with
        | 'P' -> return true
        | ' ' -> return false
        | _ -> fail "not a prediction"
      in
      let* _ = F.blank in
      let* _ = F.blank in
      let* maneuver =
        let* c = any_char in
        match c with
        | 'M' -> return true
        | ' ' -> return false
        | _ -> fail "not a maneuver"
      in
      let+ orbit_pred =
        let* c = any_char in
        match c with
        | 'P' -> return true
        | ' ' -> return false
        | _ -> fail "not an orbit prediction"
      in
      Some
        { Data.x
        ; y
        ; z
        ; clock
        ; x_stddev
        ; y_stddev
        ; z_stddev
        ; clock_stddev
        ; clock_event
        ; clock_pred
        ; maneuver
        ; orbit_pred
        }

    let parse =
      let* space_vehicle_id, x, y, z, clock = parse_prefix in
      let* data =
        if Bigdecimal.is_zero x 
        && Bigdecimal.is_zero y
        && Bigdecimal.is_zero z
        && Option.is_none clock
        then begin
          let+ _ = take_while (function | '\n' -> false | _ -> true) in
          None
        end
        else begin
          choice
           [ full_data ~x ~y ~z ~clock
           ; partial_data ~x ~y ~z ~clock
           ]
        end
      in
      return
        { space_vehicle_id; data }

    let parse = parse <?> "Position_and_clock"
  end

end

module type Parseable = sig
  type t [@@deriving sexp]

  val parse : t Angstrom.t
end

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

module Epoch_block = struct
  type t =
    Line.Epoch.t * Line.Position_and_clock.t list
  [@@deriving sexp]

  let parse =
    let* epoch_header = F.line Line.Epoch.parse in
    let+ records = F.lines Line.Position_and_clock.parse in
    epoch_header, records

end

module Full_file = struct
  type t =
    { version : Line.Version.t
    ; time_info : Line.Time_info.t
    ; space_vehicles : Line.Space_vehicles.t list
    ; accuracy : Line.Accuracy.t list
    ; type_and_time : Line.Type_and_time.t
    ; base : Line.Base.t
    ; comments : Line.Comment.t list
    ; epoch_blocks : (Line.Epoch.t * Line.Position_and_clock.t list) list
    } [@@deriving sexp]

  let parse =
    let* version = F.line Line.Version.parse in
    let* time_info = F.line Line.Time_info.parse in
    let* space_vehicles = F.lines Line.Space_vehicles.parse in
    let* () = F.eol in
    let* accuracy = F.lines Line.Accuracy.parse in
    let* () = F.eol in
    let* type_and_time = F.line Line.Type_and_time.parse in
    let* base = F.line Line.Base.parse in
    let* _int_empty = F.line Line.Int_empty.parse in
    let* _int_empty = F.line Line.Int_empty.parse in
    let* comments = F.lines Line.Comment.parse in
    let* () = F.eol in
    let* epoch_blocks = F.lines Epoch_block.parse in
    let* () = F.eol in
    let* _eof = string "EOF" in
    let+ () = F.eol in
    { version
    ; time_info
    ; space_vehicles
    ; accuracy
    ; type_and_time
    ; base
    ; comments
    ; epoch_blocks
    }
end

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
            ((kind Gps) (prn 31)) ((kind Gps) (prn 32)) ((kind Glonass) (prn 1))
            ((kind Glonass) (prn 2)) ((kind Glonass) (prn 3)))))
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
           ((6) (8) (8) (5) (7) (7) (6) (5) (6) (7) (7) (7) (6) (7) (7) (7) (8))))
         ((accuracy_exponent
           ((7) (7) (6) (7) (7) (6) (5) (7) (6) (6) (7) (6) (7) (6) (7) (7) (7))))
         ((accuracy_exponent
           ((7) (7) (7) (7) (7) (8) (8) (9) (8) (8) (7) (9) (9) (8) (6) (8) (7))))
         ((accuracy_exponent
           ((7) (6) (6) (5) (5) (6) (6) (6) (6) (6) (7) (8) (7) (8) (7) (6) (7))))
         ((accuracy_exponent
           ((7) (6) (7) (7) (7) (6) (7) (7) (7) (7) (7) (7) () () () () ())))))
       (type_and_time ((file_type Mixed) (time_system Gps)))
       (base ((position_velocity 1.25) (clock 1.025))) (comments (() () () ()))
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
