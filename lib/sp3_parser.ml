open Core
open Angstrom

(* Reference: https://files.igs.org/pub/data/format/sp3d.pdf *)

module Version = struct
  type t =
    | D
  [@@deriving sexp]

  let parse =
    let+ _ = char 'd' in
    D
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
    let+ v = take n in
    Int.of_string (String.strip v)

  let f w d = Fortran.f_editing ~w ~d

  let a n = take n

  let blank = char ' '
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
end

module type Parseable = sig
  type t [@@deriving sexp]

  val parse : t Angstrom.t
end

let expect_test_p (module P : Parseable) s =
  parse_string ~consume:All P.parse s
  |> [%sexp_of: (P.t, string) result]
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


(*
let line_kind : Line_kind.t t =
  let* a = any_char in
  let* b = any_char in
  match a, b with
  | '#', 'd' -> return (Line_kind.Version_identifier D)
  | '#', '#' -> return Line_kind.Time_info
  | '+', '_' -> return Line_kind.Space_vehicles
  | '+', '+' -> return Line_kind.Accuracy
  | '%', 'c' -> return Line_kind.Type_and_time
  | '%', 'f' -> return Line_kind.Base
  | _ -> fail "Unknown line kind"
   *)
