open Core
open Angstrom
open! Util

(* Reference: https://files.igs.org/pub/data/format/sp3d.pdf *)

module Bitset = struct
  include Bitset

  let create_full ~len =
    let result = create ~len in
    set_all result;
    result

  let sexp_of_t t =
    if Bitset.is_empty t
    then Sexp.List [ Atom "Empty"; Int.sexp_of_t (Bitset.capacity t) ]
    else if Bitset.is_empty (Bitset.complement t)
    then Sexp.List [ Atom "Full"; Int.sexp_of_t (Bitset.capacity t) ]
    else sexp_of_t t

  let t_of_sexp sexp =
    match (sexp : Sexp.t) with
    | List [ Atom "Empty"; len ] -> 
      let len = [%of_sexp: int] len in
      create ~len
    | List [ Atom "Full"; len ] -> 
      let len = [%of_sexp: int] len in
      create_full ~len
    | _ -> t_of_sexp sexp
end

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

  let blank = let+ _ = char ' ' <?> "F.blank" in ()

  let blank_n n =
    let+ _ = Angstrom.count n blank in
    ()

  let i n =
    i n <?> "F.i"

  let i_opt n =
    choice
      [ i n >>| Option.some
      ; blank_n n >>| fun () -> None
      ]

  let f w d = Fortran.f_editing ~w ~d

  let f_opt w d =
    choice
      [ f w d >>| Option.some
      ; blank_n w >>| fun () -> None
      ]

  let a n = take n

  let a n =
    a n <?> "F.a"

  let eol =
    let* () = skip_while Char.is_blank in
    let+ _ = char '\n' in
    ()

  let eol = 
    eol <?> "F.eol"

  let line p =
    let* p = p in
    let+ () = eol in
    p

  let line p =
    line p <?> "F.line"

  let lines p =
    let+ l = sep_by eol p in
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
    [@@deriving sexp, equal]

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
    } [@@deriving sexp, equal]

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
      ; (let* kind = Space_vehicle_id.Kind.parse in
        let+ _ = F.blank in
        Only kind)
      ; (let* _ = F.blank in
        let+ kind = Space_vehicle_id.Kind.parse in
        Only kind)
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
        let* _ = F.eol in
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
      let* _ = F.eol in
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

  let clock_is_none clock =
    Bignum.equal (Bignum.truncate (Bigdecimal.to_bignum clock)) (Bignum.of_int 999999)

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
      let* x_stddev = F.i_opt 2 in
      let* _ = F.blank in
      let* y_stddev = F.i_opt 2 in
      let* _ = F.blank in
      let* z_stddev = F.i_opt 2 in
      let* _ = F.blank in
      let+ clock_stddev = F.i_opt 3 in
      x_stddev, y_stddev, z_stddev, clock_stddev

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

  module Velocity = struct
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
        } [@@deriving sexp]

      let parse =
        let* x = F.f 14 6 in
        let* y = F.f 14 6 in
        let* z = F.f 14 6 in
        let* clock = F.f 14 6 in
        let* () = F.blank in
        let* x_stddev = F.i_opt 2 in
        let* () = F.blank in
        let* y_stddev = F.i_opt 2 in
        let* () = F.blank in
        let* z_stddev = F.i_opt 2 in
        let* () = F.blank in
        let* clock_stddev = F.i_opt 3 in
        let+ () = skip_while Char.is_blank in
        let clock = if clock_is_none clock then None else Some clock in
        { x
        ; y
        ; z
        ; clock
        ; x_stddev
        ; y_stddev
        ; z_stddev
        ; clock_stddev
        }
    end

    type t =
      { space_vehicle_id : Space_vehicle_id.t
      ; data : Data.t
      } [@@deriving sexp]

    let parse = 
      let* _ = char 'V' in
      let* space_vehicle_id = Space_vehicle_id.parse in
      let+ data = Data.parse in
      { space_vehicle_id; data }
  end

end

module type Parseable = sig
  type t [@@deriving sexp]

  val parse : t Angstrom.t
end

module Epoch_block = struct
  type t =
    Line.Epoch.t * (Line.Position_and_clock.t * Line.Velocity.t option) list
  [@@deriving sexp]

  let with_velocity =
    let* pos_and_clock = Line.Position_and_clock.parse in
    let velocity =
      let* () = F.eol in
      let+ velocity = Line.Velocity.parse in
      Some velocity
    in
    let+ velocity = option None velocity in
    pos_and_clock, velocity

  let parse =
    let* epoch_header = F.line Line.Epoch.parse in
    let+ records = F.lines with_velocity in
    epoch_header, records

end

module Header = struct
  type t =
    { version : Line.Version.t
    ; time_info : Line.Time_info.t
    ; space_vehicles : Line.Space_vehicles.t list
    ; accuracy : Line.Accuracy.t list
    ; type_and_time : Line.Type_and_time.t
    ; base : Line.Base.t
    ; comments : Line.Comment.t list
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
    let+ comments = F.lines Line.Comment.parse in
    { version
    ; time_info
    ; space_vehicles
    ; accuracy
    ; type_and_time
    ; base
    ; comments
    }
end

module Full_file = struct
  type t =
    { header : Header.t
    ; epoch_blocks : (Line.Epoch.t * (Line.Position_and_clock.t * Line.Velocity.t option) list) list
    } [@@deriving sexp]

  let parse =
    let* header = F.line Header.parse in
    let* epoch_blocks = F.lines Epoch_block.parse in
    let* () = if List.is_empty epoch_blocks then Angstrom.return () else F.eol in
    let* _eof = string "EOF" in
    let+ () = F.eol in
    { header
    ; epoch_blocks
    }
end

module Processed_file = struct
  module Presence = struct
    type t =
      { pos : Bitset.t
      ; clock : Bitset.t
      ; pos_stddev : Bitset.t
      ; clock_stddev : Bitset.t
      ; maneuver : Bitset.t
      ; clock_event : Bitset.t
      } [@@deriving sexp]

    let create len =
      { pos = Bitset.create_full ~len
      ; clock = Bitset.create_full ~len
      ; pos_stddev = Bitset.create_full ~len
      ; clock_stddev = Bitset.create_full ~len
      ; maneuver = Bitset.create ~len
      ; clock_event = Bitset.create ~len
      }
  end

  module Epoch = struct
    type t =
      { metadata : Line.Epoch.t
      ; x : Float_option.Array.t
      ; y : Float_option.Array.t
      ; z : Float_option.Array.t
      ; clock : Float_option.Array.t
      ; x_stddev : Float_option.Array.t
      ; y_stddev : Float_option.Array.t
      ; z_stddev : Float_option.Array.t
      ; clock_stddev : Float_option.Array.t
      ; maneuver : Bitset.t
      ; orbit_pred : Bitset.t
      ; clock_event : Bitset.t
      ; clock_pred : Bitset.t
      } [@@deriving sexp]

    let of_epoch_block
        ~(presence : Presence.t)
        ~(base : Line.Base.t)
        ~(space_vehicles : Space_vehicle_id.t array)
        (epoch_block : Line.Epoch.t * (Line.Position_and_clock.t * Line.Velocity.t option) list)
      =
      let metadata, records = epoch_block in
      let len = Array.length space_vehicles in
      let result =
        { metadata
        ; x = Float_option.Array.create len
        ; y = Float_option.Array.create len
        ; z = Float_option.Array.create len
        ; clock = Float_option.Array.create len
        ; x_stddev = Float_option.Array.create len
        ; y_stddev = Float_option.Array.create len
        ; z_stddev = Float_option.Array.create len
        ; clock_stddev = Float_option.Array.create len
        ; maneuver = Bitset.create ~len
        ; orbit_pred = Bitset.create ~len
        ; clock_event = Bitset.create ~len
        ; clock_pred = Bitset.create ~len
        }
      in
      let convert_pos (b : Bigdecimal.t) =
        Bigdecimal.scale_by ~power_of_ten:(-3) b
        |> Bigdecimal.to_float
        |> Float_option.some
      in
      let convert_pos_stddev (s : int option) =
        match s with
        | None -> Float_option.none
        | Some exponent ->
          Bigdecimal.( ** ) base.position_velocity exponent
          |> Bigdecimal.scale_by ~power_of_ten:(-3)
          |> Bigdecimal.to_float
          |> Float_option.some
      in
      let convert_clock_stddev (s : int option) =
        match s with
        | None -> Float_option.none
        | Some exponent ->
          Bigdecimal.( ** ) base.clock exponent
          |> Bigdecimal.scale_by ~power_of_ten:(-12)
          |> Bigdecimal.to_float
          |> Float_option.some
      in
      let processed =
        List.foldi ~init:0
          records
          ~f:(fun i processed (record, _velocity) ->
              let space_vehicle = space_vehicles.(i) in
              if not (Space_vehicle_id.equal space_vehicle record.space_vehicle_id)
              then begin
                raise_s
                  [%message
                    "Unexpected space vehicle"
                      ~expected:(space_vehicle : Space_vehicle_id.t)
                      ~actual:(record.space_vehicle_id : Space_vehicle_id.t)
                      ~epoch:(metadata : Line.Epoch.t)
                      ~index:(i : int)
                  ]
              end;
              let none = Float_option.none in
              let x, y, z, clock, x_stddev, y_stddev, z_stddev, clock_stddev =
                match record.data with
                | None ->
                  Bitset.assign presence.pos i false;
                  Bitset.assign presence.clock i false;
                  none, none, none, none, none, none, none, none
                | Some data ->
                  let x = convert_pos data.x in
                  let y = convert_pos data.y in
                  let z = convert_pos data.z in
                  let clock =
                    match data.clock with
                    | None ->
                      Bitset.assign presence.clock i false;
                      none
                    | Some clock ->
                      Bigdecimal.scale_by ~power_of_ten:(-6) clock
                      |> Bigdecimal.to_float
                      |> Float_option.some
                  in
                  let x_stddev = convert_pos_stddev data.x_stddev in
                  let y_stddev = convert_pos_stddev data.y_stddev in
                  let z_stddev = convert_pos_stddev data.z_stddev in
                  begin match%optional.Float_option x_stddev, y_stddev, z_stddev with
                    | Some _, Some _, Some _ -> ()
                    | _ -> Bitset.assign presence.pos_stddev i false
                  end;
                  let clock_stddev = convert_clock_stddev data.clock_stddev in
                  begin match%optional.Float_option clock_stddev with
                    | Some _ -> ()
                    | _ -> Bitset.assign presence.clock_stddev i false
                  end;
                  Bitset.assign result.maneuver i data.maneuver;
                  if data.maneuver
                  then Bitset.assign presence.maneuver i true;
                  Bitset.assign result.orbit_pred i data.orbit_pred;
                  Bitset.assign result.clock_event i data.clock_event;
                  if data.clock_event
                  then Bitset.assign presence.clock_event i true;
                  Bitset.assign result.clock_pred i data.clock_pred;
                  x, y, z, clock, x_stddev, y_stddev, z_stddev, clock_stddev
              in
              Float_option.Array.unsafe_set result.x i x;
              Float_option.Array.unsafe_set result.y i y;
              Float_option.Array.unsafe_set result.z i z;
              Float_option.Array.unsafe_set result.clock i clock;
              Float_option.Array.unsafe_set result.x_stddev i x_stddev;
              Float_option.Array.unsafe_set result.y_stddev i y_stddev;
              Float_option.Array.unsafe_set result.z_stddev i z_stddev;
              Float_option.Array.unsafe_set result.clock_stddev i clock_stddev;
              processed + 1
            )
      in
      if processed <> len
      then begin
        raise_s
          [%message "expected number of space vehicles not found in epoch block"
              ~expected:(len : int)
              ~actual:(processed : int)
              ~epoch:(metadata : Line.Epoch.t)]
      end;
      result
  end

  type t =
    { raw_header : Header.t
    ; space_vehicles : Space_vehicle_id.t array
    ; accuracy : Float_option.Array.t
    ; epochs : Epoch.t list
    ; presence : Presence.t
    } [@@deriving sexp]

  let parse =
    let* raw_header = F.line Header.parse in
    let* epoch_blocks = F.lines Epoch_block.parse in
    let* () =  if List.is_empty epoch_blocks then Angstrom.return () else F.eol in
    let* _eof = string "EOF" in
    let+ () = F.eol in
    let space_vehicles =
      List.concat_map raw_header.space_vehicles ~f:(fun x ->
          x.ids
        )
      |> Array.of_list
    in
    let space_vehicle_count = Array.length space_vehicles in
    let presence = Presence.create space_vehicle_count in
    let accuracy =
      List.concat_map
        raw_header.accuracy
        ~f:(fun { Line.Accuracy.accuracy_exponent } -> accuracy_exponent)
      |> Array.of_list
      |> (fun a -> Array.slice a 0 space_vehicle_count)
      |> Float_option.Array.of_option_array_map
        ~f:(fun i ->
            let p =
              Int.( ** ) 2 i
              |> Float.of_int
            in
            p /. 1000.0
          )
    in
    let epochs =
      List.map
        epoch_blocks
        ~f:(Epoch.of_epoch_block ~presence ~base:raw_header.base ~space_vehicles)
    in
    { raw_header
    ; space_vehicles
    ; epochs
    ; accuracy
    ; presence
    }

end

