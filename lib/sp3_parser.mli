(* SPDX-License-Identifier: MPL-2.0
 * SPDX-FileCopyrightText: (c) 2025 Stefan Muenzel
 *)

open! Util

module Bitset := Fast_bitvector

module F : sig
  val lines : 'a Angstrom.t -> 'a list Angstrom.t
end

module Version : sig
  type t =
    | C
    | D
  [@@deriving sexp]

  val parse : t Angstrom.t
end

module Pos_or_vel : sig
  type t =
    | Pos
    | Vel
  [@@deriving sexp]

  val parse : t Angstrom.t
end

module Space_vehicle_id : sig
  module Kind : sig
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

    val parse : t Angstrom.t
  end

  type t =
    { kind : Kind.t
    ; prn: int
    } [@@deriving sexp, equal]

  val parse : t Angstrom.t
  val parse_opt : t option Angstrom.t
end

module File_type : sig
  type t =
    | Mixed
    | Only of Space_vehicle_id.Kind.t
  [@@deriving sexp]

  val parse : t Angstrom.t
end

module Time_system : sig
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

  val parse : t Angstrom.t
end

module Line : sig
  module Version : sig
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

    val parse : t Angstrom.t
  end

  module Time_info : sig
    type t =
      { gps_week : int
      ; seconds_of_week : Bigdecimal.t
      ; epoch_interval : Bigdecimal.t
      ; modified_julian_day : int
      ; fractional_day : Bigdecimal.t
      } [@@deriving sexp]

    val parse : t Angstrom.t
  end

  module Space_vehicles : sig
    type t =
      { number : int option
      ; ids : Space_vehicle_id.t list
      } [@@deriving sexp]

    val parse : t Angstrom.t
  end

  module Accuracy : sig
    type t =
      { accuracy_exponent : int option list
      } [@@deriving sexp]

    val parse : t Angstrom.t
  end

  module Type_and_time : sig
    type t =
      { file_type : File_type.t
      ; time_system : Time_system.t
      } [@@deriving sexp]

    val parse : t Angstrom.t
  end

  module Base : sig
    type t =
      { position_velocity : Bigdecimal.t
      ; clock : Bigdecimal.t
      } [@@deriving sexp]

    val parse : t Angstrom.t
  end

  module Int_empty : sig
    type t = unit [@@deriving sexp]

    val parse : t Angstrom.t
  end

  module Comment : sig
    type t = unit [@@deriving sexp]

    val parse : t Angstrom.t
  end

  module Epoch : sig
    type t =
      { year : int
      ; month : int
      ; day_of_month : int
      ; hour : int
      ; minute : int
      ; second : Bigdecimal.t
      } [@@deriving sexp]

    val parse : t Angstrom.t
  end


  module Position_and_clock : sig
    module Data : sig
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

    val parse : t Angstrom.t
  end

  module Velocity : sig
    module Data : sig
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

      val parse : t Angstrom.t
    end

    type t =
      { space_vehicle_id : Space_vehicle_id.t
      ; data : Data.t
      } [@@deriving sexp]

    val parse : t Angstrom.t
  end

end


module type Parseable = sig
  type t [@@deriving sexp]

  val parse : t Angstrom.t
end

module Epoch_block : sig
  type t =
    Line.Epoch.t * (Line.Position_and_clock.t * Line.Velocity.t option) list
  [@@deriving sexp]

  val parse : t Angstrom.t
end

module Header : sig
  type t =
    { version : Line.Version.t
    ; time_info : Line.Time_info.t
    ; space_vehicles : Line.Space_vehicles.t list
    ; accuracy : Line.Accuracy.t list
    ; type_and_time : Line.Type_and_time.t
    ; base : Line.Base.t
    ; comments : Line.Comment.t list
    } [@@deriving sexp]

  val parse : t Angstrom.t
end

module Full_file : sig
  type t =
    { header : Header.t
    ; epoch_blocks : (Line.Epoch.t * (Line.Position_and_clock.t * Line.Velocity.t option) list) list
    } [@@deriving sexp]

  val parse : t Angstrom.t
end

module Processed_file : sig
  module Presence : sig
    type t =
      { pos : Bitset.t
      ; clock : Bitset.t
      ; pos_stddev : Bitset.t
      ; clock_stddev : Bitset.t
      ; maneuver : Bitset.t
      ; clock_event : Bitset.t
      ; velocity : Bitset.t
      ; clock_velocity : Bitset.t
      ; velocity_stddev : Bitset.t
      ; clock_velocity_stddev : Bitset.t
      } [@@deriving sexp]

    val create : int -> t
  end

  module Epoch : sig
    module Velocity : sig
      type t =
        { x : Float_option.Array.t
        ; y : Float_option.Array.t
        ; z : Float_option.Array.t
        ; clock : Float_option.Array.t
        ; x_stddev : Float_option.Array.t
        ; y_stddev : Float_option.Array.t
        ; z_stddev : Float_option.Array.t
        ; clock_stddev : Float_option.Array.t
        } [@@deriving sexp]
    end

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
      ; velocity : Velocity.t option
      } [@@deriving sexp]

    val of_epoch_block
      :  presence:Presence.t
      -> base:Line.Base.t
      -> space_vehicles:Space_vehicle_id.t array
      -> (Line.Epoch.t * (Line.Position_and_clock.t * Line.Velocity.t option) list)
      -> t
  end

  type t =
    { raw_header : Header.t
    ; space_vehicles : Space_vehicle_id.t array
    ; accuracy : Float_option.Array.t
    ; epoch_count : int
    ; epochs : Epoch.t list
    ; presence : Presence.t
    } [@@deriving sexp]

  val parse : t Angstrom.t
end



















