open! Core

type t [@@immediate] [@@deriving sexp, bin_io, compare, equal]

val to_int : t -> int

val start_of_week : t -> Time_ns.t
val of_time_ns : Time_ns.t -> t
