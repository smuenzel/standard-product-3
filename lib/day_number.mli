open! Core

type t [@@immediate] [@@deriving compare, bin_io, equal, sexp]

val to_date : year:int -> t -> Date.t
val of_date : Date.t -> t
