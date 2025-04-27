open Core

type t = int [@@deriving sexp, bin_io, compare, equal]

let epoch_start =
  (* Time_ns doesn't handle leap seconds anyway.... *)
  Time_ns.of_date_ofday
    ~zone:Time_ns_unix.Zone.utc
    (Date.of_string "1980-01-06")
    Time_ns.Ofday.start_of_day

let length_of_week = Time_ns.Span.of_day 7.

let of_time_ns (time : Time_ns.t) =
  let span = Time_ns.diff time epoch_start in
  let weeks = Time_ns.Span.(//) span length_of_week in
  Float.round_down weeks
  |> Int.of_float

let start_of_week week =
  Time_ns.add epoch_start (Time_ns.Span.scale_int length_of_week week)

let%expect_test _ =
  of_time_ns (Time_ns_unix.of_string "2025-04-27T07:00:00Z")
  |> [%sexp_of: t] |> print_s;
  [%expect {| 2364 |}]
