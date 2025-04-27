open Core

type t = int [@@deriving sexp, bin_io, compare, equal]

let of_date (date : Date.t) =
  let y = Date.year date in
  (Date.diff date (Date.create_exn ~y ~m:Jan ~d:1)) + 1

let to_date ~year (day_number : t) =
  Date.add_days (Date.create_exn ~y:year ~m:Jan ~d:1) (day_number - 1)

let%expect_test _ =
  of_date (Date.create_exn ~y:2025 ~m:Apr ~d:27)
  |> [%sexp_of: t] |> print_s;
  [%expect {| 117 |}]
