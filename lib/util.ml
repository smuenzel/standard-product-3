open Sexplib0
open Sexplib0.Sexp_conv

let empty_float_array = Float.Array.create 0

let floatarray_of_sexp sexp : floatarray =
  match (sexp : Sexp.t) with
  | List [] -> Float.Array.create 0
  | List list ->
    let len = List.length list in
    let arr = Float.Array.create len in
    List.iteri (fun i x -> Float.Array.unsafe_set arr i (float_of_sexp x)) list;
    arr
  | Atom _ -> of_sexp_error "floatarray_of_sexp: list needed" sexp

let sexp_of_floatarray (ar : floatarray) : Sexp.t =
  let lst_ref = ref [] in
  for i = Float.Array.length ar - 1 downto 0 do
    lst_ref := sexp_of_float (Float.Array.unsafe_get ar i):: !lst_ref
  done;
  List !lst_ref
