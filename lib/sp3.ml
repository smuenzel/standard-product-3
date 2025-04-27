open! Core

let map_file filename =
  let fd = Core_unix.openfile ~mode:[ O_RDONLY ] filename in
  let size = (Core_unix.fstat fd).st_size in
  Bigstring_unix.map_file ~shared:false fd (Int64.to_int_exn size)

let with_nice_error ~parse filename =
  let bs = map_file filename in
  let state = Angstrom.Buffered.parse parse in
  let state = Angstrom.Buffered.feed state (`Bigstring bs) in
  let state = Angstrom.Buffered.feed state `Eof in
  match state with
  | Done (unconsumed, result) ->
    if unconsumed.len = 0
    then Ok result
    else Or_error.errorf "unconsumed offset %d" unconsumed.off
  | Partial _ ->
    Or_error.errorf "incomplete input"
  | Fail (unconsumed, marks, msg) ->
    Or_error.errorf !"unconsumed offset %d: %s (%{sexp:string list})" unconsumed.off msg
      marks


let parse_file filename =
  let bs = map_file filename in
  Angstrom.parse_bigstring ~consume:All Sp3_parser.Full_file.parse bs

let process_file filename =
  with_nice_error ~parse:Sp3_parser.Processed_file.parse filename
