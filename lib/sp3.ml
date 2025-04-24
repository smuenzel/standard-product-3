open! Core

let map_file filename =
  let fd = Core_unix.openfile ~mode:[ O_RDONLY ] filename in
  let size = (Core_unix.fstat fd).st_size in
  Bigstring_unix.map_file ~shared:false fd (Int64.to_int_exn size)

let parse_file filename =
  let bs = map_file filename in
  Angstrom.parse_bigstring ~consume:All Sp3_parser.Full_file.parse bs
