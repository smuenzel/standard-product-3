open! Core

let command =
  Command.basic
    ~summary:""
    [%map_open.Command
      let filename = anon ("FILENAME" %: string)
      in
      fun () ->
        let result = Standard_product_3.Sp3.process_file filename in
        result
        |> [%sexp_of : (Standard_product_3.Sp3_parser.Processed_file.t, string) Result.t]
        |> print_s
    ]

let () =
  Command_unix.run command
