open Base
open Stdio

let () =
  let result = In_channel.fold_lines
    stdin
    ~init:0
    ~f:(fun freq shift_string ->
      let shift = Int.of_string shift_string in
      freq + shift)
  in
  Out_channel.printf "%d" result
