open Base
open Stdio

let determine_final_shift shifts =
  List.fold shifts
    ~init:0
    ~f:(fun freq shift -> freq + shift)

let parsed_shifts shift_strings =
  List.map shift_strings Int.of_string

let () =
  In_channel.input_lines stdin
  |> parsed_shifts
  |> determine_final_shift
  |> Out_channel.printf "%d"
