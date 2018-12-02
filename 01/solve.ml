open Base
open Stdio

let parsed_shifts shift_strings =
  List.map shift_strings Int.of_string

let frequencies shifts =
  List.fold shifts
    ~init:[0]
    ~f:(fun (previous :: history) shift ->
      (previous + shift) :: previous :: history
    )

let format_result result =
  match result with
  | None -> "No result!"
  | Some num -> Int.to_string num

let () =
  In_channel.input_lines stdin
  |> parsed_shifts
  |> frequencies
  |> List.hd
  |> format_result
  |> Out_channel.printf "%s"
