open Core
open Stdio

let parsed_shifts shift_strings =
  List.map shift_strings ~f:Int.of_string

let frequencies shifts =
  List.fold shifts
    ~init:[0]
    ~f:(fun freqs shift ->
      match freqs with
      | (previous :: history) -> (previous + shift) :: previous :: history
      | [] -> []
    )

let format_result result =
  match result with
  | None -> "No result!"
  | Some num -> Int.to_string num

let shift_streamer pattern =
  fun index ->
    List.nth pattern (index mod List.length pattern)

let rec _find_duplicate seen current shifts =
  let shift = Stream.next shifts in
  let next = current + shift in
  if Set.mem seen next
  then next
  else _find_duplicate (Set.add seen next) next shifts

let find_duplicate stream =
  _find_duplicate (Int.Set.of_list [0]) 0 stream

let () =
  let shifts = parsed_shifts (In_channel.input_lines stdin) in
  let streamer = shift_streamer shifts in
  let stream = Stream.from streamer in
  Out_channel.printf "%d" (find_duplicate stream)

(*
let () =
  In_channel.input_lines stdin
  |> parsed_shifts
  |> frequencies
  |> List.rev
  |> first_duplicate
  |> format_result
  |> Out_channel.printf "%s"
*)
