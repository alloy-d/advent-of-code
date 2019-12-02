open Core
open Stdio

let without_position n str =
  match n with
  | 0 -> String.slice str 1 n
  | _ -> String.((slice str 0 n) ^ (slice str (n + 1) 0))

let rec _find_deviant_position pos ids =
  let count = List.length ids in
  let stripped = List.map ids ~f:(without_position pos) in
  let set = String.Set.of_list stripped in
  if Set.length set < count
  then pos
  else _find_deviant_position (pos + 1) ids

let find_deviant_position ids =
  _find_deviant_position 0 ids

let find_common_letters deviant_pos ids =
  let stripped = List.map ids ~f:(without_position deviant_pos) in
  List.find_a_dup ~compare:(String.compare) stripped

let format_result result =
  match result with
  | None -> "No result!"
  | Some str -> str

let () =
  let ids = In_channel.input_lines stdin in
  let deviant_position = find_deviant_position ids in
  find_common_letters deviant_position ids
  |> format_result
  |> Out_channel.printf "%s\n"
