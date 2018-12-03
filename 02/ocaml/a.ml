open Core
open Stdio

let character_counts id =
  String.fold id
    ~init:Char.Map.empty
    ~f:(fun counts character ->
      let count =
        match Map.find counts character with
        | Some n -> n
        | None -> 0
      in
      Map.set counts ~key:character ~data:(count + 1)
    )

let score counts =
  Map.fold counts
    ~init:(0,0)
    ~f:(fun ~key ~data (two, three) ->
      match data with
      | 2 -> (1, three)
      | 3 -> (two, 1)
      | _ -> (two, three)
    )

let checksum scores =
  let (sum_two, sum_three) = List.fold scores
    ~init:(0,0)
    ~f:(fun (sum_two, sum_three) (two, three) ->
      (sum_two + two, sum_three + three)
    )
  in
  sum_two * sum_three
  
let format_result (id, counts) =
  Printf.sprintf "%s => %a" id counts

let () =
  In_channel.input_lines stdin
  |> List.map ~f:character_counts
  |> List.map ~f:score
  |> checksum
  |> Out_channel.printf "%d"
