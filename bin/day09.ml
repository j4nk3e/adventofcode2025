let rec read acc =
  try
    let line = input_line stdin in
    read (line :: acc)
  with End_of_file -> List.rev acc

let parse lines =
  List.map
    (fun l ->
      match String.split_on_char ',' l |> List.map int_of_string with
      | [ x; y ] -> (x, y)
      | _ -> failwith "invalid input")
    lines

let () =
  let coords = read [] |> parse in
  List.map
    (fun (x, y) ->
      List.map (fun (x', y') -> abs (x - x' + 1) * abs (y - y' + 1)) coords)
    coords
  |> List.flatten |> List.fold_left max 0 |> Printf.printf "%d\n"
