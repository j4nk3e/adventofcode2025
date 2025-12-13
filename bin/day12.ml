let rec read acc =
  try
    let line = input_line stdin in
    read (line :: acc)
  with End_of_file -> List.rev acc

let parse lines =
  List.filter_map
    (fun l ->
      match l |> Str.split (Str.regexp ": ") with
      | [ area; rest ] -> (
          let b =
            rest |> String.trim |> String.split_on_char ' '
            |> List.map int_of_string |> List.fold_left ( + ) 0
          in
          match String.split_on_char 'x' area |> List.map int_of_string with
          | [ x; y ] -> Some (x * y, b * 9)
          | _ -> None)
      | _ -> None)
    lines

let () =
  let lines = read [] |> parse in
  lines
  |> List.filter (fun (a, b) -> a >= b)
  |> List.length |> Printf.printf "%d\n"
