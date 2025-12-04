let rec read acc =
  try
    let line = input_line stdin in
    read (line :: acc)
  with End_of_file -> List.rev acc

let parse lines = List.map (fun l -> l) lines

let () =
  let lines = read [] |> parse in
  List.iter (fun line -> Printf.printf "%s\n" line) lines
