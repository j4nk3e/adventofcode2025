let rec read acc =
  try
    let line = input_line stdin in
    read (line :: acc)
  with End_of_file -> List.rev acc

let () =
  let lines = read [] in
  List.iter print_endline lines
