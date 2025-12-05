let rec read acc =
  try
    let line = input_line stdin in
    read (line :: acc)
  with End_of_file -> List.rev acc

let parse lines =
  match List.find_index (fun l -> l = "") lines with
  | Some sep ->
      let ranges =
        List.take sep lines
        |> List.map (fun range ->
            match String.split_on_char '-' range |> List.map int_of_string with
            | [ a; b ] -> (a, b)
            | _ -> failwith "no range")
      in
      let available = List.drop (sep + 1) lines |> List.map int_of_string in
      (ranges, available)
  | None -> failwith "missing separator"

let in_range n (lower, upper) = lower <= n && n <= upper

let rec merge_ranges sum list =
  match list with
  | [] -> sum
  | [ (lower, upper) ] -> sum + upper - lower + 1
  | (lower, upper) :: (next, _) :: _ when next > upper ->
      merge_ranges (sum + upper - lower + 1) (List.tl list)
  | (lower, u1) :: (_, u2) :: tl -> merge_ranges sum ((lower, max u1 u2) :: tl)

let () =
  let ranges, available = read [] |> parse in
  let _ =
    List.filter (fun a -> List.exists (in_range a) ranges) available
    |> List.length |> Printf.printf "%d\n"
  in
  ranges
  |> List.sort (fun a b -> compare (Pair.fst a) (Pair.fst b))
  |> merge_ranges 0 |> Printf.printf "%d\n"
