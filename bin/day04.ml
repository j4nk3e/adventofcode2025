module CoordSet = Set.Make (struct
  type t = int * int

  let compare (x, y) (x', y') =
    match compare x x' with 0 -> compare y y' | c -> c
end)

let rec read acc =
  try
    let line = input_line stdin in
    read (line :: acc)
  with End_of_file -> List.rev acc

let parse lines =
  lines
  |> List.mapi (fun y l ->
      String.to_seqi l
      |> Seq.filter_map (fun (x, c) -> if c = '@' then Some (x, y) else None)
      |> List.of_seq)
  |> List.flatten

let adjacent set pred (x, y) =
  [
    (x - 1, y - 1);
    (x, y - 1);
    (x + 1, y - 1);
    (x - 1, y);
    (x + 1, y);
    (x - 1, y + 1);
    (x, y + 1);
    (x + 1, y + 1);
  ]
  |> List.filter (fun c -> CoordSet.mem c set)
  |> pred

let rec remove sum pred set =
  let removed = CoordSet.filter (adjacent set pred) set in
  let diff = CoordSet.cardinal removed in
  let _ = Printf.printf "Removed: %d\n" diff in
  match diff with
  | 0 -> sum
  | _ -> remove (sum + diff) pred (CoordSet.diff set removed)

let () =
  let rolls = read [] |> parse in
  let set = CoordSet.of_seq @@ List.to_seq rolls in
  set
  |> remove 0 (fun l -> List.length l < 4)
  |> Printf.printf "Total removed: %d\n"
