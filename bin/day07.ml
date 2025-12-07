module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

let rec read acc =
  try
    let line = input_line stdin in
    read (line :: acc)
  with End_of_file -> List.rev acc

let parse lines =
  lines
  |> List.map (fun l ->
      l |> String.to_seqi |> List.of_seq
      |> List.filter (fun (_i, c) -> c = 'S' || c = '^')
      |> List.map Pair.fst)
  |> List.filter (fun l -> not (List.is_empty l))

let rec splice count beams split next =
  match split with
  | [] -> (count, next)
  | hd :: tl ->
      if IntSet.mem hd beams then
        let next =
          next |> IntSet.remove hd |> IntSet.add (hd - 1) |> IntSet.add (hd + 1)
        in
        splice (count + 1) beams tl next
      else splice count beams tl next

let rec beam count beams split =
  match split with
  | [] -> count
  | hd :: tl ->
      let s, beams = splice 0 beams hd beams in
      beam (count + s) beams tl

let rec quantum_splice map l next =
  match l with
  | [] -> next
  | hd :: tl -> (
      match IntMap.find_opt hd map with
      | Some count ->
          quantum_splice map tl
            (next |> IntMap.remove hd
            |> IntMap.update (hd - 1) (function
              | Some n -> Some (n + count)
              | None -> Some count)
            |> IntMap.update (hd + 1) (function
              | Some n -> Some (n + count)
              | None -> Some count))
      | None -> quantum_splice map tl next)

let rec timelines map split =
  match split with
  | [] -> map
  | hd :: tl -> timelines (quantum_splice map hd map) tl

let () =
  let splits = read [] |> parse in
  let start = List.hd splits |> IntSet.of_list in
  let _ = beam 0 start (List.tl splits) |> Printf.printf "%d\n" in
  let start = List.hd splits |> List.map (fun i -> (i, 1)) |> IntMap.of_list in
  timelines start (List.tl splits)
  |> IntMap.to_list
  |> List.fold_left (fun a (_i, b) -> a + b) 0
  |> Printf.printf "%d\n"
