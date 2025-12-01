let rec read acc =
  try
    let line = input_line stdin in
    read (line :: acc)
  with End_of_file -> List.rev acc

let parse lines =
  List.map
    (fun l ->
      let len = String.length l in
      let dir = l.[0] in
      let n = String.sub l 1 (len - 1) |> int_of_string in
      match dir with
      | 'L' -> -n
      | 'R' -> n
      | c -> Printf.sprintf "Invalid: %c" c |> failwith)
    lines

let () =
  let clicks = read [] |> parse in
  let start = 50 in
  let _p, a =
    List.fold_left
      (fun (p, c) b ->
        let p' = (p + b) mod 100 in
        (p', match p' with 0 -> c + 1 | _ -> c))
      (start, 0) clicks
  in
  let _p, b =
    List.fold_left
      (fun (p, w) b ->
        let zeros =
          if b > 0 then (p + b) / 100
          else if p = 0 then -b / 100
          else if p <= -b then (100 - p - b) / 100
          else 0
        in
        let p' = (((p + b) mod 100) + 100) mod 100 in
        (p', w + zeros))
      (start, 0) clicks
  in
  Printf.printf "%d\n%d\n" a b
