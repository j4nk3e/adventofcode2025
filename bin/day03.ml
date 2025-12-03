let rec read acc =
  try
    let line = input_line stdin in
    read (line :: acc)
  with End_of_file -> List.rev acc

let parse line =
  String.to_seq line |> Seq.map (fun n -> int_of_char n - 48) |> List.of_seq

let rec max_jolt count prefix nums =
  if List.length prefix = count then
    prefix |> List.rev |> List.fold_left (fun a n -> (a * 10) + n) 0
  else
    let len = List.length nums in
    let candidates = List.take (len - count + 1 + List.length prefix) nums in
    let m = List.fold_left max 0 candidates in
    match List.find_index (fun a -> a = m) nums with
    | Some i -> max_jolt count (m :: prefix) (List.drop (i + 1) nums)
    | None -> failwith "wtf"

let sum_max n l =
  l |> List.map (max_jolt n []) |> List.fold_left (fun a b -> a + b) 0

let () =
  let nums = read [] |> List.map parse in
  Printf.printf "%d\n%d\n" (sum_max 2 nums) (sum_max 12 nums)
