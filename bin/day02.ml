let rec read acc =
  try
    let line = input_line stdin in
    read (line :: acc)
  with End_of_file -> List.rev acc

let parse lines =
  List.hd lines |> String.split_on_char ','
  |> List.map (fun range ->
      match String.split_on_char '-' range |> List.map int_of_string with
      | [ from; to_val ] -> (from, to_val)
      | _ -> failwith "Invalid range format")

let rec next_invalid n =
  let l = n |> float_of_int |> log10 |> floor |> int_of_float in
  if l mod 2 = 0 then
    let q = 10.0 ** (float_of_int l +. 1.0) |> int_of_float in
    q |> next_invalid
  else
    let factor = 10.0 ** (float_of_int (l + 1) /. 2.0) |> int_of_float in
    let top = n / factor in
    let bot = n - (top * factor) in
    if top >= bot then (top * factor) + top else (top + 1) * (factor + 1)

let rec invalid_pt1 sum (a, b) =
  let n = next_invalid a in
  if n > b then sum else invalid_pt1 (sum + n) (n + 1, b)

let rec is_repeating pat r =
  if r = "" then true
  else if String.starts_with ~prefix:pat r then
    is_repeating pat
      (String.sub r (String.length pat) (String.length r - String.length pat))
  else false

let is_invalid num =
  let s = num |> string_of_int in
  1 |> Seq.ints
  |> Seq.take (String.length s / 2)
  |> Seq.exists (fun i ->
      is_repeating (String.sub s 0 i) (String.sub s i (String.length s - i)))

let rec invalid_pt2 sum (a, b) =
  if a > b then sum
  else if is_invalid a then invalid_pt2 (sum + a) (a + 1, b)
  else invalid_pt2 sum (a + 1, b)

let rec sum_invalid f list sum =
  match list with [] -> sum | range :: tl -> sum_invalid f tl sum + f 0 range

let () =
  let ranges = read [] |> parse in
  Printf.printf "%d\n%d\n"
    (sum_invalid invalid_pt1 ranges 0)
    (sum_invalid invalid_pt2 ranges 0)
