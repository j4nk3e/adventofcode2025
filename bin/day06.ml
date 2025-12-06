let rec read acc =
  try
    let line = input_line stdin in
    read (line :: acc)
  with End_of_file -> List.rev acc

let split input = Str.split (Str.regexp "[ ]+") input

let parse lines =
  match lines |> List.rev with
  | ops :: nums ->
      let nums =
        nums |> List.map (fun l -> l |> split |> List.map int_of_string)
      in
      let ops =
        ops |> split
        |> List.map (fun c ->
            match c with
            | "*" -> fun a b -> a * b
            | "+" -> fun a b -> a + b
            | _ -> failwith "undefined op")
      in
      (nums, ops)
  | _ -> failwith "undefined input"

let rec merge nums ops =
  match nums with
  | [] :: _ -> []
  | _ ->
      let column = List.map List.hd nums in
      let res =
        List.fold_left (List.hd ops) (List.hd column) (List.tl column)
      in
      res :: merge (List.map List.tl nums) (List.tl ops)

let rec transpose = function
  | [] -> []
  | [] :: _ -> []
  | rows -> List.map List.hd rows :: transpose (List.map List.tl rows)

let join l =
  match l |> List.rev |> List.to_seq |> String.of_seq |> String.trim with
  | "" -> None
  | s ->
      let q = int_of_string s in
      Some q

let parse2 lines =
  match
    lines |> List.rev
    |> List.map (fun s -> s |> String.to_seq |> List.of_seq |> List.rev)
  with
  | ops :: nums ->
      let n = nums |> transpose |> List.map join in
      (ops, n)
  | _ -> failwith "undefined input"

let rec merge2 acc ops nums =
  match (ops, nums) with
  | [], [] -> []
  | ' ' :: tl, None :: tn -> merge2 acc tl tn
  | ' ' :: tl, Some n :: tn -> merge2 (n :: acc) tl tn
  | '+' :: tl, Some n :: tn ->
      List.fold_left (fun a b -> a + b) n acc :: merge2 [] tl tn
  | '*' :: tl, Some n :: tn ->
      List.fold_left (fun a b -> a * b) n acc :: merge2 [] tl tn
  | _ -> failwith "undefined structure"

let () =
  let lines = read [] in
  let nums, ops = lines |> parse in
  let _ =
    merge nums ops
    |> List.fold_left (fun a b -> a + b) 0
    |> Printf.printf "%d\n"
  in
  let ops, nums = lines |> parse2 in
  merge2 [] ops nums
  |> List.fold_left (fun a b -> a + b) 0
  |> Printf.printf "%d\n"
