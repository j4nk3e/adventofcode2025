let rec read acc =
  try
    let line = input_line stdin in
    read (line :: acc)
  with End_of_file -> List.rev acc

let parse lines =
  List.map
    (fun line ->
      let l = String.split_on_char ' ' line in
      let b = List.hd l in
      let b =
        String.sub b 1 (String.length b - 2)
        |> String.to_seq
        |> Seq.map (fun c ->
            match c with '.' -> '0' | '#' -> '1' | _ -> failwith "no match")
        |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq
        |> String.trim
      in
      let num = int_of_string ("0b" ^ b) in
      let ops =
        List.tl l
        |> List.take (List.length l - 2)
        |> List.map (fun s ->
            String.sub s 1 (String.length s - 2)
            |> String.split_on_char ',' |> List.map int_of_string
            |> List.fold_left (fun acc i -> acc lor (1 lsl i)) 0)
      in
      let j = l |> List.drop (List.length l - 1) |> List.hd in
      let j =
        String.sub j 1 (String.length j - 2)
        |> String.split_on_char ',' |> List.map int_of_string
      in
      (num, ops, j))
    lines

let rec find num count acc pos ops =
  match ops with
  | [] when num = pos -> count :: acc
  | [] -> acc
  | hd :: tl ->
      let skip = find num count acc pos tl in
      let applied = pos lxor hd in
      find num (count + 1) skip applied tl

let find_j _jolts _ops = 1 (* todo: implement *)

let () =
  let lines = read [] |> parse in
  let _ =
    lines
    |> List.map (fun (num, ops, _) ->
        let r = find num 0 [] 0 ops |> List.fold_left min Int.max_int in
        r)
    |> List.fold_left ( + ) 0 |> Printf.printf "%d\n"
  in
  lines
  |> List.map (fun (_, ops, jolts) -> find_j jolts ops)
  |> List.fold_left ( + ) 0 |> Printf.printf "%d\n"
