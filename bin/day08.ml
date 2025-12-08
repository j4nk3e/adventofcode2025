module IntMap = Map.Make (Int)

module CoordMap = Map.Make (struct
  type t = int * int * int

  let compare (x, y, z) (x', y', z') =
    match compare x x' with
    | 0 -> ( match compare y y' with 0 -> compare z z' | q -> q)
    | p -> p
end)

let rec read acc =
  try
    let line = input_line stdin in
    read (line :: acc)
  with End_of_file -> List.rev acc

let coord_of_list l =
  match l with [ x; y; z ] -> (x, y, z) | _ -> failwith "no 3 elements"

let c_diff a b =
  match (a, b) with
  | (x, y, z), (x', y', z') ->
      ((x' - x) * (x' - x)) + ((y' - y) * (y' - y)) + ((z' - z) * (z' - z))

let parse lines =
  List.map
    (fun l ->
      l |> String.split_on_char ',' |> List.map int_of_string |> coord_of_list)
    lines

let replace p q map =
  map |> CoordMap.to_seq
  |> Seq.map (fun (k, v) -> if v = p then (k, q) else (k, v))
  |> CoordMap.of_seq

let () =
  let coords = read [] |> parse in
  let pairs =
    List.map (fun a -> List.map (fun b -> (a, b, c_diff a b)) coords) coords
    |> List.flatten
    |> List.filter (fun (a, b, d) -> a > b && d > 0)
    |> List.sort (fun (_, _, a) (_, _, b) -> compare a b)
  in
  let map =
    pairs |> List.take 1000
    |> List.fold_left
         (fun map (l, r, _) ->
           match map |> CoordMap.find_opt l with
           | Some q -> (
               match map |> CoordMap.find_opt r with
               | Some p when p = q -> map |> CoordMap.add r q
               | Some p -> map |> replace p q
               | None -> map |> CoordMap.add r q)
           | None -> (
               match map |> CoordMap.find_opt r with
               | Some p -> map |> CoordMap.add l p
               | None ->
                   let hi = CoordMap.cardinal map in
                   map |> CoordMap.add l hi |> CoordMap.add r hi))
         CoordMap.empty
  in
  let _ =
    CoordMap.to_list map
    |> List.map (fun (_, b) -> b)
    |> List.sort compare |> List.to_seq
    |> Seq.group (fun a b -> a = b)
    |> Seq.map Seq.length |> List.of_seq |> List.sort compare |> List.rev
    |> List.take 3
    |> List.fold_left (fun a b -> a * b) 1
    |> Printf.printf "%d\n"
  in
  let _, conn =
    pairs
    |> List.fold_left
         (fun (map, conn) (l, r, _) ->
           match map |> CoordMap.find_opt l with
           | Some q -> (
               match map |> CoordMap.find_opt r with
               | Some p when p = q -> (map |> CoordMap.add r q, conn)
               | Some p ->
                   let x1, _, _ = l in
                   let x2, _, _ = r in
                   (map |> replace p q, (x1 * x2) :: conn)
               | None ->
                   let x1, _, _ = l in
                   let x2, _, _ = r in
                   (map |> CoordMap.add r q, (x1 * x2) :: conn))
           | None -> (
               match map |> CoordMap.find_opt r with
               | Some p ->
                   let x1, _, _ = l in
                   let x2, _, _ = r in
                   (map |> CoordMap.add l p, (x1 * x2) :: conn)
               | None ->
                   let hi = CoordMap.cardinal map in
                   (map |> CoordMap.add l hi |> CoordMap.add r hi, conn)))
         (CoordMap.empty, [])
  in
  conn |> List.hd |> Printf.printf "%d\n"
