module StringMap = Map.Make (struct
  type t = string

  let compare = compare
end)

let rec read acc =
  try
    let line = input_line stdin in
    read (line :: acc)
  with End_of_file -> List.rev acc

let parse lines =
  List.map
    (fun l ->
      match String.split_on_char ':' l with
      | [ a; b ] ->
          let b = String.trim b |> String.split_on_char ' ' in
          (a, b)
      | _ -> failwith "no match")
    lines
  |> StringMap.of_list

let rec paths count map goal = function
  | [] -> count
  | states ->
      let next =
        states
        |> List.map (fun (k, v) ->
            match StringMap.find_opt k map with
            | Some q -> List.map (fun r -> (r, v)) q
            | None -> [])
        |> List.flatten
      in
      let l, r = List.partition (fun (k, _v) -> k = goal) next in
      let count = List.fold_left (fun acc (_k, v) -> acc + v) count l in
      let states =
        List.fold_left
          (fun acc (k, v) ->
            StringMap.update k
              (function Some v' -> Some (v + v') | None -> Some v)
              acc)
          StringMap.empty r
      in
      paths count map goal (StringMap.to_list states)

let () =
  let map = read [] |> parse in
  let ___ = paths 0 map "out" [ ("you", 1) ] |> Printf.printf "%d\n" in
  let svr = paths 0 map "fft" [ ("svr", 1) ] in
  let fft = paths 0 map "dac" [ ("fft", 1) ] in
  let dac = paths 0 map "out" [ ("dac", 1) ] in
  Printf.printf "%d\n" (dac * fft * svr)
