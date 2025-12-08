open Aoc2025
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)
module IntSetSet = Set.Make (IntSet)

let parse lines =
  lines
  |> List.map (fun line ->
         line |> String.split_on_char ',' |> function
         | [ x; y; z ] -> (int_of_string x, int_of_string y, int_of_string z)
         | _ -> failwith "parsing failed")

let sorted_pairs data =
  let dist2 (x1, y1, z1) (x2, y2, z2) =
    let x = x1 - x2 in
    let y = y1 - y2 in
    let z = z1 - z2 in
    (x * x) + (y * y) + (z * z)
  in
  let rec loop lines i acc =
    match lines with
    | [] | _ :: [] -> acc
    | x :: xs ->
        let acc =
          (xs |> List.mapi (fun j y -> (i, j + i + 1, dist2 x y))) :: acc
        in
        loop xs (i + 1) acc
  in
  loop data 0 [] |> List.flatten
  |> List.sort (fun (_, _, k1) (_, _, k2) -> k1 - k2)

let n = 1000

let part1 data =
  let map =
    sorted_pairs data |> List.take n
    |> List.fold_left
         (fun map (i, j, _) ->
           let group =
             match (IntMap.find_opt i map, IntMap.find_opt j map) with
             | None, None -> IntSet.of_list [ i; j ]
             | None, Some group -> IntSet.add i group
             | Some group, None -> IntSet.add j group
             | Some group1, Some group2 -> IntSet.union group1 group2
           in
           IntSet.fold (fun x acc -> IntMap.add x group acc) group map)
         IntMap.empty
  in
  let set =
    IntMap.fold (fun _ value acc -> IntSetSet.add value acc) map IntSetSet.empty
  in
  let a, b, c =
    IntSetSet.fold
      (fun value (a, b, c) ->
        let len = IntSet.cardinal value in
        if len > a then (len, a, b)
        else if len > b then (a, len, b)
        else if len > c then (a, b, len)
        else (a, b, c))
      set (1, 1, 1)
  in
  a * b * c

let part2 data =
  let len = List.length data in
  let pairs = sorted_pairs data in
  let rec loop map list =
    match list with
    | [] -> failwith "unreachable"
    | (i, j, _) :: xs ->
        let group =
          match (IntMap.find_opt i map, IntMap.find_opt j map) with
          | None, None -> IntSet.of_list [ i; j ]
          | None, Some group -> IntSet.add i group
          | Some group, None -> IntSet.add j group
          | Some group1, Some group2 -> IntSet.union group1 group2
        in
        if IntSet.cardinal group = len then (i, j)
        else
          let map =
            IntSet.fold (fun x acc -> IntMap.add x group acc) group map
          in
          loop map xs
  in
  let i, j = loop IntMap.empty pairs in
  let x1, _, _ = List.nth data i in
  let x2, _, _ = List.nth data j in
  x1 * x2

let () =
  let parsed = Io.read_file "./input/puzzle8.txt" |> parse in
  let part1 = part1 parsed in
  Printf.printf "part1 = %d\n" part1;
  let part2 = part2 parsed in
  Printf.printf "part2 = %d\n" part2
