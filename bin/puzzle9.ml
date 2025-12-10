open Aoc2025
module IntMap = Map.Make (Int)

module PointSet = Set.Make (struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    let dx = Int.compare x1 x2 in
    if dx = 0 then Int.compare y1 y2 else dx
end)

let parse lines =
  lines
  |> List.map (fun line ->
         line |> String.split_on_char ',' |> function
         | [ x; y ] -> (int_of_string x, int_of_string y)
         | _ -> failwith "parsing failed")

let areas data =
  let area (x1, y1) (x2, y2) =
    (Int.abs (x1 - x2) + 1) * (Int.abs (y1 - y2) + 1)
  in
  let rec loop lines acc =
    match lines with
    | [] | _ :: [] -> acc
    | x :: xs ->
        let acc = (xs |> List.map (fun y -> (x, y, area x y))) :: acc in
        loop xs acc
  in
  loop data [] |> List.flatten

let part1 data =
  areas data |> List.fold_left (fun acc (_, _, area) -> Int.max acc area) (-1)

let min_max x y = if x > y then (y, x) else (x, y)

let part2 data =
  let p0, data = (List.hd data, List.tl data) in
  let extract_lines ((x0, y0), hlines, vlines) (x, y) =
    let xmin, xmax = min_max x x0 in
    let ymin, ymax = min_max y y0 in
    let hlines, vlines =
      if x = x0 then ((xmin, ymin, ymax) :: hlines, vlines)
      else (hlines, (ymin, xmin, xmax) :: vlines)
    in
    ((x, y), hlines, vlines)
  in
  let r = data |> List.fold_left extract_lines (p0, [], []) in
  let _, hlines, vlines = extract_lines r p0 in
  let ranges =
    hlines
    |> List.fold_left
         (fun acc (x, ymin, ymax) ->
           Range.exclusive ymin ymax
           |> Range.fold
                (fun acc y ->
                  IntMap.update y
                    (function None -> Some [ x ] | Some xs -> Some (x :: xs))
                    acc)
                acc)
         IntMap.empty
  in
  let rec pairs values acc =
    match values with
    | [] -> acc
    | [ _ ] -> failwith "unreachable"
    | x :: y :: values -> pairs values ((x, y) :: acc)
  in
  let ranges =
    ranges
    |> IntMap.map (fun (value : int list) ->
           let value = value |> List.sort Int.compare in
           pairs value [] |> List.rev)
  in
  let ranges =
    vlines
    |> List.fold_left
         (fun acc (y, xmin, xmax) ->
           IntMap.update y
             (function
               | None -> Some [ (xmin, xmax) ]
               | Some ranges -> Some ((xmin, xmax) :: ranges))
             acc)
         ranges
  in
  let rec merge_ranges ranges (min0, max0) acc =
    match ranges with
    | [] -> (min0, max0) :: acc |> List.rev
    | (min1, max1) :: ranges ->
        if min1 > max0 + 1 then
          merge_ranges ranges (min1, max1) ((min0, max0) :: acc)
        else merge_ranges ranges (min0, Int.max max0 max1) acc
  in
  let ranges =
    ranges
    |> IntMap.map (fun value ->
           let value =
             value |> List.sort (fun (min0, _) (min1, _) -> min0 - min1)
           in
           match value with [] -> [] | hd :: tl -> merge_ranges tl hd [])
  in
  let areas =
    areas data |> List.sort (fun (_, _, area1) (_, _, area2) -> area2 - area1)
  in
  let is_inside_red_and_green xmin xmax ymin ymax =
    let rec loop y =
      if y > ymax then true
      else
        let ranges = try IntMap.find y ranges with Not_found -> [] in
        let rec aux = function
          | [] -> false
          | (min, max) :: ranges ->
              if xmin < min then false
              else if xmin > max then aux ranges
              else if xmax > max then false
              else true
        in
        if aux ranges then loop (y + 1) else false
    in
    loop ymin
  in
  let rec greatest_area_in_red_and_green areas =
    match areas with
    | [] -> failwith "unreachable"
    | ((x1, y1), (x2, y2), area) :: areas ->
        let xmin, xmax = min_max x1 x2 in
        let ymin, ymax = min_max y1 y2 in
        if is_inside_red_and_green xmin xmax ymin ymax then area
        else greatest_area_in_red_and_green areas
  in
  greatest_area_in_red_and_green areas

let () =
  let parsed = Io.read_file "./input/puzzle9.txt" |> parse in
  let part1 = part1 parsed in
  Printf.printf "part1 = %d\n" part1;
  let part2 = part2 parsed in
  Printf.printf "part2 = %d\n" part2
