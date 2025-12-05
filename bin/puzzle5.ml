open Aoc2025

let parse lines =
  let _, l1, l2 =
    lines
    |> List.fold_left
         (fun (b, l1, l2) line ->
           if b then (true, l1, int_of_string line :: l2)
           else if String.length line = 0 then (true, l1, [])
           else
             let el =
               match String.split_on_char '-' line with
               | [ x; y ] -> (int_of_string x, int_of_string y)
               | _ -> failwith "unreachable"
             in
             (false, el :: l1, []))
         (false, [], [])
  in
  (List.rev l1, List.rev l2)

let part1 (ranges, ingredients) =
  ingredients
  |> List.map (fun x ->
         let is_in_a_range =
           ranges
           |> List.fold_left
                (fun acc (min, max) ->
                  if acc then true else x >= min && x <= max)
                false
         in
         if is_in_a_range then 1 else 0)
  |> List.fold_left ( + ) 0

let part2 (ranges, _) =
  let sorted_ranges =
    List.sort (fun (min1, _) (min2, _) -> min1 - min2) ranges
  in
  match sorted_ranges with
  | [] -> 0
  | head :: tail ->
      let ranges, last =
        List.fold_left
          (fun (ranges, (current_min, current_max)) (min, max) ->
            if min <= current_max + 1 then
              if max > current_max then (ranges, (current_min, max))
              else (ranges, (current_min, current_max))
            else ((current_min, current_max) :: ranges, (min, max)))
          ([], head) tail
      in
      last :: ranges
      |> List.map (fun (min, max) -> max - min + 1)
      |> List.fold_left ( + ) 0

let () =
  let parsed = Io.read_file "./input/puzzle5.txt" |> parse in
  let part1 = part1 parsed in
  Printf.printf "part1 = %d\n" part1;
  let part2 = part2 parsed in
  Printf.printf "part2 = %d\n" part2
