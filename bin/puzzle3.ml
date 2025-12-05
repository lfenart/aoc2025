open Aoc2025

let parse lines =
  let zero = int_of_char '0' in
  lines
  |> List.map (fun line ->
         String.to_seq line
         |> Seq.map (fun c -> int_of_char c - zero)
         |> List.of_seq)

let part1 data =
  data
  |> List.map (fun line ->
         let rec loop xs (a, b) =
           match xs with
           | [] -> (10 * a) + b
           | [ y ] -> loop [] (a, max y b)
           | x :: y :: xs ->
               loop (y :: xs)
                 (if x > a then (x, y) else if y > b then (a, y) else (a, b))
         in
         loop line (0, 0))
  |> List.fold_left ( + ) 0

let n = 12

let part2 data =
  data
  |> List.map (fun line ->
         let len = List.length line in
         let _, _, numbers =
           Range.exclusive 0 n
           |> Range.fold
                (fun (line, len, acc) i ->
                  let max_index = len + i + 2 - n in
                  let _, max_i, max =
                    line
                    |> Lists.fold_max max_index
                         (fun (i, max_i, max) x ->
                           if x > max then (i + 1, i, x) else (i + 1, max_i, max))
                         (0, -1, -1)
                  in
                  (List.drop (max_i + 1) line, len - max_i - 1, max :: acc))
                (line, len, [])
         in
         List.rev numbers |> List.fold_left (fun acc x -> (acc * 10) + x) 0)
  |> List.fold_left ( + ) 0

let () =
  let parsed = Io.read_file "./input/puzzle3.txt" |> parse in
  let part1 = part1 parsed in
  Printf.printf "part1 = %d\n" part1;
  let part2 = part2 parsed in
  Printf.printf "part2 = %d\n" part2
