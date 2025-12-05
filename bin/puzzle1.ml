open Aoc2025

type direction = Left | Right

let parse lines =
  lines
  |> List.map (fun line ->
         let head, tail = Lists.split_at line 1 in
         let direction =
           match head.[0] with
           | 'L' -> Left
           | 'R' -> Right
           | _ -> failwith "failed to parse"
         in
         (direction, int_of_string tail))

let dial_start = 50
let dial_len = 100

let part1 data =
  data
  |> List.fold_left
       (fun (dial, count) (direction, value) ->
         let value = match direction with Left -> -value | Right -> value in
         let dial = dial + value in
         let dial = Math.floor_mod dial dial_len in
         let count = if dial = 0 then count + 1 else count in
         (dial, count))
       (dial_start, 0)
  |> snd

let part2 data =
  data
  |> List.fold_left
       (fun (dial, count) (direction, value) ->
         match direction with
         | Left ->
             let new_dial = dial - value in
             let count =
               count
               + Math.floor_div (dial - 1) dial_len
               - Math.floor_div (new_dial - 1) dial_len
             in
             (Math.floor_mod new_dial dial_len, count)
         | Right ->
             let new_dial = dial + value in
             let count =
               count
               + Math.floor_div new_dial dial_len
               - Math.floor_div dial dial_len
             in
             (Math.floor_mod new_dial dial_len, count))
       (dial_start, 0)
  |> snd

let () =
  let parsed = Io.read_file "./input/puzzle1.txt" |> parse in
  let part1 = part1 parsed in
  Printf.printf "part1 = %d\n" part1;
  let part2 = part2 parsed in
  Printf.printf "part2 = %d\n" part2
