open Aoc2025
module IntSet = Set.Make (Int)

let parse lines =
  List.hd lines |> String.split_on_char ','
  |> List.map (fun range ->
         match String.split_on_char '-' range with
         | [ min; max ] -> (int_of_string min, int_of_string max)
         | _ -> failwith "failed to parse")

let part1 data =
  let aux min max len =
    if len mod 2 <> 0 then 0
    else
      let half_len = len / 2 in
      let mask = Math.powi 10 half_len in
      let min =
        let min_len = 1 + Math.log10i min in
        if len = min_len then
          let x1 = min / mask in
          let x2 = min mod mask in
          if x2 > x1 then x1 + 1 else x1
        else mask / 10
      in
      let max =
        let max_len = 1 + Math.log10i max in
        if len = max_len then
          let x1 = max / mask in
          let x2 = max mod mask in
          if x2 < x1 then x1 - 1 else x1
        else mask - 1
      in
      if min > max then 0 else (1 + mask) * (Math.int_sum max - Math.int_sum (min - 1))
  in
  data
  |> List.map (fun (min, max) ->
         let min_len = 1 + Math.log10i min in
         let max_len = 1 + Math.log10i max in
         let f = aux min max in
         Range.inclusive min_len max_len
         |> Range.fold (fun acc i -> acc + f i) 0)
  |> List.fold_left ( + ) 0

let part2 data =
  let aux min max len sub_len =
    if len mod sub_len <> 0 then IntSet.empty
    else
      let count = len / sub_len in
      let mask = Math.powi 10 sub_len in
      let min =
        let min_len = 1 + Math.log10i min in
        if len = min_len then
          Range.exclusive 0 count
          |> Range.fold
               (fun (x, acc) _ ->
                 let sub = x mod mask in
                 let acc = if acc > sub then sub + 1 else sub in
                 (x / mask, acc))
               (min, 0)
          |> snd
        else mask / 10
      in
      let max =
        let max_len = 1 + Math.log10i max in
        if len = max_len then
          Range.exclusive 0 count
          |> Range.fold
               (fun (x, acc) _ ->
                 let sub = x mod mask in
                 let acc = if acc < sub then sub - 1 else sub in
                 (x / mask, acc))
               (max, mask - 1)
          |> snd
        else mask - 1
      in
      let mask =
        Range.exclusive 0 count |> Range.fold (fun acc _ -> (acc * mask) + 1) 0
      in
      Range.inclusive min max
      |> Range.fold
           (fun invalid_ids i -> IntSet.add (mask * i) invalid_ids)
           IntSet.empty
  in
  let invalid_ids =
    data
    |> List.map (fun (min, max) ->
           let min_len = 1 + Math.log10i min in
           let max_len = 1 + Math.log10i max in
           let f = aux min max in
           Range.inclusive min_len max_len
           |> Range.fold
                (fun invalid_ids len ->
                  let max_sub_len = len / 2 in
                  let f = f len in
                  let invalid_ids_for_sub_len =
                    Range.inclusive 1 max_sub_len
                    |> Range.fold
                         (fun acc sub_len -> IntSet.union acc (f sub_len))
                         IntSet.empty
                  in
                  IntSet.union invalid_ids invalid_ids_for_sub_len)
                IntSet.empty)
    |> List.fold_left IntSet.union IntSet.empty
  in
  IntSet.fold ( + ) invalid_ids 0

let () =
  let parsed = Io.read_file "./input/puzzle2.txt" |> parse in
  let part1 = part1 parsed in
  Printf.printf "part1 = %d\n" part1;
  let part2 = part2 parsed in
  Printf.printf "part2 = %d\n" part2
