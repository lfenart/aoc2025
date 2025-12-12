open Aoc2025
module IntMap = Map.Make (Int)

let parse lines =
  let parse_shape line lines =
    let index = String.sub line 0 (String.length line - 1) |> int_of_string in
    let rec loop lines acc =
      match lines with
      | [] -> (List.rev acc, [])
      | "" :: lines -> (List.rev acc, lines)
      | line :: lines ->
          let line =
            line |> String.to_seq |> List.of_seq
            |> List.map (function
                 | '#' -> true
                 | '.' -> false
                 | _ -> failwith "parsing failed")
          in
          loop lines (line :: acc)
    in
    let shape, lines = loop lines [] in
    (index, shape, lines)
  in
  let rec loop lines b (shapes, acc) =
    match lines with
    | [] -> (shapes, List.rev acc)
    | line :: lines -> (
        if (not b) && line |> String.ends_with ~suffix:":" then
          let index, shape, lines = parse_shape line lines in
          loop lines false ((index, shape) :: shapes, acc)
        else
          match line |> String.split_on_char ':' with
          | [ a; b ] ->
              let a =
                match a |> String.split_on_char 'x' with
                | [ x; y ] -> (int_of_string x, int_of_string y)
                | _ -> failwith "parsing failed"
              in
              let b =
                b |> String.trim |> String.split_on_char ' '
                |> List.map int_of_string
              in
              loop lines true (shapes, (a, b) :: acc)
          | _ -> failwith "parsing failed")
  in
  loop lines false ([], [])

let part1 (_, line) =
  line
  |> List.fold_left
       (fun acc ((x, y), counts) ->
         acc
         + if counts |> List.fold_left ( + ) 0 <= x / 3 * (y / 3) then 1 else 0)
       0

let part2 _data = 0

let () =
  let parsed = Io.read_file "./input/puzzle12.txt" |> parse in
  let part1 = part1 parsed in
  Printf.printf "part1 = %d\n" part1;
  let part2 = part2 parsed in
  Printf.printf "part2 = %d\n" part2
