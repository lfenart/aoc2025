open Aoc2025
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

module S = Set.Make (struct
  type t = int * int array

  let compare = Stdlib.compare
end)

let parse lines =
  lines
  |> List.map (fun line ->
         let tokens = line |> String.split_on_char ' ' in
         match tokens with
         | [] -> failwith "parsing failed"
         | indicators :: rest -> (
             let indicators =
               String.sub indicators 1 (String.length indicators - 2)
               |> String.to_seq
               |> Seq.map (function
                    | '#' -> true
                    | '.' -> false
                    | _ -> failwith "parsing failed")
               |> List.of_seq
             in
             match List.rev rest with
             | [] -> failwith "parsing failed"
             | joltage :: buttons ->
                 let joltage =
                   String.sub joltage 1 (String.length joltage - 2)
                   |> String.split_on_char ',' |> List.map int_of_string
                 in
                 let buttons =
                   buttons
                   |> List.map (fun button ->
                          String.sub button 1 (String.length button - 2)
                          |> String.split_on_char ',' |> List.map int_of_string)
                 in
                 (indicators, buttons, joltage)))

let part1 data =
  let f indicators buttons =
    let queue = Queue.create () in
    Queue.add (indicators, 0) queue;
    let rec loop visited =
      let state, n = Queue.take queue in
      if state = 0 then n
      else if IntSet.find_opt state visited |> Option.is_some then loop visited
      else
        let visited = IntSet.add state visited in
        buttons
        |> List.iter (fun button ->
               let new_state = state lxor button in
               Queue.add (new_state, n + 1) queue);
        loop visited
    in
    loop IntSet.empty
  in
  let indicators_to_int indicators =
    let rec loop indicators acc =
      match indicators with
      | [] -> acc
      | x :: xs -> loop xs ((acc * 2) + if x then 1 else 0)
    in
    loop (List.rev indicators) 0
  in
  let button_to_int button =
    button |> List.fold_left (fun acc x -> acc + Int.shift_left 1 x) 0
  in
  data
  |> List.map (fun (indicators, buttons, _) ->
         f (indicators_to_int indicators) (buttons |> List.map button_to_int))
  |> List.fold_left ( + ) 0

let () =
  let parsed = Io.read_file "./input/puzzle10.txt" |> parse in
  let part1 = part1 parsed in
  Printf.printf "part1 = %d\n" part1
