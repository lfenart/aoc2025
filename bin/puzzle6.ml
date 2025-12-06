open Aoc2025

type op = Mult | Add

let parse lines =
  let lines =
    lines
    |> List.map (fun line ->
           line |> String.split_on_char ' ' |> List.filter (fun s -> s <> ""))
  in
  match List.rev lines with
  | [] -> failwith "parsing failed"
  | ops :: lines ->
      let ops =
        ops
        |> List.map (fun x ->
               match x with
               | "*" -> Mult
               | "+" -> Add
               | _ -> failwith "parsing failed")
      in
      let lines =
        lines |> List.map (fun line -> line |> List.map int_of_string)
      in
      (ops, lines)

let part1 (ops, lines) =
  let cols = Lists.transpose lines in
  List.combine ops cols
  |> List.map (fun (op, col) ->
         let f, id = match op with Mult -> (( * ), 1) | Add -> (( + ), 0) in
         List.fold_left f id col)
  |> List.fold_left ( + ) 0

let parse2 lines =
  match lines |> List.rev with
  | [] -> failwith "parsing failed"
  | ops :: lines ->
      let lines = lines |> List.rev in
      let ops_len, ops =
        ops |> String.to_seq
        |> Seq.fold_left
             (fun (i, acc) c ->
               match c with
               | '*' -> (i + 1, (i, Mult) :: acc)
               | '+' -> (i + 1, (i, Add) :: acc)
               | ' ' -> (i + 1, acc)
               | _ -> failwith "parsing failed")
             (0, [])
      in
      ops
      |> List.fold_left
           (fun (j, acc) (i, op) ->
             let lines =
               lines
               |> List.map (fun line ->
                      String.sub line i (j - i) |> String.to_seq |> List.of_seq)
               |> Lists.transpose
               |> List.map (fun line ->
                      line |> List.to_seq |> String.of_seq |> String.trim
                      |> int_of_string)
             in
             (i - 1, (op, lines) :: acc))
           (ops_len, [])
      |> snd

let part2 data =
  data
  |> List.map (fun (op, xs) ->
         let f, id = match op with Mult -> (( * ), 1) | Add -> (( + ), 0) in
         xs |> List.fold_left f id)
  |> List.fold_left ( + ) 0

let () =
  let raw = Io.read_file "./input/puzzle6.txt" in
  let part1 = part1 (parse raw) in
  Printf.printf "part1 = %d\n" part1;
  let part2 = part2 (parse2 raw) in
  Printf.printf "part2 = %d\n" part2
