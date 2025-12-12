open Aoc2025
module StringMap = Map.Make (String)

module S = Map.Make (struct
  type t = string * bool * bool

  let compare = Stdlib.compare
end)

let parse lines =
  lines
  |> List.map (fun line ->
         line |> String.split_on_char ':' |> function
         | [ from; tos ] ->
             (from, tos |> String.trim |> String.split_on_char ' ')
         | _ -> failwith "parsing failed")

let part1 data =
  let map =
    data
    |> List.fold_left
         (fun acc (from, tos) -> acc |> StringMap.add from tos)
         StringMap.empty
  in
  let rec loop s acc =
    if s = "out" then acc + 1
    else
      let tos = map |> StringMap.find s in
      tos |> List.fold_left (fun acc t -> loop t acc) acc
  in
  loop "you" 0

let part2 data =
  let map =
    data
    |> List.fold_left
         (fun acc (from, tos) -> acc |> StringMap.add from tos)
         StringMap.empty
  in
  let rec loop s dac fft memo =
    match memo |> S.find_opt (s, dac, fft) with
    | Some v -> (memo, v)
    | None ->
        if s = "out" then if dac && fft then (memo, 1) else (memo, 0)
        else
          let dac = dac || s = "dac" in
          let fft = fft || s = "fft" in
          let tos = map |> StringMap.find s in
          let memo, v =
            tos
            |> List.fold_left
                 (fun (memo, acc) t ->
                   let memo, x = loop t dac fft memo in
                   (memo, x + acc))
                 (memo, 0)
          in
          let memo = memo |> S.add (s, dac, fft) v in
          (memo, v)
  in
  loop "svr" false false S.empty |> snd

let () =
  let parsed = Io.read_file "./input/puzzle11.txt" |> parse in
  let part1 = part1 parsed in
  Printf.printf "part1 = %d\n" part1;
  let part2 = part2 parsed in
  Printf.printf "part2 = %d\n" part2
