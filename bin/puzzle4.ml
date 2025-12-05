open Aoc2025

let parse lines =
  lines
  |> List.map (fun lines ->
         String.to_seq lines |> Seq.map (fun c -> c = '@') |> Array.of_seq)
  |> Array.of_list |> Matrix.of_array

let max_neighbors = 4

let part1 matrix =
  Matrix.foldi
    (fun acc index value ->
      if not value then acc
      else
        let count =
          Matrix.neighbors matrix index
          |> List.fold_left (fun acc x -> acc + if x then 1 else 0) 0
        in
        if count < max_neighbors then acc + 1 else acc)
    0 matrix

let part2 matrix =
  let rec loop removed =
    let to_remove =
      Matrix.foldi
        (fun acc index value ->
          if not value then acc
          else
            let count =
              Matrix.neighbors matrix index
              |> List.fold_left (fun acc x -> acc + if x then 1 else 0) 0
            in
            if count < max_neighbors then index :: acc else acc)
        [] matrix
    in
    if List.is_empty to_remove then removed
    else (
      to_remove |> List.iter (fun key -> Matrix.set matrix key false);
      loop (removed + List.length to_remove))
  in
  loop 0

let () =
  let parsed = Io.read_file "./input/puzzle4.txt" |> parse in
  let part1 = part1 parsed in
  Printf.printf "part1 = %d\n" part1;
  let part2 = part2 parsed in
  Printf.printf "part2 = %d\n" part2
