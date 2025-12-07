open Aoc2025
module IntSet = Set.Make (Int)

module IntIntMap = Map.Make (struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    match Int.compare x1 x2 with 0 -> Int.compare y1 y2 | x -> x
end)

let parse lines =
  match lines with
  | [] -> failwith "parsing failed"
  | head :: lines ->
      let start_pos = String.index head 'S' in
      let lines =
        lines
        |> List.map (fun line ->
               line
               |> String.fold_left
                    (fun (i, acc) x ->
                      match x with
                      | '.' -> (i + 1, acc)
                      | '^' -> (i + 1, IntSet.add i acc)
                      | _ -> failwith "parsing failed")
                    (0, IntSet.empty))
      in
      (start_pos, lines)

let part1 (start_pos, lines) =
  lines
  |> List.fold_left
       (fun (count, rays) (len, line) ->
         let acc = (count, IntSet.empty) in
         IntSet.fold
           (fun ray (count, new_rays) ->
             if IntSet.exists (( = ) ray) line then
               let new_rays =
                 if ray > 0 then IntSet.add (ray - 1) new_rays else new_rays
               in
               let new_rays =
                 if ray + 1 < len then IntSet.add (ray + 1) new_rays
                 else new_rays
               in
               (count + 1, new_rays)
             else (count, IntSet.add ray new_rays))
           rays acc)
       (0, IntSet.add start_pos IntSet.empty)
  |> fst

let memo_rec extract_key f =
  let h = Hashtbl.create 16 in
  let rec self x =
    let k = extract_key x in
    match Hashtbl.find_opt h k with
    | Some y -> y
    | None ->
        let y = f self x in
        Hashtbl.add h k y;
        y
  in
  self

let part2 (start_pos, lines) =
  let loop_memo =
    let loop self (lines, start_pos, i) =
      match lines with
      | [] -> 1
      | (len, line) :: lines ->
          if IntSet.exists (( = ) start_pos) line then
            let count =
              if start_pos > 0 then self (lines, start_pos - 1, i + 1) else 0
            in
            let count2 =
              if start_pos + 1 < len then self (lines, start_pos + 1, i + 1)
              else 0
            in
            count + count2
          else self (lines, start_pos, i + 1)
    in
    memo_rec (fun (_, start_pos, i) -> (start_pos, i)) loop
  in
  loop_memo (lines, start_pos, 0)

let () =
  let parsed = Io.read_file "./input/puzzle7.txt" |> parse in
  let part1 = part1 parsed in
  Printf.printf "part1 = %d\n" part1;
  let part2 = part2 parsed in
  Printf.printf "part2 = %d\n" part2
