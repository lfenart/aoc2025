let split_at s i =
  let head = String.sub s 0 i in
  let tail = String.sub s i (String.length s - i) in
  (head, tail)

let fold_max count f =
  let rec loop i acc l =
    match (i, l) with
    | 0, _ | _, [] -> acc
    | _, x :: xs ->
        let acc = f acc x in
        loop (i - 1) acc xs
  in
  loop (count - 1)
