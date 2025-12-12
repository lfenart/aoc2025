type 'a t = 'a Array.t Array.t
type index = int * int

let of_array arr = arr

let get t (x, y) =
  if x < 0 || x >= Array.length t then None
  else
    let line = Array.get t x in
    if y < 0 || y >= Array.length line then None else Some (Array.get line y)

let get_unsafe t key = get t key |> Option.get
let set t (x, y) v = Array.set (Array.get t x) y v
let iteri f = Array.iteri (fun i -> Array.iteri (fun j value -> f (i, j) value))

let foldi f acc t =
  let acc = ref acc in
  iteri (fun index value -> acc := f !acc index value) t;
  !acc

let neighbors t (x, y) =
  [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
  |> List.filter_map (fun (dx, dy) ->
         let x = x + dx in
         let y = y + dy in
         get t (x, y))
