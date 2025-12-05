let floor_mod x y =
  let a = x mod y in
  if a < 0 then a + y else a

let floor_div x y =
  let a = x / y in
  if a * y > x then a - 1 else a

let powi x y =
  let rec aux x y acc =
    match y with
    | 0 -> acc
    | y when y land 1 = 0 -> aux (x * x) (y / 2) acc
    | y -> aux (x * x) ((y - 1) / 2) (x * acc)
  in
  aux x y 1

let logni n x =
  let rec aux x acc = if x < n then acc else aux (x / n) (acc + 1) in
  aux x 0

let log10i = logni 10

let int_sum max = (max * (max + 1) / 2)
