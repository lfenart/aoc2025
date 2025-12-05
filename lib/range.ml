type t = { min : int; max : int; inclusive : bool }

let inclusive min max = { min; max; inclusive = true }
let exclusive min max = { min; max; inclusive = false }

let fold f acc t =
  let rec loop i acc =
    let break =
      match t.inclusive with true -> i > t.max | false -> i >= t.max
    in
    if break then acc
    else
      let acc = f acc i in
      loop (i + 1) acc
  in
  loop t.min acc
