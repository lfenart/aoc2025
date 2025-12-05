type t

val inclusive : int -> int -> t
val exclusive : int -> int -> t
val fold : ('acc -> int -> 'acc) -> 'acc -> t -> 'acc
