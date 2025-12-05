type 'a t
type index = int * int

val of_array : 'a Array.t Array.t -> 'a t
val get : 'a t -> index -> 'a option
val get_unsafe : 'a t -> index -> 'a
val set : 'a t -> index -> 'a -> unit
val iteri : (index -> 'a -> unit) -> 'a t -> unit
val foldi : ('acc -> index -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val neighbors : 'a t -> index -> 'a list
