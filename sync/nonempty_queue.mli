@@ portable

type ('a : value mod contended portable) t : value mod contended portable

val singleton : 'a -> 'a t
val enqueue : 'a -> 'a t -> 'a t
val dequeue : 'a t -> #('a * 'a t or_null)

exception Not_found

val reject_exn : 'a -> 'a t -> 'a t or_null
val iter : f:('a -> unit) @ local -> 'a t -> unit
