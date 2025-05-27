@@ portable

(** A non-blocking bitset. *)
type t : value mod contended portable

(** [create len] creates an atomic bitset of the given length. *)
val create : int -> t

(** [get t idx] is the value of the bit at [idx].

    This operation is wait-free. *)
val get : t -> int -> bool

(** [set t idx v] sets the bit at [idx] to [v].

    This operation is wait-free. *)
val set : t -> int -> bool -> unit

(** [non_linearizable_pop t] tries to find a bit in [t] set to [true], sets the bit to
    [false], and returns the index of the bit or [Null] in case no such bit was found.

    This operation may return [Null] even when the bitset actually contained a bit set to
    [true]. In case some index is returned, the corresponding bit was cleared atomically.

    This operation is lock-free. *)
val non_linearizable_pop : t -> int or_null
