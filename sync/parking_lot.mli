@@ portable

open Import

module Random_key : sig
  type t = int
end

(** Add a new key to the parking lot. Returns [true] if the key was in fact new (and did
    not previously exist in the table) *)
val add_new : Random_key.t -> Trigger.Source.t -> bool

val find : Random_key.t -> Trigger.Source.t Nonempty_queue.t or_null

val compare_and_set
  :  Random_key.t
  -> if_phys_equal_to:Trigger.Source.t Nonempty_queue.t
  -> replace_with:Trigger.Source.t Nonempty_queue.t
  -> Atomic.Compare_failed_or_set_here.t

(** Remove a key from the parking lot, iff its queue is currently the same queue as
    [if_phys_equal_to]. Returns [true] if the key was removed. *)
val compare_remove
  :  Random_key.t
  -> if_phys_equal_to:Trigger.Source.t Nonempty_queue.t
  -> Atomic.Compare_failed_or_set_here.t

val remove : Random_key.t -> Trigger.Source.t Nonempty_queue.t or_null

module Drop : sig
  type t =
    | Empty
    | Dropped
    | Not_found
end

module For_testing : sig
  val non_linearizable_length : unit -> int
end
