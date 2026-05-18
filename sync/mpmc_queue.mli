@@ portable

(** A multi-producer, multi-consumer concurrent queue. *)

open Base
open Await_kernel

(** A blocking multi-producer multi-consumer queue backed by a {!Portable_mpmc_queue.t}. *)
type !'a t : value mod contended non_float portable

(** [create ()] creates and returns a new empty queue.

    The optional [padded] argument specifies whether to pad the data structure to avoid
    false sharing. See {!Atomic.make} for a longer explanation. *)
val create : ?padded:bool @ local -> unit -> 'a t

(** [push t a] enqueues [a] at the tail of [t]. *)
val push : 'a t @ local -> 'a @ contended portable -> unit

(** [pop await t] removes and returns the value at the head of [t], blocking using [await]
    if [t] is empty. *)
val pop : Await.t @ local -> 'a t @ local -> 'a @ contended portable

(** [pop_or_cancel await c t] is [Completed (pop await t)] if [c] is not cancelled,
    otherwise it is [Canceled].

    @raise Terminated if [await] is terminated, even if [c] is canceled. *)
val pop_or_cancel
  :  Await.t @ local
  -> Cancellation.t @ local
  -> 'a t @ local
  -> 'a Or_canceled.t @ contended portable

(** [pop_nonblocking t] removes and returns [This v] where [v] is the value at the head of
    [t], or returns [Null] if the queue is empty. *)
val pop_nonblocking : 'a t @ local -> 'a or_null @ contended portable

(** [peek t] is the next value that will be [pop]ped from the queue, or [Null] if the
    queue is empty.

    Note that it is possible that [pop] returns a different value after [peek] is called
    if another thread modifies the queue in the interim. *)
val peek : 'a t -> 'a or_null @ contended portable

(** [length t] is the number of values in the queue [t]. *)
val length : 'a t @ local -> int

module For_testing : sig
  (** [length t] returns an upper bound on the length of the internal queue of awaiters
      for testing purposes. *)
  val length : 'a t @ local -> int
end
