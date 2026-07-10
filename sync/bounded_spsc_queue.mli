@@ portable

(** A bounded, single-producer single-consumer queue implemented via a ring buffer.

    A [Bounded_spsc_queue.t] has a "producer" end and a "consumer" end, which can be
    (though do not have to be) in different capsules. The queue internally has a
    {!capacity}; messages written into the queue will only be buffered up to that
    capacity, after which {!push} will block using the provided implementation of awaiting
    until {!pop} is called on the corresponding consumer. *)

open! Base
open! Import

type ('a : value mod non_float, 'phantom) t : value mod non_float portable
type ('a : value mod non_float, 'phantom) queue = ('a, 'phantom) t

(** {2 Producer and Consumer} *)

(** These two modules provide producer- and consumer-specific types for the base queue
    type. *)

module Producer : sig
  type 'a t = ('a, [ `Producer ]) queue
end

module Consumer : sig
  type 'a t = ('a, [ `Consumer ]) queue
end

(** {2 Creating new queues} *)

(** [create ~capacity] creates a new bounded single-producer single-consumer queue and
    returns an unboxed pair of a producer and consumer for the queue, each in a separate
    owned capsule. *)
val create
  :  capacity:int
  -> #('a Producer.t Capsule.Owned.t * 'a Consumer.t Capsule.Owned.t) @ unique

(** {2 Writing} *)

(** [push w producer v] pushes [v] to the write end of the queue backing [producer]. If
    the queue is at capacity, [push] will use [w] to block until [pop] is called on the
    corresponding consumer. *)
val push
  :  Await.t @ local
  -> 'a Producer.t @ local
  -> 'a @ contended once portable unique
  -> unit

(** [push_or_cancel w c producer v] is [Completed (push w producer v)] if [c] is not
    cancelled, or [Cancelled] otherwise. *)
val push_or_cancel
  :  Await.t @ local
  -> Cancellation.t @ local
  -> 'a Producer.t @ local
  -> 'a @ contended once portable unique
  -> unit Or_canceled.t

(** {2 Reading} *)

(** [pop w consumer] removes and returns the {i oldest} [push]ed value from the queue
    backing [consumer]. If the queue is empty, [pop] will use [w] to block until [push] is
    called on the corresponding producer. *)
val pop : Await.t @ local -> 'a Consumer.t @ local -> 'a @ contended once portable unique

(** [pop_or_cancel w c consumer] is [Completed (pop w consumer)] if [c] is not cancelled,
    or [Cancelled] otherwise. *)
val pop_or_cancel
  :  Await.t @ local
  -> Cancellation.t @ local
  -> 'a Consumer.t @ local
  -> 'a Or_canceled.t @ contended once portable unique

(** {2 Metadata on queues} *)

(** [capacity t] is the capacity of the queue [t]. Takes either a producer or a consumer. *)
val capacity : (_, _) t @ contended local -> int

(** [equal t1 t2] is [true] if [t1] and [t2] refer to the same physical queue. *)
val equal : ('a, _) t @ contended local -> ('a, _) t @ contended local -> bool
