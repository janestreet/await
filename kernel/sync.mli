@@ portable

open! Base

(** Abstract implementation of {i bounded} awaiting.

    Operations which might need to synchronize with other threads for a {i bounded} amount
    of time, eg to acquire a lock, take a {{!t} [Sync.t]} which provides an implementation
    of synchronization for them to use.

    Usually, you should use the {{!t} [Sync.t]} provided by a parallelism or concurrency
    scheduler, but if you don't have one available you can use [Sync_blocking.sync]
    instead. *)

(** Represents the capability to block for a {i bounded} amount of time. *)
type t : value mod contended non_float portable

(** [with_ ~yield ~sync ctx ~f] calls [f] with an implementation of synchronization such
    that [sync ctx t trigger] is its implementation of [sync], and [yield ctx t] is its
    implementation of [yield]. *)
val with_
  : 'c ('r : value_or_null).
  yield:('c @ local -> unit) or_null @ local
  -> sync:(#('c * Trigger.t global) @ local -> unit) @ local
  -> 'c @ local
  -> f:(t @ local -> 'r @ forkable local once unique) @ local once
  -> 'r @ forkable local once unique

(** [create ~yield ~sync ctx] is a new implementation of synchronization such that
    [sync ctx t trigger] is its implementation of [sync], and [yield ctx t] is its
    implementation of [yield]. *)
val%template create
  :  yield:('c @ contended local portable -> unit) or_null @ l portable
  -> sync:(#('c portended * Trigger.t global) @ local -> unit) @ l portable
  -> 'c @ contended l portable
  -> t @ l
[@@alloc a @ l = (stack_local, heap_global)]

(** [yield t] uses [t] to yield to the scheduler using the implementation of yielding
    associated with [t]. *)
val yield : t @ local -> unit

module For_testing : sig
  (** [never] is an implementation of synchronizing that should never be used. If [sync]
      is called with the implementation, it will raise.

      This is useful in tests, for testing operations which otherwise might conditionally
      block, in a single-threaded manner that never needs to block - or, to test that a
      blocking operation always blocks, eg using
      [Expect_test_helpers_base.require_does_raise]

      Bear in mind that proper implementations of [sync] do not usually raise and are not
      documented to potentially raise. This means that abstractions built on sync may e.g.
      leave the program in an invalid state when using [never]. *)
  val never : t
end

(**/**)

module Expert : sig
  (** [sync t ~on:trigger] blocks the current thread until the [trigger] is signaled.

      This function should not be used to block on triggers that might not be signalled
      for an unbounded period of time, such as triggers that are signaled when another
      thread exits. Instead, this function should only be used to block on triggers where
      there is some guarantee that the trigger will be signalled in a bounded period of
      time, such as triggers for acquiring locks where the critical section is guaranteed
      to finish within a bounded period of time. *)
  val sync : t @ local -> on:Trigger.t -> unit

  (** [sync_without_checking_trigger] is like [sync], except it doesn't first check
      whether [trigger] has already been signaled. This is an optimization for the case
      where calling code has already performed such a check. *)
  val sync_without_checking_trigger : t @ local -> on:Trigger.t -> unit

  (** [sync_or_cancel t cancellation ~on:trigger] is like [sync t ~on:trigger], except it
      will also return if the cancellation token [cancellation] is cancelled *)
  val sync_or_cancel : t @ local -> Cancellation.t @ local -> on:Trigger.t -> unit
end
