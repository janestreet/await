open! Base
open! Import
module Capsule = Capsule.Prim

module type S = sig @@ portable
  (** Condition variable for waiting for changes to state protected by a lock. *)

  type 'k lock

  (** ['k t] is the type of a condition variable associated with the capsule ['k]. This
      condition may only be used with the matching ['k lock]. *)
  type 'k t : value mod contended forkable many portable unyielding

  module Wait : sig
    (** A value of type ['k Condition.Wait.t] provides the local ability to wait on a
        condition variable associated with the capsule ['k] *)
    type 'k t : value mod non_float portable with 'k lock

    (** [release_temporarily cw key ~f] temporarily releases the lock associated with
        [cw], runs [f] with the lock {i not} held, re-acquires the lock, and then returns
        the result of [f] along with the key for the capsule.

        If [f] raises an exception, the lock will {i not} be poisoned.

        @raise Poisoned if [t] cannot be reacquired because it is otherwise poisoned.
        @raise Frozen if [t] cannot be reacquired because it is frozen.
        @raise Terminated if [w] is terminated before the lock is reacquired. *)
    val%template release_temporarily
      :  'k t @ local
      -> 'k Capsule.Key.t @ unique
      -> f:(Await.t @ local -> 'a @ l once unique) @ local once
      -> #('a * 'k Capsule.Key.t) @ l once unique
    [@@mode l = (global, local)]
  end

  (** [create ()] creates a new condition variable associated with the matching ['k lock]
      and with a certain property {i P} that is protected by the lock.

      The optional [padded] argument specifies whether to pad the data structure to avoid
      false sharing. See {!Atomic.make} for a longer explanation. *)
  val create : ?padded:bool @ local -> unit -> 'k t

  (** [wait w t key] atomically releases the lock associated with [cw] and blocks on the
      condition variable [t]. To ensure exception safety, it takes hold of the ['k Key.t]
      associated with the lock.

      [wait] returns with the [lock] reacquired once the condition variable [t] has been
      signaled via {!signal} or {!broadcast}. [wait] may also return for no reason -
      callers cannot assume that the property {i P} associated with the condition variable
      [c] holds when [wait] returns.

      @raise Poisoned if [t] cannot be reacquired because it is poisoned.
      @raise Frozen if [t] cannot be reacquired because it is frozen.
      @raise Terminated if [w] is terminated before the lock is reacquired. *)
  val wait
    :  'k Wait.t @ local
    -> 'k t @ local
    -> 'k Capsule.Key.t @ unique
    -> 'k Capsule.Key.t @ unique

  (** [signal t] wakes up one waiter on the condition variable [t], if there is one. If
      there is none, this call has no effect. It is recommended to call [signal t] after a
      critical section - that is, after the lock associated with [t] has been acquired,
      the condition for signaling has been confirmed, and the lock is released. *)
  val signal : 'k t @ local -> unit

  (** [broadcast t] wakes up all waiters on the condition variable [t]. If there are none,
      this call has no effect. It is recommended to call [broadcast t] after a critical
      section, that is, after the lock associated with [t] has been acquired, the
      condition for broadcasting has been confirmed, and the lock released. *)
  val broadcast : 'k t @ local -> unit
end

(** The private interface to condition variables, used by implementations of locks. Not
    exposed outside of this library. *)
module type S_private = sig @@ portable
  include S

  (*_ These two functions might ostensibly be nicer to define in the Wait module, but
      overriding submodules in module types is cumbersome, and it's an internal interface
      so it doesn't matter as much *)

  (** This function must be called with the lock held, and takes the key to prove that *)
  val%template with_wait
    : ('r : value_or_null).
    Await.t @ local
    -> 'k lock @ local
    -> 'k Capsule.Key.t @ unique
    -> ('k Wait.t @ local -> 'k Capsule.Key.t @ unique -> 'r @ l once unique) @ local once
    -> 'r @ l once unique
  [@@mode l = (global, local)]

  val lock_is_held : 'k Wait.t @ local -> bool
end

module type Arg = sig @@ portable
  type 'k t

  val unsafe_acquire : Await.t @ local -> 'k t @ local -> unit
  val unsafe_release : 'k t @ local -> unit
end

module type Condition = sig @@ portable
  module type S = S

  module Make (Lock : Arg) : S_private with type 'k lock := 'k Lock.t
end
