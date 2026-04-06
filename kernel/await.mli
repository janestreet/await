@@ portable

open Base

(** [Terminated] is an exception indicating that an operation has been terminated. *)
exception Terminated

(** [Await.t] is the type of implementations of awaiting. Operations that need to block
    the current thread for an {i unbounded} amount of time take an [Await.t] which
    provides an implementation of awaiting for them to use.

    Any awaiting operation can be terminated by the awaiting implementation, which results
    in a [Terminated] exception being raised.

    Under the hood, an [Await.t] is just a {!Sync.t} and a {!Terminator.t}. Since
    [Await.t] is intended to be used for blocking for an unbounded period of time, it's
    important to make sure that if the thread that we're waiting on exits or is
    terminated, this thread is also terminated. *)
type t : value mod contended non_float portable

(** [terminator t] is the terminator associated with [t]. Awaiting operations should
    attempt to cancel themselves if they have been terminated, raising [Terminated] if
    they succeed in doing so. *)
val terminator : t @ local -> Terminator.t @ local

(** [sync t] is the implementation of synchronizing associated with [t] *)
val sync : t @ local -> Sync.t @ local

(** [await t ~on_terminate ~await_on] will use [t] to attach [on_terminate] to be
    signalled on termination and to wait until [await_on] has been signaled.

    Note that the [on_terminate] trigger may not be used by [await]. The [await_on]
    trigger will be used before [await] returns. *)
val await : t @ local -> on_terminate:Trigger.Source.t -> on:Trigger.t -> unit

(** [await_until_terminated t trigger] is equivalent to
    [await t ~on_terminate:(Trigger.source trigger) ~await_on:trigger]. *)
val await_until_terminated : t @ local -> Trigger.t -> unit

(** [await_until_terminated_or_canceled t c trigger] is like
    [await_until_terminated t trigger] except it will also return in case the cancellation
    token has been cancelled . *)
val await_until_terminated_or_canceled
  :  t @ local
  -> Cancellation.t @ local
  -> Trigger.t
  -> unit

(** [await_with_terminate t trigger ~terminate r] will attach a trigger to call
    [terminate r] to the {!terminator} of [t] and wait until [trigger] has been signalled. *)
val await_with_terminate
  :  t @ local
  -> Trigger.t
  -> terminate:('r @ contended once portable unique -> unit) @ once portable
  -> 'r @ contended once portable unique
  -> unit

(** [await_with_terminate_or_cancel t c trigger ~terminate_or_cancel r] will attach a
    trigger to call [terminate_or_cancel r] to both the {!terminator} of [t] and to the
    given cancellation token [c] and then wait until [trigger] has been signalled. *)
val await_with_terminate_or_cancel
  :  t @ local
  -> Cancellation.t @ local
  -> Trigger.t
  -> terminate_or_cancel:('r @ contended once portable unique -> unit) @ once portable
  -> 'r @ contended once portable unique
  -> unit

(** [is_terminated t] is [Terminator.is_terminated (terminator t)]. *)
val is_terminated : t @ local -> bool

(** [with_terminator t new_terminator] is an awaiter [u] like [t] where [terminator u] is
    [new_terminator].

    The main use case of [with_terminator] is to protect a blocking operation from being
    terminated by replacing the terminator with {!Terminator.never}:
    {[
      blocking_operation (with_terminator t Terminator.never)
    ]} *)
val with_terminator : t @ local -> Terminator.t @ local -> t @ local

(** [await_never_terminated t trigger] is
    [await_until_terminated (with_terminator t Terminator.never) trigger]. *)
val await_never_terminated : t @ local -> Trigger.t -> unit

(** [yield t] yields to the scheduler using the implementation of yielding associated with
    [t].

    @raise [Terminated] if the terminator associated with [t] has been terminated. *)
val yield : t @ local -> unit

(** [is_canceled t c] is [Cancellation.is_canceled c ~terminator:(terminator t)]. *)
val is_canceled : t @ local -> Cancellation.t @ local -> bool

(** [check_canceled t c] is [Cancellation.check c ~terminator:(terminator t)]. *)
val check_canceled : t @ local -> Cancellation.t @ local -> unit Or_canceled.t

module For_testing : sig
  (** [with_never ~f] calls [f] with an implementation of awaiting that should never be
      used. If [await] is called with the implementation, it will raise.

      This is useful in tests, for testing operations which otherwise might conditionally
      block in a single-threaded manner that never needs to block.

      Bear in mind that proper implementations of [await] do not usually raise and are not
      documented to potentially raise. This means that abstractions built on await may
      e.g. leave the program in an invalid state when using [with_never]. *)
  val with_never
    : ('r : value_or_null).
    f:(t @ local -> 'r @ forkable local once unique) @ local once
    -> 'r @ forkable local once unique
end

(**/**)

module Expert : sig
  (** [create ~sync ~terminator] is an [Await.t] that has [terminator] as its terminator
      and uses [sync] to block the current thread. *)
  val%template create : sync:Sync.t @ l -> terminator:Terminator.t @ l -> t @ l
  [@@alloc a @ l = (stack_local, heap_global)]
end
