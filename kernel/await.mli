@@ portable

open Base

(** Abstract implementation of {i unbounded} awaiting.

    Operations that need to block the current thread for an {i unbounded} amount of time
    take an {{!t} [Await.t]} which provides an implementation of awaiting for them to use.

    In practice, code which needs an {{!t} [Await.t]} should get one out of an
    implementation of concurrency by calling [Concurrent.await].

    Any awaiting operation can be terminated by the awaiting implementation, which results
    in a {!Terminated} exception being raised.

    Under the hood, an {{!t} [Await.t]} is just a {!Sync.t} and a {!Terminator.t}. Since
    {{!t} [Await.t]} is intended to be used for blocking for an unbounded period of time,
    it's important to make sure that if the thread that we're waiting on exits or is
    terminated, this thread is also terminated. *)

(** Represents the capability to block for an {i unbounded} amount of time. *)
type t : value mod contended non_float portable

(** @inline *)
include module type of struct
  include Await_kernel_intf (** @inline *)
end

(** [terminator t] is the terminator associated with [t]. Awaiting operations should
    attempt to cancel themselves if they have been terminated, raising {!Terminated} if
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
    [terminate r] to the {!terminator} of [t] and wait until [trigger] has been signalled.

    The caller must always check for the reason that this call returned and act
    accordingly. In particular, it is possible that both the operation being awaited upon
    has completed successfully and that the terminator has been terminated. *)
val await_with_terminate
  :  t @ local
  -> Trigger.t
  -> terminate:('r @ contended once portable unique -> unit) @ global once portable
  -> 'r @ contended once portable unique
  -> unit

(** [await_with_terminate_or_cancel t c trigger ~terminate_or_cancel r] will attach a
    trigger to call [terminate_or_cancel r] to both the {!terminator} of [t] and to the
    given cancellation token [c] and then wait until [trigger] has been signalled.

    The caller must always check for the reason that this call returned and act
    accordingly. In particular, it is possible that both the operation being awaited upon
    has completed successfully and that the terminator has been terminated or the
    cancellation token has been canceled or both. *)
val await_with_terminate_or_cancel
  :  t @ local
  -> Cancellation.t @ local
  -> Trigger.t
  -> terminate_or_cancel:('r @ contended once portable unique -> unit)
     @ global once portable
  -> 'r @ contended once portable unique
  -> unit

(** [is_terminated t] is [Terminator.is_terminated (terminator t)]. *)
val is_terminated : t @ local -> bool

(** [with_terminator t new_terminator] is an awaiter [u] like [t] where [terminator u] is
    [new_terminator].

    The main use case of [with_terminator] is to protect a blocking operation from being
    terminated by replacing the terminator with {!Terminator.unkillable}:
    {[
      blocking_operation (with_terminator t Terminator.unkillable)
    ]} *)
val with_terminator : t @ local -> Terminator.t @ local -> t @ local

(** [await_never_terminated t trigger] is
    [await_until_terminated (with_terminator t Terminator.unkillable) trigger]. *)
val await_never_terminated : t @ local -> Trigger.t -> unit

(** [check_terminated t] checks whether the terminator of [t] has been terminated.

    @raise Terminated if the terminator associated with [t] has been terminated. *)
val check_terminated : t @ local -> unit

(** [yield t] yields to the scheduler using the implementation of yielding associated with
    [t].

    @raise Terminated if the terminator associated with [t] has been terminated. *)
val yield : t @ local -> unit

(** [is_canceled t c] is [Cancellation.is_canceled c ~terminator:(terminator t)]. *)
val is_canceled : t @ local -> Cancellation.t @ local -> bool

(** [check_canceled t c] is [Cancellation.check c ~terminator:(terminator t)]. *)
val check_canceled : t @ local -> Cancellation.t @ local -> unit Or_canceled.t

module For_testing : sig
  (** [never] is an implementation of awaiting that should never be used. If [await] is
      called with the implementation, it will raise.

      This is useful in tests, for testing operations which otherwise might conditionally
      block in a single-threaded manner that never needs to block - or, to test that a
      blocking operation always blocks, eg using
      [Expect_test_helpers_base.require_does_raise]

      Bear in mind that proper implementations of [await] do not usually raise and are not
      documented to potentially raise. This means that abstractions built on await may
      e.g. leave the program in an invalid state when using [never]. *)
  val never : t
end

(**/**)

module Expert : sig
  (** [create ~sync ~terminator] is an [Await.t] that has [terminator] as its terminator
      and uses [sync] to block the current thread. *)
  val%template create : sync:Sync.t @ l -> terminator:Terminator.t @ l -> t @ l
  [@@alloc a @ l = (stack_local, heap_global)]

  (** [with_sync t sync] is an [Await.t] with the same terminator as [t] that uses [sync]
      to suspend. *)
  val%template with_sync : t @ l -> Sync.t @ l -> t @ l
  [@@alloc a @ l = (stack_local, heap_global)]
end
