@@ portable

(** [Terminated] is an exception indicating that an operation has been terminated. *)
exception Terminated

(** [t] is the type of implementations of awaiting. Operations that need to await
    something take a [t] that provides an implementation of awaiting for them to use.

    Any awaiting operation can be terminated by the awaiting implementation, which results
    in a [Terminated] exception being raised. *)
type t : value mod contended

(** [create r ~await c] returns an awaiter that has [r] as its terminator, and [await c]
    as its implementation of [await]. *)
val create
  : ('c : value mod contended).
  Terminator.t @ local
  -> await:('c @ local -> Trigger.t -> unit) @ local
  -> 'c @ local
  -> t @ local

(** [terminator t] is the terminator associated with [t]. Awaiting operations should
    attempt to cancel themselves if they have been terminated, raising [Terminated] if
    they succeed in doing so. *)
val terminator : t @ local -> Terminator.t @ local

(** [await t ~on_terminate ~await_on] will use [t] to attach [on_terminate] to be
    signalled on termination and to wait until [await_on] has been signaled.

    Note that the [on_terminate] trigger may not be used by [await]. The [await_on]
    trigger will be used before [await] returns. *)
val await : t @ local -> on_terminate:Trigger.Source.t -> await_on:Trigger.t -> unit

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

(** [await_with_terminate t trigger ~terminate] will attach a trigger to call [terminate]
    to the [terminator] of [t] and wait until [trigger] has been signalled. *)
val await_with_terminate
  :  t @ local
  -> Trigger.t
  -> terminate:(unit -> unit) @ once portable
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
