open! Base
open Await_kernel

module type S = sig @@ portable
  (** A deferred computation. *)

  open! Base
  open Await_kernel

  type capability : value mod contended portable

  (** A value of type ['a Await_sync.Lazy.t] is a deferred computation that has a result
      of type ['a].

      It is comparable to ['a Lazy.t], except that it can be shared between threads. It is
      comparable to ['a Base.Portable_lazy.t], except that forcing it takes a
      {!capability} which provides an implementation of blocking to use if another thread
      is already forcing the lazy value.

      You should prefer ['a Await_sync.Lazy.t] to ['a Base.Portable_lazy.t] in any case
      where you have such a capability (such as [Await.t]) available, and/or for
      long-running computations which are likely to be forced by many threads
      simultaneously (for which you can use {!Await_blocking} to obtain an [Await.t]). *)
  type ('a : value_or_null) t : value mod contended non_float portable

  (** [from_val v] returns an already-forced suspension of [v].

      The optional [padded] argument specifies whether to pad the data structure to avoid
      false sharing. See {!Atomic.make} for a longer explanation. *)
  val from_val : ('a : value_or_null). ?padded:bool -> 'a @ contended portable -> 'a t

  (** [from_fun f] returns a suspension of the function [f]. Note that [f] must be
      [portable], as it may be run on any thread.

      The optional [padded] argument specifies whether to pad the data structure to avoid
      false sharing. See {!Atomic.make} for a longer explanation. *)
  val from_fun
    : ('a : value_or_null).
    ?padded:bool
    -> (capability @ local -> 'a @ contended portable) @ once portable
    -> 'a t

  (** [from_fun_fixed f] returns a suspension of the {i fix-point} function [f], which
      takes the lazy value itself in addition to the [capability] that is passed to the
      call to [force] as arguments. This can be used in places where one might normally
      use [let rec] to make a [lazy_t] that refers to itself.

      The optional [padded] argument specifies whether to pad the data structure to avoid
      false sharing. See {!Atomic.make} for a longer explanation. *)
  val from_fun_fixed
    : ('a : value_or_null).
    ?padded:bool
    -> (capability @ local -> 'a t -> 'a @ contended portable) @ once portable
    -> 'a t

  (** [force w t] forces the suspension [t] and returns its result. If [t] has already
      been forced, [force t] returns the same value again without recomputing it. If
      multiple threads force the same lazy simultaneously, only one will execute the
      computation, and the rest will block using [w] until the computation has finished
      executing.

      If the suspension raises an exception, that exception will be converted to a string
      (to guarantee that it's safe to share between threads) and [force] will raise an
      exception. The raised exception is intentionally opaque and cannot be matched on in
      order to preserve forward compatibility; if you want the suspension to possibly
      return an exception, use [Result.t] or another similar type.

      Note that unlike the [lazy] in the standard library, [Await_sync.Lazy] does {i not}
      raise an error if a lazy calls [force] from within its own suspension - instead,
      this will cause a deadlock.

      @raise Terminated in case [w] was terminated. *)
  val force : ('a : value_or_null). capability @ local -> 'a t -> 'a @ contended portable

  (** [force_or_cancel w c t] is [Completed (force w t)] if [c] was not canceled,
      otherwise it is [Canceled].

      @raise Terminated in case [w] was terminated, even if [c] was canceled. *)
  val force_or_cancel
    : ('a : value_or_null).
    capability @ local
    -> Cancellation.t @ local
    -> 'a t
    -> 'a Or_canceled.t @ contended portable

  (** [is_val t] is [true] if [t] has already been forced and did not raise an exception,
      or [false] otherwise. *)
  val is_val : ('a : value_or_null). 'a t -> bool

  (** [peek t] is [This v] if [t] has already been forced to the value [v] and did not
      raise an exception, or [Null] otherwise. *)
  val peek : ('a : value). 'a t -> 'a Or_null.t @ contended portable

  (** [peek_opt t] is like [peek], except it returns an [option] instead of [or_null], for
      use with lazy values with layout [value_or_null]. *)
  val peek_opt : ('a : value_or_null). 'a t -> 'a option @ contended portable
end

module type Lazy = sig @@ portable
  module Sync : S with type capability := Sync.t
  module Await : S with type capability := Await.t

  module type S = S

  module Expert : sig
    module Make (C : Lock_common.Capability) : S with type capability := C.t
  end
end
