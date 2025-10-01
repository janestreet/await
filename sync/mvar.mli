@@ portable

open Await_kernel

(** An ['a Mvar.t] is a mutable cell that is either empty or contains a single value of
    type ['a]. One can {!put} new values, and wait to read the value with {!take}.

    Mvars are entirely linear - every {!put} value is {!take}n exactly once. *)
type 'a t : value mod contended portable

(** [create ()] returns an empty mvar *)
val create : unit -> 'a t

(** [create_full v] returns an mvar filled with [v] *)
val create_full : 'a @ contended once portable unique -> 'a t

(** [put w t a] waits using [w] until the mvar [t] is empty, and then sets the value to
    [a]. If there are multiple concurrent [put]s, there is no fairness guarantee (ie,
    [put]s may happen out of order or may be starved). *)
val put : Await.t @ local -> 'a t @ local -> 'a @ contended once portable unique -> unit

(** [put_or_cancel w c t a] is [Completed (put w t a)] if [c] is not canceled, otherwise
    it is [Canceled] *)
val put_or_cancel
  :  Await.t @ local
  -> Cancellation.t @ local
  -> 'a t @ local
  -> 'a @ contended once portable unique
  -> unit Or_canceled.t

module Ok_or_already_full : sig
  type t =
    | Ok
    | Already_full
  [@@deriving sexp_of]
end

(** [try_put t a] sets the value of [t] to [a] and returns [Ok] if it is empty, otherwise
    it returns [Already_full]. *)
val try_put : 'a t @ local -> 'a @ contended once portable unique -> Ok_or_already_full.t

(** [put_exn t a] sets the value of [t] to [a] and returns [Ok] if it is empty.

    @raise Already_full in case [t] was already full. *)
val put_exn : 'a t @ local -> 'a @ contended once portable unique -> unit

(** [take w t] waits using [w] until the mvar [t] is full, then clears it and returns its
    value. If there are multiple concurrent calls to [take] then only one will be
    fulfilled and the others will continue waiting on future values. There is no ordering
    guarantee for which [take] call will be filled first. *)
val take : Await.t @ local -> 'a t @ local -> 'a @ contended once portable unique

(** [take_or_cancel w c t a] is [Completed (take w t a)] if [c] is not canceled, otherwise
    it is [Canceled] *)
val take_or_cancel
  :  Await.t @ local
  -> Cancellation.t @ local
  -> 'a t @ local
  -> 'a Or_canceled.t @ contended once portable unique

(** [try_take t] empties the mvar [t] and returns [This v] if it is currently full, or
    returns [Null] otherwise. *)
val try_take : 'a t @ local -> 'a or_null @ contended once portable unique

(** [take_exn] empties the mvar [t] and returns its value [v] if it is currently full.

    @raise Empty in case [t] was empty. *)
val take_exn : 'a t @ local -> 'a @ contended once portable unique

(** [is_full t] is [true] if the mvar [t] is currently full. *)
val is_full : _ t @ local -> bool
