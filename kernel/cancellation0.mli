@@ portable

(** Core cancellation token implementation.

    This module provides the fundamental cancellation token functionality that both
    {!Cancellation} and {!Terminator} build upon. Should not be used directly. *)

type t : value mod contended non_float portable unyielding

val is_canceled : t @ local -> bool
val same : t @ local -> t @ local -> bool
val never : t
val always : t

module Source : sig
  type t : value mod contended non_float portable

  val cancel : t @ local -> unit
end

val is_cancellable : t @ local -> bool
val source : t @ local -> Source.t or_null

val with_
  : ('a : value_or_null).
  (t @ local -> 'a @ once unique) @ local once -> 'a @ once unique

val with_linked
  : ('a : value_or_null).
  t @ local -> (t @ local -> 'a @ once unique) @ local once -> 'a @ once unique

val with_linked_multi
  : ('a : value_or_null).
  t list @ local -> (t @ local -> 'a @ once unique) @ local once -> 'a @ once unique

module Link : sig
  type t =
    | Attached
    | Canceled
    | Signaled
  [@@deriving equal ~localize, sexp ~stackify]
end

val add_trigger : t @ local -> Trigger.Source.t -> Link.t

module Expert : sig
  val check_clean_and_close : t @ local -> unit
  val globalize : t @ local -> t
  val create : unit -> t
end

module For_testing : sig
  val get_countdown : t @ local -> int
end
