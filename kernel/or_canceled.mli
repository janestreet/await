@@ portable

(** Result of a cancellable operation that is either {{!Completed} [Completed _]} or
    {!Canceled}. *)

open Base

type%template ('a : k) t =
  | Canceled
  | Completed of 'a
[@@kind
  k
  = ( void
    , value_or_null & void
    , value_or_null & value_or_null
    , (value_or_null & value_or_null) & value_or_null
    , word
    , word & value_or_null )]

(** The type for the result of a cancellable operation. *)
type ('a : value_or_null) t =
  | Canceled
  | Completed of 'a

[%%rederive:
  type nonrec ('a : value_or_null) t = 'a t
  [@@deriving
    compare ~localize, equal ~localize, globalize, sexp ~stackify, sexp_grammar, hash]]

(** [completed_exn t] is [a] if [t] is [Completed a], otherwise it raises *)
val%template completed_exn : ('a : k). ('a t[@kind k]) -> 'a
[@@kind
  k
  = ( value_or_null
    , void
    , value_or_null & void
    , value_or_null & value_or_null
    , (value_or_null & value_or_null) & value_or_null
    , word
    , word & value_or_null )]

(** [never_completed t] can be used for an [Or_canceled.t] computation which either loops
    forever or is canceled. *)
val never_completed : Nothing.t t @ local -> unit

(** {1 Monad interface} *)

include Monad.S [@kind value_or_null] [@mode local] with type 'a t := 'a t

(** {1 Exception based interface for experts} *)

module Exn : sig
  (** Provides a way to implement cancellable and non-cancellable variants of an operation
      through a single exception raising implementation.

      This is not meant for casual use. This is rather meant for internal implementation
      in cases where the burden of providing both cancellable and non-cancellable variants
      of operations would otherwise be high. The [Canceled] exception should not be
      allowed to propagate outside of such internal uses. *)

  (** Exception to be raised and shortly handled in case of cancellation. *)
  exception Canceled

  (** [catch fn] calls [Completed (fn ())] and handles the [Canceled] exception by
      returning the [Canceled] value. *)
  val%template catch
    : ('a : k).
    (unit# -> 'a @ l p u) @ local once -> ('a t[@kind k]) @ l p u
  [@@kind
    k
    = ( value_or_null
      , void
      , value_or_null & void
      , value_or_null & value_or_null
      , (value_or_null & value_or_null) & value_or_null
      , word
      , word & value_or_null )]
  [@@mode u = (aliased, unique), l = (global, local), p = (nonportable, portable)]
end
