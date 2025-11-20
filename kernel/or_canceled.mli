@@ portable

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
    , value_or_null )]
[@@deriving
  compare ~localize, equal ~localize, globalize, sexp ~stackify, sexp_grammar, hash]

(** [completed_exn t] is [a] if [t] is [Completed a], otherwise it raises *)
val%template completed_exn : ('a : k). ('a t[@kind k]) -> 'a
[@@kind
  k
  = ( value_or_null
    , void
    , value_or_null & void
    , value_or_null & value_or_null
    , (value_or_null & value_or_null) & value_or_null )]

include Monad.S [@kind value_or_null] [@mode local] with type 'a t := 'a t

(** [never_completed t] can be used for an [Or_canceled.t] computation which either loops
    forever or is canceled. *)
val never_completed : Nothing.t t @ local -> unit
