@@ portable

open Base

type%template ('a : k) t =
  | Canceled
  | Completed of 'a
[@@kind k = (value_or_null, void, value_or_null & void)]
[@@deriving
  compare ~localize, equal ~localize, globalize, sexp ~stackify, sexp_grammar, hash]

(** [completed_exn t] is [a] if [t] is [Completed a], otherwise it raises *)
val%template completed_exn : ('a : k). ('a t[@kind k]) -> 'a
[@@kind k = (value_or_null, void, value_or_null & void)]

include Monad.S [@mode local] with type 'a t := 'a t
