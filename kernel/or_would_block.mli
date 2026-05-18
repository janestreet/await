@@ portable

(** Result of a try-lock operation that is either {{!Acquired} [Acquired _]} or
    {!Would_block}. *)

open Base

(** The type for the result of a try-lock operation. *)
type ('a : value_or_null) t =
  | Acquired of 'a
  | Would_block
[@@deriving
  compare ~localize, equal ~localize, globalize, sexp ~stackify, sexp_grammar, hash]
