open Base

type ('a : value_or_null) t =
  | Acquired of 'a
  | Would_block
[@@deriving
  compare ~localize, equal ~localize, globalize, sexp ~stackify, sexp_grammar, hash]
