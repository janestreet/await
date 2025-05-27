type 'a t =
  | Canceled
  | Completed of 'a
[@@deriving
  compare ~localize, equal ~localize, globalize, hash, sexp ~localize, sexp_grammar]
