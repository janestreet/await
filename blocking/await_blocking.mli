@@ portable

open Await_kernel

(** [with_await terminator ~f] runs [f t] such that
    [Await.await t ~on_terminate ~await_on] will block the thread until the trigger to
    [await_on] is signalled. *)
val with_await
  : ('a : value_or_null).
  Terminator.t @ local -> f:(Await.t @ local -> 'a) @ local once -> 'a
