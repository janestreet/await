@@ portable

open Await_kernel

(** [with_sync ~f] runs [f t] such that [Sync.sync t ~on:trigger] will block the current
    thread by spinning until the [trigger] is signalled, and [Sync.yield t] does nothing.

    This is a simple spinning implementation of [Sync.t] suitable for testing and
    benchmarking purposes. This should not be used outside of said use cases. *)
val with_sync : f:(Sync.t @ local -> 'a @ unique) @ local once -> 'a

(** [with_await terminator ~f] runs [f t] such that
    [Await.sync t ~on_terminate ~on:trigger] will block the current thread by spinning
    until the [trigger] is signalled, and [Await.yield t] does nothing.

    This is a simple spinning implementation of [Await.t] suitable for testing and
    benchmarking purposes. This should not be used outside of said use cases. *)
val with_await : Terminator.t @ local -> f:(Await.t @ local -> 'a) @ local once -> 'a
