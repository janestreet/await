@@ portable

open Await_kernel

(** [sync] is an implementation of sychronizing that blocks the current OS thread

    by spinning until the [trigger] is signalled. [Sync.yield t] does nothing.

    It is reexported as [Sync.spinning] by the [Await] library - you should usually use
    that instead of depending on this library directly. *)
val sync : Sync.t
