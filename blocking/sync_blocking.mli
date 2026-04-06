@@ portable

open Await_kernel

(** [sync] is an implementation of sychronizing that blocks the current OS thread.

    It is reexported as [Sync.blocking] by the [Await] library - you should usually use
    that instead of depending on this library directly. *)
val sync : Sync.t
