# Await

`Await` is a library that provides low-level support for suspending and resuming
concurrent tasks with support for propagation of cancelation and termination.

<!--
```ocaml
open Await
```
-->

As a trivial example, here is a function `increment` that increments a counter protected
by a mutex:

```ocaml
let increment (await: Await.t @ local)
              (mutex: 'k Await.Mutex.t @ local)
              (counter: (int ref, 'k) Capsule.Data.t @ local) =
  Await.Mutex.with_access await mutex ~f:(fun access ->
    let counter = Capsule.Data.(unwrap [@mode local]) ~access counter in
    counter := !counter + 1) [@nontail]
```

In case the mutex is locked, the `with_access` call will suspend the current task using
the `await` capability.
