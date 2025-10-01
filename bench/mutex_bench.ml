open Base
open Basement
open Await
module Capsule = Capsule.Expert

let cpu_relax = Stdlib_shim.Domain.cpu_relax

module type Mutex = sig @@ portable
  type 'k t : value mod contended portable

  val create : 'k Capsule.Key.t @ unique -> 'k t

  val with_key
    :  Await.t @ local
    -> 'k t @ local
    -> f:('k Capsule.Key.t @ unique -> #('a * 'k Capsule.Key.t) @ once unique)
       @ local once
    -> 'a @ once unique
end

module Bench_mutex (Mutex : Mutex) = struct
  let num_iters = 100_000

  let lock_unlock_spin_wait_loop ~num_domains =
    let (Capsule.Key.P key) = Capsule.create () in
    let mutex = Mutex.create key in
    fun () ->
      let%with.tilde.stack c = Concurrent_in_thread.with_concurrent Terminator.never in
      Concurrent.with_scope c () ~f:(fun s ->
        let loop _ _ c =
          for _ = 1 to num_iters do
            Mutex.with_key (Concurrent.await c) mutex ~f:(fun key -> #(cpu_relax (), key))
          done
        in
        for _ = 1 to num_domains do
          Concurrent.spawn s ~f:loop
        done)
  ;;

  let%bench_fun "uncontended" = lock_unlock_spin_wait_loop ~num_domains:1
  let%bench_fun "contended" = lock_unlock_spin_wait_loop ~num_domains:2
  let%bench_fun "very contended" = lock_unlock_spin_wait_loop ~num_domains:10
end

module%bench Stdlib_mutex = Bench_mutex (struct
    type 'k t = Stdlib.Mutex.t

    let create _key = Stdlib.Mutex.create ()

    let with_key _await t ~f =
      Stdlib.Mutex.lock t;
      match f (Capsule.Key.unsafe_mk ()) with
      | #(res, _key) ->
        Stdlib.Mutex.unlock t;
        res
      | exception exn ->
        Stdlib.Mutex.unlock t;
        raise exn
    ;;
  end)

module%bench Mutex_in_thread = Bench_mutex (Mutex)
module%bench Rwlock_in_thread = Bench_mutex (Rwlock)
