open Base
open Basement
open Await
open Await_sync
open Await_test_helpers

let spawn = Multicore.spawn
and setup = Await_blocking.with_await

external is_runtime5 : unit -> bool = "%runtime5"

let cpu_relax = if is_runtime5 () then Domain.cpu_relax else Thread.yield

module type Mutex = sig @@ portable
  type 'k t : value mod contended portable

  val create : 'k Capsule.Key.t @ unique -> 'k t

  val with_key
    :  Await.t @ local
    -> 'k t
    -> f:('k Capsule.Key.t @ unique -> 'a * 'k Capsule.Key.t @ once unique) @ local once
    -> 'a @ once unique
end

module Bench_mutex (Mutex : Mutex) = struct
  let num_iters = 100_000

  let lock_unlock_spin_wait_loop ~num_domains =
    let (Capsule.Key.P key) = Capsule.create () in
    let mutex = Mutex.create key in
    fun () ->
      setup Terminator.never ~f:(fun w ->
        Structured.with_scope w () ~f:(fun _w s ->
          let loop w () =
            for _ = 1 to num_iters do
              Mutex.with_key w mutex ~f:(fun key -> cpu_relax (), key)
            done
          in
          for _ = 1 to num_domains do
            Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:loop
          done))
      [@nontail]
  ;;

  let%bench_fun "uncontended" = lock_unlock_spin_wait_loop ~num_domains:1
  let%bench_fun "contended" = lock_unlock_spin_wait_loop ~num_domains:2
  let%bench_fun "very contended" = lock_unlock_spin_wait_loop ~num_domains:10
end

module%bench Mutex_in_thread = Bench_mutex (Mutex)
