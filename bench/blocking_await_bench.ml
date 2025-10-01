open Base
open Portable
open Await

let relax_or_yield =
  if Domain.recommended_domain_count () > 1 then Domain.cpu_relax else Thread.yield
;;

let num_iters = 50_000

(* This tries to measure the overhead of setting up a blocking await when the thread is
   actually never suspended.  This is a fairly isolating benchmark and should give stable
   results. *)
let%bench_fun "blocking with_await (overhead)" =
  let trigger = Trigger.create () in
  Trigger.Source.signal (Trigger.source trigger);
  fun () ->
    for _ = 1 to num_iters do
      Concurrent_in_thread.with_concurrent Terminator.never ~f:(fun c ->
        Await.await_until_terminated (Concurrent.await c) trigger [@nontail])
    done
;;

(* This tries to measure the cost of await with blocking await both in cases when the
   thread is not suspended and when it is suspended.  The results from this benchmark are
   likely to be unstable as the cost varies significantly depending on whether the thread
   was suspended or not. *)
let%bench "blocking await (thruput)" =
  let%with.tilde.stack c = Concurrent_in_thread.with_concurrent Terminator.never in
  Concurrent.with_scope c () ~f:(fun s ->
    let stop = Atomic.make_alone false in
    let source = Atomic.make_alone (Trigger.source (Trigger.create ())) in
    Concurrent.spawn s ~f:(fun _ _ _ ->
      while not (Atomic.get stop) do
        Trigger.Source.signal (Atomic.get source);
        relax_or_yield ()
      done);
    for _ = 1 to num_iters do
      let trigger = Trigger.create () in
      Atomic.set source (Trigger.source trigger);
      Await.await_until_terminated (Concurrent.await c) trigger
    done;
    Atomic.set stop true)
  [@nontail]
;;

(* This combines the work of both of the above by setting up a new blocking await before
   potentially suspending the thread.  Interestingly this tends to be faster than the
   previous benchmark, because this is more likely to not result in actually suspending
   the thread as the blocking implementation allocates the blocking implementation lazily
   and that gives time to the background thread to signal the trigger and prevent the
   thread from being suspended. *)
let%bench "blocking with_await and await" =
  let%with.tilde.stack c = Concurrent_in_thread.with_concurrent Terminator.never in
  Concurrent.with_scope c () ~f:(fun s ->
    let stop = Atomic.make_alone false in
    let source = Atomic.make_alone (Trigger.source (Trigger.create ())) in
    Concurrent.spawn s ~f:(fun _ _ _ ->
      while not (Atomic.get stop) do
        Trigger.Source.signal (Atomic.get source);
        relax_or_yield ()
      done);
    for _ = 1 to num_iters do
      Concurrent_in_thread.with_concurrent Terminator.never ~f:(fun c ->
        let trigger = Trigger.create () in
        Atomic.set source (Trigger.source trigger);
        Await.await_until_terminated (Concurrent.await c) trigger [@nontail])
    done;
    Atomic.set stop true)
;;
