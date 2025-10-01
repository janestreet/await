open Core
open Async
open Await
open Await_in_async
open Expect_test_helpers_base
module Ivar = Async.Ivar

[@@@alert "-experimental"]

let%expect_test ("Await_in_async.schedule_with_await" [@tags "runtime5-only"]) =
  let ivar0 = Ivar.create () in
  let ivar1 = Ivar.create () in
  let ivar2 = Ivar.create () in
  let t1 =
    schedule_with_await Terminator.never ~f:(fun w ->
      require_equal (module Int) 42 (await_deferred w (Ivar.read ivar0));
      Ivar.fill_exn ivar1 101;
      await_deferred w (Ivar.read ivar2))
  in
  let t2 =
    schedule_with_await Terminator.never ~f:(fun w ->
      Ivar.fill_exn ivar0 42;
      require_equal (module Int) 101 (await_deferred w (Ivar.read ivar1));
      await_deferred w (Ivar.read ivar2))
  in
  Ivar.fill_exn ivar2 76;
  let%bind x = t1 in
  require_equal (module Int) 76 x;
  let%bind y = t2 in
  require_equal (module Int) 76 y;
  [%expect {| |}];
  return ()
;;

let%expect_test ("Await_in_async.Expert" [@tags "runtime5-only"]) =
  let ivar0 = Ivar.create () in
  let ivar1 = Ivar.create () in
  let ivar2 = Ivar.create () in
  Concurrent_in_async.schedule_with_concurrent Terminator.never ~f:(fun c ->
    Concurrent.with_scope c () ~f:(fun s ->
      let thread_spawn = Concurrent_in_thread.spawn_into s in
      Concurrent.spawn thread_spawn ~f:(fun _ _ _ -> print_endline "can do");
      Concurrent.spawn_onto_initial s ~f:(fun _ _ c ->
        require_equal
          (module Int)
          42
          (await_deferred (Concurrent.await c) (Ivar.read ivar0));
        Ivar.fill_exn ivar1 101;
        require_equal
          (module Int)
          76
          (await_deferred (Concurrent.await c) (Ivar.read ivar2)));
      Concurrent.spawn_onto_initial s ~f:(fun _ _ c ->
        Ivar.fill_exn ivar0 42;
        require_equal
          (module Int)
          101
          (await_deferred (Concurrent.await c) (Ivar.read ivar1));
        require_equal
          (module Int)
          76
          (await_deferred (Concurrent.await c) (Ivar.read ivar2)));
      Ivar.fill_exn ivar2 76);
    [%expect {| can do |}])
;;

let%expect_test ("yield" [@tags "runtime5-only"]) =
  let barrier = Barrier.create 2 in
  let%bind () =
    Concurrent_in_async.schedule_with_concurrent Terminator.never ~f:(fun c ->
      let stop = Atomic.make false in
      Concurrent.with_scope c () ~f:(fun s ->
        Concurrent.spawn s ~f:(fun _ _ c ->
          (* A busy loop that yields at the top of every iteration *)
          Barrier.await (Concurrent.await c) barrier;
          while not (Atomic.get stop) do
            Await.yield (Concurrent.await c)
          done);
        (* And another task, that shouldn't be blocked by the first (since the first
           yields to the async scheduler every iteration) *)
        Concurrent.spawn_onto_initial s ~f:(fun _ _ c ->
          Barrier.await (Concurrent.await c) barrier;
          print_endline "Hello from another task!";
          await_deferred (Concurrent.await c) (Clock_ns.after (Time_ns.Span.of_int_ms 50));
          print_endline "Hello again, from another task!";
          Atomic.set stop true)))
  in
  [%expect
    {|
    Hello from another task!
    Hello again, from another task!
    |}];
  return ()
;;
