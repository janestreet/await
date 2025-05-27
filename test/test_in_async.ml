open Base
open Async
open Await
open Await_in_async
open Await_test_helpers
open Expect_test_helpers_base

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
  schedule_with_await Terminator.never ~f:(fun w ->
    let spawn =
      Await_in_async.Expert.thread_safe_spawn
        (Async_kernel_scheduler.current_execution_context ())
    in
    let setup = Await_in_async.Expert.with_await in
    Structured.with_scope w () ~f:(fun _w s ->
      Structured.Scope.(add [@mode portable local])
        ~spawn:Multicore.spawn
        ~setup:Await_blocking.with_await
        s
        ~f:(fun _ _ -> Stdio.print_endline "can do");
      Structured.Scope.add ~spawn ~setup s ~f:(fun w () ->
        require_equal (module Int) 42 (await_deferred w (Ivar.read ivar0));
        Ivar.fill_exn ivar1 101;
        require_equal (module Int) 76 (await_deferred w (Ivar.read ivar2)));
      Structured.Scope.add ~spawn ~setup s ~f:(fun w () ->
        Ivar.fill_exn ivar0 42;
        require_equal (module Int) 101 (await_deferred w (Ivar.read ivar1));
        require_equal (module Int) 76 (await_deferred w (Ivar.read ivar2)));
      Ivar.fill_exn ivar2 76);
    [%expect {| can do |}])
;;
