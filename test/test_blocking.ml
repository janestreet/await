open Await
open Await_test_helpers

let spawn = Multicore.spawn
and setup = Await_blocking.with_await

let%expect_test "Await_blocking basics" =
  (* This tests that we can create systhreads and [await_until_terminated] in them using
     the awaiter provided by [run_with_await]. *)
  let trigger0 = Trigger.create () in
  let trigger1 = Trigger.create () in
  let trigger2 = Trigger.create () in
  let trigger3 = Trigger.create () in
  setup Terminator.never ~f:(fun w ->
    Structured.with_scope w () ~f:(fun _w s ->
      Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun w () ->
        Await.await_until_terminated w trigger0;
        print_endline "signal 1";
        Trigger.Source.signal (Trigger.source trigger1);
        Await.await_until_terminated w trigger2);
      Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun w () ->
        Await.await_until_terminated w trigger3;
        print_endline "signal 0";
        Trigger.Source.signal (Trigger.source trigger0);
        Await.await_until_terminated w trigger1);
      Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun _ () ->
        print_endline "start";
        Trigger.Source.signal (Trigger.source trigger2);
        Trigger.Source.signal (Trigger.source trigger3)))
    [@nontail]);
  [%expect
    {|
    start
    signal 0
    signal 1
    |}]
;;
