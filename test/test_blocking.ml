open Core
open Await
open Await_sync
open Await_test_helpers

let require_equal expected actual =
  if expected <> actual
  then Stdio.printf "Error: expected=%d actual=%d\n%!" expected actual
;;

let spawn = Multicore.spawn
and setup = Await_blocking.with_await

let%expect_test "Await_blocking basics" =
  (* This tests that we can create parallel threads and read/await [Ivar]s in them using
     the awaiter provided by [with_await]. *)
  let ivar0 : int Ivar.t = Ivar.create () in
  let ivar1 : int Ivar.t = Ivar.create () in
  let ivar2 : int Ivar.t = Ivar.create () in
  let ivar3 : int Ivar.t = Ivar.create () in
  setup Terminator.never ~f:(fun w ->
    Structured.with_scope w () ~f:(fun _w s ->
      Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun w () ->
        (match Ivar.read_or_cancel w Await.Cancellation.never ivar0 with
         | Canceled -> print_endline "Error: Canceled"
         | Completed value -> require_equal 42 value);
        print_endline "signal 1";
        Ivar.fill_exn ivar1 101;
        require_equal 76 (Ivar.read w ivar2));
      Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun w () ->
        require_equal 19 (Ivar.read w ivar3);
        print_endline "signal 0";
        Ivar.fill_exn ivar0 42;
        require_equal 101 (Ivar.read w ivar1));
      Structured.Scope.(add [@mode portable local]) ~spawn ~setup s ~f:(fun _ () ->
        print_endline "start";
        Ivar.fill_exn ivar2 76;
        Ivar.fill_exn ivar3 19))
    [@nontail]);
  [%expect
    {|
    start
    signal 0
    signal 1
    |}]
;;
